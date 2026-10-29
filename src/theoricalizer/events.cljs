(ns theoricalizer.events
  (:require [re-frame.core :as rf]
            [theoricalizer.theory :as t]
            [theoricalizer.exercise :as ex]
            [theoricalizer.audio :as audio]
            [theoricalizer.storage :as storage]
            [theoricalizer.i18n :as i18n]))

(def default-settings
  {:lang (i18n/browser-lang)
   :note-names (if (= :el (i18n/browser-lang)) :solfege :letters)
   :kind :interval-name
   :clef :any
   :max-acc 4
   :aug-dim true
   :compound false
   :forms :harmonic
   :with-sig false
   :inversions true})

(def exercise-settings #{:kind :clef :max-acc :aug-dim :compound :forms :with-sig :inversions})

(def initial-db
  {:view :practice
   :settings default-settings
   :stats {}
   :width 800})

(rf/reg-fx :audio/play (fn [events] (audio/stop-all!) (audio/play-events! events)))
(rf/reg-fx :save (fn [kvs] (doseq [[k v] kvs] (storage/save! k v))))
(rf/reg-fx :html-lang (fn [lang] (set! (.. js/document -documentElement -lang) (name lang))))

(defn writing? [kind] (contains? #{:interval-write :scale-write :triad-write} kind))
(defn keys-kind? [kind] (contains? #{:interval-keys :triad-keys} kind))
(defn chord-cols? [kind] (= kind :triad-write))

(defn- empty-answer [e]
  (cond (writing? (:kind e)) (:cols e)
        (keys-kind? (:kind e)) #{}
        :else {}))

(defn- new-exercise [db]
  (let [e (ex/generate rand (:settings db))]
    (assoc db :ex e :answer (empty-answer e) :result nil
           :sel (when (writing? (:kind e)) [(first (remove (or (:given e) #{}) (range (count (:cols e))))) nil]))))

(rf/reg-event-db :init (fn [_ _] (new-exercise initial-db)))

(rf/reg-event-fx :storage/loaded
  (fn [{:keys [db]} [_ {:keys [settings stats]}]]
    (let [s (merge default-settings (select-keys settings (keys default-settings)))]
      {:db (new-exercise (assoc db :settings s :stats (or stats {})))
       :html-lang (:lang s)})))

(rf/reg-event-db :audio/loaded (fn [db [_ n]] (assoc db :samples n)))
(rf/reg-event-db :resize (fn [db [_ w]] (assoc db :width w)))
(rf/reg-event-db :set-view (fn [db [_ v]] (assoc db :view v)))

(rf/reg-event-fx :settings/set
  (fn [{:keys [db]} [_ k v]]
    (let [db (cond-> (assoc-in db [:settings k] v)
               (exercise-settings k) new-exercise)]
      (cond-> {:db db :save {:settings (:settings db)}}
        (= k :lang) (assoc :html-lang v)))))

(rf/reg-event-db :new (fn [db _] (new-exercise db)))

;; ---------------------------------------------------------------- answering

(defn- open? [db] (and (:ex db) (nil? (:result db))))

(rf/reg-event-db :answer/set
  (fn [db [_ k v]] (if (open? db) (assoc-in db [:answer k] v) db)))

(rf/reg-event-db :answer/toggle-key
  (fn [db [_ key]]
    (if (open? db)
      (update db :answer #(if (contains? % key) (disj % key) (conj % key)))
      db)))

(defn- midi-events [notes] (map (fn [n] [(t/midi n) 0 1.2]) notes))

(defn- default-alt [db l] (t/sig-alt (get-in db [:ex :keysig]) l))

(rf/reg-event-fx :pick
  (fn [{:keys [db]} [_ c step]]
    (when (and (open? db) (writing? (get-in db [:ex :kind])))
      (let [notes (get-in db [:answer c])
            chord? (chord-cols? (get-in db [:ex :kind]))
            [sc ss] (:sel db)
            existing (some #(when (= step (:s %)) %) notes)
            n {:s step :a (default-alt db (mod step 7))}
            notes' (cond existing notes
                         (not chord?) [n]
                         (< (count notes) 3) (conj notes n)
                         (= sc c) (mapv #(if (= ss (:s %)) n %) notes)
                         :else notes)]
        (cond-> {:db (-> db (assoc-in [:answer c] (vec (sort-by :s notes'))) (assoc :sel [c step]))}
          (not existing) (assoc :audio/play (midi-events [n])))))))

(defn- update-sel
  "Applies `f` to the selected note; `f` returns the new note or nil to delete it."
  [db f]
  (let [[c s] (:sel db)]
    (if (and (open? db) c s)
      (let [notes (get-in db [:answer c])
            n (some #(when (= s (:s %)) %) notes)
            n' (when n (f n))
            notes' (vec (sort-by :s (cond->> (remove #{n} notes) n' (cons n'))))]
        (if n
          (-> db (assoc-in [:answer c] notes') (assoc :sel [c (:s n')]))
          db))
      db)))

(rf/reg-event-fx :alter
  (fn [{:keys [db]} [_ a]]
    (let [db (update-sel db #(assoc % :a a))
          [c s] (:sel db)]
      {:db db :audio/play (midi-events (filter #(= s (:s %)) (get-in db [:answer c])))})))

(rf/reg-event-fx :move
  (fn [{:keys [db]} [_ d]]
    (let [[c s] (:sel db)
          taken? (some #(= (+ s d) (:s %)) (get-in db [:answer c]))
          db (if (or (nil? s) taken?) db (update-sel db #(let [s' (+ (:s %) d)] {:s s' :a (default-alt db (mod s' 7))})))
          [c s] (:sel db)]
      {:db db :audio/play (midi-events (filter #(= s (:s %)) (get-in db [:answer c])))})))

(rf/reg-event-db :delete (fn [db _] (update-sel db (constantly nil))))

(rf/reg-event-db :select-col
  (fn [db [_ d]]
    (let [{:keys [cols given]} (:ex db)
          editable (remove (or given #{}) (range (count cols)))
          [c] (:sel db)
          c' (or (first (filter (if (pos? d) #(> % c) #(< % c)) (if (pos? d) editable (reverse editable)))) c)]
      (if (and (open? db) c)
        (assoc db :sel [c' (:s (first (get-in db [:answer c'])))])
        db))))

(rf/reg-event-fx :check
  (fn [{:keys [db]} _]
    (let [{:keys [ex answer]} db]
      (when (and (open? db) (ex/complete? ex answer))
        (let [res (ex/check ex answer)
              stats (update (:stats db) (:kind ex)
                            (fn [{:keys [n ok]}] {:n (inc (or n 0)) :ok (+ (or ok 0) (if (:ok? res) 1 0))}))]
          {:db (assoc db :result res :stats stats :sel nil)
           :save {:stats stats}})))))

(rf/reg-event-fx :stats/reset
  (fn [{:keys [db]} _] {:db (assoc db :stats {}) :save {:stats {}}}))

(defn shown-cols
  "The columns on the staff: the exercise's, or the user's while writing."
  [{:keys [ex answer]}]
  (if (writing? (:kind ex)) answer (:cols ex)))

(rf/reg-event-fx :play
  (fn [{:keys [db]} [_ cols]]
    (let [cols (or cols (shown-cols db))
          gap 0.9]
      {:audio/play (for [[i notes] (map-indexed vector cols) n notes]
                     [(t/midi n) (* i gap) (if (= 1 (count cols)) 2 1.1)])})))

(rf/reg-event-fx :key
  (fn [{:keys [db]} [_ k]]
    (let [writing (writing? (get-in db [:ex :kind]))]
      (when (= :practice (:view db))
        (case k
          "Enter" {:dispatch (if (:result db) [:new] [:check])}
          "ArrowUp" (when writing {:dispatch [:move 1]})
          "ArrowDown" (when writing {:dispatch [:move -1]})
          "ArrowLeft" (when writing {:dispatch [:select-col -1]})
          "ArrowRight" (when writing {:dispatch [:select-col 1]})
          ("+" "=") (when writing {:dispatch-n [[:alter-by 1]]})
          ("-" "_") (when writing {:dispatch-n [[:alter-by -1]]})
          "Backspace" (when writing {:dispatch [:delete]})
          " " {:dispatch [:play]}
          nil)))))

(rf/reg-event-fx :alter-by
  (fn [{:keys [db]} [_ d]]
    (let [[c s] (:sel db)
          n (some #(when (= s (:s %)) %) (get-in db [:answer c]))]
      (when n {:dispatch [:alter (max -2 (min 2 (+ (:a n) d)))]}))))
