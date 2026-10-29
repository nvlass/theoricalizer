(ns theoricalizer.views
  (:require [re-frame.core :as rf]
            [theoricalizer.theory :as t]
            [theoricalizer.exercise :as ex]
            [theoricalizer.events :as ev]
            [theoricalizer.staff :as staff]
            [theoricalizer.lessons :as lessons]
            [theoricalizer.i18n :refer [tr]]))

(defn <sub [q] @(rf/subscribe q))
(defn >evt [e] (rf/dispatch e))

(defn seg [options value on-change]
  [:div.seg
   (for [[v label] options]
     ^{:key (str v)}
     [:button {:class (when (= v value) "on") :on-click #(on-change v)} label])])

(defn toggle [label value on-change & [hint]]
  [:label.toggle
   [:input {:type "checkbox" :checked (boolean value) :on-change #(on-change (not value))}]
   [:span label (when hint [:small hint])]])

(defn field [label & body] (into [:div.field [:span label]] body))

(defn avail
  "Width in px a staff may take inside a card, less `used` for things beside it."
  [used]
  (- (min (<sub [:width]) 760) 72 used))

(defn- setter [k] #(>evt [:settings/set k %]))

;; ---------------------------------------------------------------- choosing an exercise

(defn- kind-label [k]
  (let [{:keys [n ok]} (get (<sub [:stats]) k)]
    [:<> (tr (keyword "kind" (name k))) (when n [:small.count (str ok "/" n)])]))

(defn picker []
  (let [s (<sub [:settings])]
    [:div.card.picker
     (for [g [:intervals :scales :triads]]
       ^{:key g}
       [:div.pick-row
        [:div.group-label (tr (keyword "nav" (name g)))]
        [seg (vec (for [k (ex/kinds g)] [k [kind-label k]])) (:kind s) (setter :kind)]])
     [:details.options
      [:summary (tr :ui/options)]
      [:div.options-body
       [field (tr :ui/clef)
        [seg [[:treble (tr :ui/treble)] [:bass (tr :ui/bass)] [:any (tr :ui/both)]] (:clef s) (setter :clef)]]
       [field (tr :ui/max-acc)
        [seg (vec (for [n [2 4 7]] [n (tr :ui/up-to n)])) (:max-acc s) (setter :max-acc)]]
       [field (tr :ui/minor-forms)
        [seg [[:harmonic (tr :ui/forms-harmonic)] [:all (tr :ui/forms-all)]] (:forms s) (setter :forms)]
        [:small.hint (tr :ui/forms-hint)]]
       [toggle (tr :ui/aug-dim) (:aug-dim s) (setter :aug-dim)]
       [toggle (tr :ui/compound) (:compound s) (setter :compound)]
       [toggle (tr :ui/with-sig) (:with-sig s) (setter :with-sig) (tr :ui/with-sig-hint)]
       [toggle (tr :ui/inversions) (:inversions s) (setter :inversions)]
       [field (tr :ui/note-names)
        [seg [[:letters "C D E"] [:solfege (tr :ui/solfege-sample)]] (:note-names s) (setter :note-names)]]]]]))

;; ---------------------------------------------------------------- answering

(defn- choice-row [options value k]
  [seg options value #(>evt [:answer/set k %])])

(defn pc-picker
  "Letter + accidental, stored as a spelling under `k`."
  [k doubles?]
  (let [names (<sub [:names])
        {:keys [l a] :or {a 0}} (get (<sub [:answer]) k)]
    [:div.pc-picker
     [seg (vec (for [i (range 7)] [i (names i)])) l #(>evt [:answer/set k {:l % :a a}])]
     [seg (vec (for [x (if doubles? [-2 -1 0 1 2] [-1 0 1])] [x (if (zero? x) "♮" (t/acc-sym x))]))
      (when l a) #(>evt [:answer/set k {:l (or l 0) :a %}])]]))

(defn interval-input []
  (let [a (<sub [:answer]) s (<sub [:settings])]
    [:div.inputs
     [choice-row (vec (for [q t/qualities] [q (tr :fmt/quality q)])) (:q a) :q]
     [choice-row (vec (for [n (range 1 (if (:compound s) 16 9))] [n (str n)])) (:num a) :num]]))

(defn scale-input []
  [:div.inputs
   [pc-picker :tonic false]
   [choice-row (vec (for [ty t/scale-types] [ty (tr :fmt/scale-type ty)])) (:type (<sub [:answer])) :type]])

(defn triad-input []
  (let [a (<sub [:answer])]
    [:div.inputs
     [pc-picker :root (>= (:max-acc (<sub [:settings])) 6)]
     [choice-row (vec (for [q t/triad-qualities] [q (tr :fmt/triad-quality q)])) (:quality a) :quality]
     [choice-row (vec (for [i [0 1 2]] [i (tr :fmt/inv i)])) (:inv a) :inv]]))

(defn keys-input []
  (let [s (<sub [:settings])
        picked (<sub [:answer])
        {:keys [ok? missed extra]} (<sub [:result])
        missed (set missed) extra (set extra)
        names (<sub [:names])]
    [:div.key-grid
     (for [mode [:major :minor]]
       ^{:key mode}
       [:div.key-row
        [:div.group-label (tr (keyword "ui" (str "keys-" (name mode))))]
        [:div.chips
         (for [k (t/keys-of mode (:max-acc s))]
           ^{:key (pr-str k)}
           [:button.chip {:class [(when (picked k) "on")
                                  (when (some? ok?) (cond (missed k) "missed" (extra k) "wrong" (picked k) "right"))]
                          :title (tr :fmt/key k)
                          :on-click #(>evt [:answer/toggle-key k])}
            (t/pc-name (:tonic k) names)])]])]))

(defn write-tools []
  (let [[c s] (<sub [:sel])
        note (when s (some #(when (= s (:s %)) %) (get (<sub [:answer]) c)))]
    [:div.toolbar
     [:span.hint (if note (tr :ui/selected (tr :fmt/note note)) (tr :ui/click-staff))]
     [:div.tools
      (for [a [-2 -1 0 1 2]]
        ^{:key a}
        [:button {:disabled (not note) :class (when (= a (:a note)) "on")
                  :on-click #(>evt [:alter a])}
         (if (zero? a) "♮" (t/acc-sym a))])
      [:button {:disabled (not note) :title (tr :ui/up) :on-click #(>evt [:move 1])} "↑"]
      [:button {:disabled (not note) :title (tr :ui/down) :on-click #(>evt [:move -1])} "↓"]
      [:button {:disabled (not note) :title (tr :ui/delete) :on-click #(>evt [:delete])} "⌫"]]]))

;; ---------------------------------------------------------------- feedback

(defn- degree-list [ds] (str (inc (first ds)) "–" (inc (second ds))))

(defn key-answers [e]
  (let [s (<sub [:settings])
        forms (ex/forms s)]
    [:ul.key-answers
     (case (:kind e)
       :interval-keys
       (let [[[x] [y]] (:cols e)]
         (for [{:keys [key degrees]} (t/interval-keys x y forms (:max-acc s))]
           ^{:key (pr-str key)} [:li (tr :fmt/key key) [:small " (" (degree-list degrees) ")"]]))
       :triad-keys
       (for [[key rows] (group-by :key (t/triad-keys (:root e) (:quality e) forms (:max-acc s)))]
         ^{:key (pr-str key)}
         [:li (tr :fmt/key key) ": " [:b (t/numeral (:degree (first rows)) (:quality e))]
          (when (= :minor (:mode key))
            [:small " (" (tr :fmt/forms (map :type rows)) ")"])]))]))

(defn feedback []
  (let [e (<sub [:ex])
        {:keys [ok?]} (<sub [:result])
        a (:answer e)]
    [:div.feedback
     [:div.banner {:class (if ok? "good" "bad")} (if ok? (tr :ui/correct) (tr :ui/wrong))]
     [:p
      (case (:kind e)
        :interval-name (tr :ui/it-is (tr :fmt/iv a) (t/interval-semis a))
        :interval-write (tr :ui/it-is (tr :fmt/note a) nil)
        :scale-name (tr :ui/it-is (tr :fmt/scale (:tonic a) (:type a)) nil)
        :scale-write nil
        :triad-name (tr :ui/it-is (tr :fmt/triad (:root a) (:quality a) (:inv a)) nil)
        :triad-write (tr :ui/it-is (apply str (interpose " " (map #(tr :fmt/note %) a))) nil)
        (tr :ui/keys-answer))]
     (when (ev/keys-kind? (:kind e)) [key-answers e])]))

(defn prompt [e]
  (case (:kind e)
    :interval-write (tr :ui/task-interval-write (:iv e) (:dir e))
    :scale-write (tr :ui/task-scale-write (:tonic e) (:type e) (some? (:keysig e)))
    :triad-write (tr :ui/task-triad-write (:root e) (:quality e) (:inv e))
    :interval-keys (tr :ui/task-interval-keys (:forms (<sub [:settings])))
    :triad-keys (tr :ui/task-triad-keys (:forms (<sub [:settings])))
    (tr (keyword "ui" (str "task-" (name (:kind e)))))))

(defn- labels
  "Under the staff: the right notes where a written answer was wrong."
  [e result]
  (when (and result (not (:ok? result)))
    (case (:kind e)
      :interval-write {(:at e) (tr :fmt/note (:answer e))}
      :scale-write (into {} (for [[c m] (:marks result) :when (= m :bad)] [c (tr :fmt/note ((:answer e) c))]))
      nil)))

(defn- right-cols [{:keys [kind answer cols at]}]
  (case kind
    :interval-write (assoc cols at [answer])
    :scale-write (mapv vector answer)
    :triad-write [answer]))

(defn exercise-card []
  (let [e (<sub [:ex])
        result (<sub [:result])
        answer (<sub [:answer])
        writing (ev/writing? (:kind e))]
    (when e
      [:div.card.exercise
       [:h3 (prompt e)]
       (when writing [write-tools])
       [:div.staff-wrap
        [staff/staff {:clef (:clef e) :keysig (:keysig e) :cols (<sub [:shown-cols]) :avail (avail 0)
                      :given (or (:given e) #{}) :sel (<sub [:sel])
                      :marks (:marks result) :labels (labels e result)
                      :editable (when writing (complement (or (:given e) #{})))
                      :on-pick (when (and writing (not result)) #(>evt [:pick %1 %2]))}]]
       (when-not writing
         (case (:kind e)
           :interval-name [interval-input]
           :scale-name [scale-input]
           :triad-name [triad-input]
           [keys-input]))
       (when result [feedback])
       [:div.actions
        [:button {:on-click #(>evt [:play])} (tr :ui/play)]
        (when (and result writing (not (:ok? result)))
          [:button {:on-click #(>evt [:play (right-cols e)])} (tr :ui/play-right)])
        (if result
          [:button.primary {:on-click #(>evt [:new])} (tr :ui/next)]
          [:button.primary {:disabled (not (ex/complete? e answer)) :on-click #(>evt [:check])} (tr :ui/check)])]
       [:p.hint.keys-hint (tr (if writing :ui/keys-write :ui/keys-hint))]])))

(defn practice-view []
  [:<>
   [picker]
   [exercise-card]])

;; ---------------------------------------------------------------- app

(defn app []
  (let [view (<sub [:view])
        s (<sub [:settings])]
    [:div.app
     [:header
      [:h1 "Theoricalizer"]
      [:div.header-end
       [:div.lang [seg [[:en "EN"] [:el "ΕΛ"]] (:lang s) (setter :lang)]]]]
     [:nav
      (for [v [:practice :intervals :scales :triads]]
        ^{:key v}
        [:button {:class (when (= v view) "on") :on-click #(>evt [:set-view v])} (tr (keyword "nav" (name v)))])]
     [:main
      (case view
        :practice [practice-view]
        [lessons/view view])]]))
