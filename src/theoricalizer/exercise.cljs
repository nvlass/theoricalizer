(ns theoricalizer.exercise
  (:require [theoricalizer.theory :as t]))

;; Exercises are plain maps; `generate` takes a rand fn (0–1) so tests can seed it.
;; Writing exercises keep the user's notes as columns: a vector of note vectors. Intervals are
;; always melodic, lower note first; writing one gives the lower note (write above) or the upper
;; (write below), and :at is the column to write in.

(def kinds
  {:intervals [:interval-name :interval-write :interval-keys]
   :scales [:scale-name :scale-write]
   :triads [:triad-name :triad-write :triad-keys]})

(def all-kinds (vec (mapcat kinds [:intervals :scales :triads])))

(def ranges {:treble [26 42] :bass [14 30]})

(defn forms [settings] (if (= :all (:forms settings)) t/minor-forms [:harmonic]))

(defn- pick [rand xs] (nth xs (Math/floor (* (rand) (count xs)))))

(defn- in-range? [clef notes]
  (let [[lo hi] (ranges clef)] (every? #(<= lo (:s %) hi) notes)))

(defn- max-alt [settings] (if (>= (:max-acc settings) 6) 2 1))

(defn- spellable? [settings notes] (every? #(<= (Math/abs (:a %)) (max-alt settings)) notes))

(defn- pick-clef [rand {:keys [clef]}] (if (= clef :any) (pick rand [:treble :bass]) clef))

(defn- random-key [rand settings]
  (pick rand (t/all-keys (:max-acc settings))))

(defn- place
  "The note with spelling `sp` whose step is within [lo, hi] (the lowest such)."
  [{:keys [l a]} lo hi]
  (let [s (first (filter #(= l (mod % 7)) (range lo (inc hi))))]
    (when s {:s s :a a})))

(defn- scale-note [rand settings clef]
  (let [key (random-key rand settings)
        type (pick rand (t/key-types key (forms settings)))
        [lo hi] (ranges clef)]
    (place (pick rand (t/key-scale key type)) (+ lo 2) (- hi 2))))

(defn- interval-pool [settings]
  (for [num (range 1 (if (:compound settings) 16 9))
        q (if (:aug-dim settings) t/qualities [:P :M :m])
        :let [iv {:num num :q q}]
        :when (and (t/valid-interval? iv) (not= iv {:num 1 :q :P}))]
    iv))

(defn- retry
  "Calls `f` until it returns something; generation constraints rarely fail twice in a row."
  [f]
  (loop [i 0] (or (f) (if (< i 500) (recur (inc i)) (throw (js/Error. "no exercise"))))))

(defmulti generate (fn [_ settings] (:kind settings)))

(defn- interval-pair [rand settings]
  (let [clef (pick-clef rand settings)
        x (scale-note rand settings clef)
        iv (pick rand (interval-pool settings))
        dir (pick rand [:up :down])
        y (when x (t/transpose x iv dir))]
    (when (and x (in-range? clef [x y]) (spellable? settings [x y]))
      {:clef clef :x x :y y :iv iv :dir dir})))

(defmethod generate :interval-name [rand settings]
  (retry #(when-let [{:keys [clef x y iv dir]} (interval-pair rand settings)]
            {:kind :interval-name :clef clef :cols (if (= dir :up) [[x] [y]] [[y] [x]]) :answer iv})))

(defmethod generate :interval-write [rand settings]
  (retry #(when-let [{:keys [clef x y iv dir]} (interval-pair rand settings)]
            (if (= dir :up)
              {:kind :interval-write :clef clef :cols [[x] []] :given #{0} :at 1 :iv iv :dir dir :answer y}
              {:kind :interval-write :clef clef :cols [[] [x]] :given #{1} :at 0 :iv iv :dir dir :answer y}))))

(defmethod generate :interval-keys [rand settings]
  (retry #(let [clef (pick-clef rand settings)
                key (random-key rand settings)
                sc (t/key-scale key (pick rand (t/key-types key (forms settings))))
                [i j] (sort [(Math/floor (* 7 (rand))) (Math/floor (* 7 (rand)))])
                [lo hi] (ranges clef)
                x (place (sc i) (+ lo 2) (- hi 7))
                y (when x (place (sc j) (inc (:s x)) (+ (:s x) 7)))]
            (when (and y (< i j))
              {:kind :interval-keys :clef clef :cols [[x] [y]]
               :answer (set (map :key (t/interval-keys x y (forms settings) (:max-acc settings))))}))))

(defn- scale-pick [rand settings]
  (let [clef (pick-clef rand settings)
        type (pick rand t/scale-types)
        key (pick rand (t/keys-of (t/mode-of type) (:max-acc settings)))
        [lo hi] (ranges clef)
        tonic (place (:tonic key) (+ lo 1) (- hi 8))
        notes (when tonic (t/scale tonic type))]
    (when (and notes (spellable? settings notes))
      {:clef clef :type type :key key :tonic tonic :notes notes})))

(defmethod generate :scale-name [rand settings]
  (retry #(when-let [{:keys [clef type notes key]} (scale-pick rand settings)]
            {:kind :scale-name :clef clef :cols (mapv vector notes)
             :answer {:tonic (:tonic key) :type type}})))

(defmethod generate :scale-write [rand settings]
  (retry #(when-let [{:keys [clef type notes key tonic]} (scale-pick rand settings)]
            {:kind :scale-write :clef clef :type type :tonic tonic
             :keysig (when (:with-sig settings) key)
             :cols (into [[tonic]] (repeat 7 [])) :given #{0} :answer notes})))

(defn- triad-pick [rand settings]
  (let [clef (pick-clef rand settings)
        key (random-key rand settings)
        type (pick rand (t/key-types key (forms settings)))
        sc (t/key-scale key type)
        d (Math/floor (* 7 (rand)))
        quality (t/triad-quality (map #(t/degree (t/tonic-note key) type %) [d (+ d 2) (+ d 4)]))
        inv (if (:inversions settings) (pick rand [0 1 2]) 0)
        [lo hi] (ranges clef)
        root (place (sc d) (+ lo 1) (- hi 11))
        notes (when root (t/triad root quality inv))]
    (when (and notes (in-range? clef notes) (spellable? settings notes))
      {:clef clef :root root :quality quality :inv inv :notes notes})))

(defmethod generate :triad-name [rand settings]
  (retry #(when-let [{:keys [clef root quality inv notes]} (triad-pick rand settings)]
            {:kind :triad-name :clef clef :cols [notes]
             :answer {:root (t/spelling root) :quality quality :inv inv}})))

(defmethod generate :triad-write [rand settings]
  (retry #(when-let [{:keys [clef root quality inv notes]} (triad-pick rand settings)]
            {:kind :triad-write :clef clef :root (t/spelling root) :quality quality :inv inv
             :cols [[]] :answer notes})))

(defmethod generate :triad-keys [rand settings]
  (retry #(when-let [{:keys [clef root quality notes]} (triad-pick rand settings)]
            {:kind :triad-keys :clef clef :cols [notes] :root root :quality quality
             :answer (set (map :key (t/triad-keys root quality (forms settings) (:max-acc settings))))})))

;; ---------------------------------------------------------------- checking

(defmulti check
  "Grades `answer` (the user's choices, or written columns): {:ok? bool ...details}."
  (fn [ex _] (:kind ex)))

(defmethod check :interval-name [{:keys [answer]} a]
  {:ok? (= answer (select-keys a [:num :q]))})

(defmethod check :interval-write [{:keys [answer at]} cols]
  (let [n (first (get cols at))]
    {:ok? (= answer n) :marks {at (if (= answer n) :good :bad)}}))

(defn- check-keys [answer picked]
  {:ok? (= answer picked)
   :missed (remove picked answer)
   :extra (remove answer picked)})

(defmethod check :interval-keys [{:keys [answer]} picked] (check-keys answer picked))

(defmethod check :triad-keys [{:keys [answer]} picked] (check-keys answer picked))

(defmethod check :scale-name [{:keys [answer]} a]
  {:ok? (= answer (select-keys a [:tonic :type]))})

(defmethod check :scale-write [{:keys [answer]} cols]
  (let [marks (into {} (for [c (range 1 8)] [c (if (= [(answer c)] (get cols c)) :good :bad)]))]
    {:ok? (every? #{:good} (vals marks)) :marks marks}))

(defmethod check :triad-name [{:keys [answer]} a]
  {:ok? (= answer (select-keys a [:root :quality :inv]))})

(defmethod check :triad-write [{:keys [answer]} cols]
  (let [notes (sort-by (juxt :s :a) (first cols))
        ok? (and (= 3 (count notes))
                 (= (set (map t/spelling answer)) (set (map t/spelling notes)))
                 (= (t/spelling (first answer)) (t/spelling (first notes))))]
    {:ok? ok? :marks {0 (if ok? :good :bad)}}))

(defn complete?
  "Whether the user has given a full answer that can be checked."
  [{:keys [kind] :as ex} a]
  (case kind
    :interval-name (and (:num a) (:q a))
    :scale-name (and (:tonic a) (:type a))
    :triad-name (and (:root a) (:quality a) (:inv a))
    (:interval-keys :triad-keys) (seq a)
    :interval-write (seq (get a (:at ex)))
    :scale-write (every? #(seq (get a %)) (range 1 8))
    :triad-write (= 3 (count (first a)))
    false))
