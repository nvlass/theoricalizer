(ns theoricalizer.lessons
  (:require [re-frame.core :as rf]
            [theoricalizer.theory :as t]
            [theoricalizer.staff :as staff]
            [theoricalizer.i18n :refer [tr]]))

;; The theory tabs. Every example is computed with the same code the exercises use.

(defn- n [step a] {:s step :a a})
(def c4 (n 28 0))
(def a4 (n 33 0))
(def c-major {:tonic {:l 0 :a 0} :mode :major})
(def a-minor {:tonic {:l 5 :a 0} :mode :minor})

(defn- lesson-avail [] (- (min @(rf/subscribe [:width]) 760) 72))

(defn- example [cols labels & [opts]]
  [:div.example
   [staff/staff (merge {:clef :treble :cols cols :given #{} :labels (into {} (map-indexed vector labels))
                        :avail (lesson-avail)}
                       opts)]
   [:button.ghost.small {:on-click #(rf/dispatch [:play cols])} (tr :ui/play)]])

(defn- section [title & body]
  (into [:div.card.lesson [:h3 (tr title)]] body))

(defn- iv-label [x y] (tr :fmt/iv-short (t/interval x y)))

;; ---------------------------------------------------------------- intervals

(def simple-ivs
  (for [[num q] [[2 :m] [2 :M] [3 :m] [3 :M] [4 :P] [4 :A] [5 :d] [5 :P] [6 :m] [6 :M] [7 :m] [7 :M] [8 :P]]]
    {:num num :q q}))

(defn interval-table []
  [:table.theory
   [:thead [:tr [:th (tr :lesson/th-interval)] [:th (tr :lesson/th-semis)] [:th (tr :lesson/th-example)]]]
   [:tbody
    (for [iv simple-ivs]
      ^{:key (pr-str iv)}
      [:tr [:td (tr :fmt/iv iv)] [:td.num (t/interval-semis iv)]
       [:td (tr :fmt/note c4) "–" (tr :fmt/note (t/transpose c4 iv :up))]])]])

(defn- iv-example
  "Intervals as note pairs in sequence, lower note first, a barline between pairs, wrapped to fit."
  [pairs & [opts]]
  (let [avail (lesson-avail)
        per-line (max 1 (Math/floor (/ (- (/ avail 1.1) 62) (* 2 staff/col-w))))]
    [:div.iv-lines
     (for [[k line] (map-indexed vector (partition-all per-line pairs))
           :let [cols (vec (mapcat (fn [[x y]] [[x] [y]]) line))]]
       ^{:key k}
       [:div.example
        [staff/staff (merge {:clef :treble :cols cols :given #{} :avail avail
                             :bars (map #(inc (* 2 %)) (range (dec (count line))))
                             :labels (into {} (map-indexed (fn [i [x y]] [(+ (* 2 i) 0.5) (iv-label x y)]) line))}
                            opts)]
        [:button.ghost.small {:on-click #(rf/dispatch [:play cols])} (tr :ui/play)]])]))

(defn intervals []
  (let [up #(t/transpose %1 %2 :up)
        e4 (n 30 0) f4 (n 31 0) b4 (n 34 0)
        from-c (fn [ivs] (for [iv ivs] [c4 (up c4 iv)]))]
    [:<>
     [section :lesson/iv-what-title (tr :lesson/iv-what)
      (example [[c4] [e4] [c4 e4]] ["" (iv-label c4 e4) (iv-label c4 e4)] {:bars [1]})]
     [section :lesson/iv-quality-title (tr :lesson/iv-quality)
      [interval-table]
      (iv-example (from-c simple-ivs))]
     [section :lesson/iv-families-title (tr :lesson/iv-families)
      (iv-example (from-c [{:num 4 :q :d} {:num 4 :q :P} {:num 4 :q :A}]))
      (iv-example (from-c [{:num 3 :q :d} {:num 3 :q :m} {:num 3 :q :M} {:num 3 :q :A}]))]
     [section :lesson/iv-method-title (tr :lesson/iv-method)
      (iv-example [[f4 b4] [f4 (n 34 -1)] [(n 31 1) (n 33 0)]])]
     [section :lesson/iv-inversion-title (tr :lesson/iv-inversion)
      (iv-example [[c4 e4] [e4 (n 35 0)] [c4 (n 31 1)] [(n 31 1) (n 35 0)]])]
     [section :lesson/iv-compound-title (tr :lesson/iv-compound)
      (iv-example [[c4 (n 36 0)] [c4 (n 37 0)] [c4 (n 39 0)]])]
     [section :lesson/iv-enharmonic-title (tr :lesson/iv-enharmonic)
      (iv-example [[c4 (n 31 1)] [c4 (n 32 -1)] [c4 (n 29 1)] [c4 (n 30 -1)]])]
     [section :lesson/iv-keys-title (tr :lesson/iv-keys)
      (iv-example [[f4 b4]])
      [:ul.key-answers
       (for [{:keys [key degrees]} (t/interval-keys f4 b4 [:harmonic] 7)]
         ^{:key (pr-str key)} [:li (tr :fmt/key key) [:small " (" (inc (first degrees)) "–" (inc (second degrees)) ")"]])]
      (tr :lesson/iv-keys-harmonic)
      (iv-example [[f4 (n 32 1)] [(n 32 1) (n 38 0)] [(n 35 0) (n 39 1)] [(n 32 1) (n 35 0)]] {:keysig a-minor})]]))

;; ---------------------------------------------------------------- scales

(defn- scale-example [tonic type & [down? opts]]
  (let [notes (t/scale tonic type down?)]
    (example (mapv vector notes)
     (for [i (range 8)] (str (inc (if down? (- 7 i) i))))
     opts)))

(defn signature-table []
  [:table.theory
   [:thead [:tr [:th (tr :lesson/th-signature)] [:th (tr :lesson/th-major)] [:th (tr :lesson/th-minor)]]]
   [:tbody
    (for [k (t/keys-of :major 7)
          :let [sig (t/signature k)]]
      ^{:key sig}
      [:tr [:td.num (if (zero? sig) "–" (str (Math/abs sig) (if (pos? sig) "♯" "♭")))]
       [:td (tr :fmt/key k)] [:td (tr :fmt/key (t/relative k))]])]])

(defn degree-table []
  [:table.theory
   [:tbody
    (for [d (range 7)]
      ^{:key d}
      [:tr [:td.num (inc d)] [:td (tr :lesson/degree-name d)]])]])

(defn scales []
  [:<>
   [section :lesson/sc-major-title (tr :lesson/sc-major)
    [scale-example c4 :major]
    [scale-example (n 29 0) :major]]
   [section :lesson/sc-signature-title (tr :lesson/sc-signature)
    (example [[]] [""] {:keysig {:tonic {:l 1 :a 1} :mode :major}})
    (example [[]] [""] {:keysig {:tonic {:l 0 :a -1} :mode :major}})
    [signature-table]]
   [section :lesson/sc-minor-title (tr :lesson/sc-minor)
    [:h4 (tr :fmt/scale-type :natural)] [scale-example a4 :natural]
    [:h4 (tr :fmt/scale-type :harmonic)] [scale-example a4 :harmonic]
    [:h4 (tr :fmt/scale-type :melodic)] [scale-example a4 :melodic] [scale-example a4 :melodic true]]
   [section :lesson/sc-degrees-title (tr :lesson/sc-degrees) [degree-table]]
   [section :lesson/sc-related-title (tr :lesson/sc-related)]])

;; ---------------------------------------------------------------- triads

(defn- degree-triads [key type]
  (let [tonic (t/tonic-note key)
        tonic (assoc tonic :s (+ (:s tonic) (if (= :major (:mode key)) 28 21)))]
    (for [d (range 7)
          :let [notes (map #(t/degree tonic type %) [d (+ d 2) (+ d 4)])]]
      {:notes (vec notes) :label (t/numeral d (t/triad-quality notes))})))

(defn quality-table []
  [:table.theory
   [:thead [:tr [:th] [:th (tr :lesson/th-major)] [:th (tr :fmt/scale-type :harmonic)]]]
   [:tbody
    (for [q t/triad-qualities
          :let [ds (fn [key type] (for [{:keys [label notes]} (degree-triads key type)
                                        :when (= q (t/triad-quality notes))] label))]]
      ^{:key q}
      [:tr [:td (tr :fmt/triad-quality q)]
       [:td (or (seq (interpose " " (ds c-major :major))) "–")]
       [:td (or (seq (interpose " " (ds a-minor :harmonic))) "–")]])]])

(defn triads []
  (let [g4 (n 32 0)]
    [:<>
     [section :lesson/tr-what-title (tr :lesson/tr-what)
      (example (mapv #(t/triad-tones c4 %) t/triad-qualities)
       (map #(tr :fmt/triad-quality %) t/triad-qualities))]
     [section :lesson/tr-degrees-title (tr :lesson/tr-degrees)
      [:h4 (tr :fmt/key c-major)]
      (let [ts (degree-triads c-major :major)] (example (mapv :notes ts) (map :label ts)))
      [:h4 (tr :fmt/scale (:tonic a-minor) :harmonic)]
      (let [ts (degree-triads a-minor :harmonic)] (example (mapv :notes ts) (map :label ts) {:keysig a-minor}))]
     [section :lesson/tr-inversions-title (tr :lesson/tr-inversions)
      (example (mapv #(t/triad c4 :maj %) [0 1 2]) ["5/3" "6" "6/4"])]
     [section :lesson/tr-keys-title (tr :lesson/tr-keys)
      [quality-table]
      (example [(t/triad-tones g4 :maj)] [(tr :fmt/triad (t/spelling g4) :maj 0)])
      [:ul.key-answers
       (for [{:keys [key degree]} (distinct (map #(dissoc % :type) (t/triad-keys g4 :maj [:harmonic] 7)))]
         ^{:key (pr-str key)} [:li (tr :fmt/key key) ": " [:b (t/numeral degree :maj)]])]]]))

(defn view [v]
  (case v
    :intervals [intervals]
    :scales [scales]
    :triads [triads]))
