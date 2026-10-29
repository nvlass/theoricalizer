(ns theoricalizer.staff
  (:require [reagent.core :as r]
            [theoricalizer.theory :as t]))

;; One staff of whole notes in columns; a column holds one note or a chord. 5px per diatonic step,
;; so lines are 10px apart. Every note shows its accidental against the signature (no carrying).

(def sp 5)
(def y0 50)
(def top {:treble 38 :bass 26})
(def col-w 46)
(def label-y 142)
(def height 150)

(def sharp-steps {:treble [38 35 39 36 33 37 34] :bass [24 21 25 22 19 23 20]})
(def flat-steps {:treble [34 37 33 36 32 35 31] :bass [20 23 19 22 18 21 17]})

(defn y-of [clef step] (+ y0 (* sp (- (top clef) step))))

(defn x0 [keysig] (+ 44 (* 9 (Math/abs (if keysig (t/signature keysig) 0))) 8))

(defn width [keysig n] (+ (x0 keysig) (* n col-w) 10))

(defn- ledger-steps [clef step]
  (let [hi (top clef) lo (- hi 8)]
    (concat (when (>= step (+ hi 2)) (range (+ hi 2) (inc step) 2))
            (when (<= step (- lo 2)) (range (- lo 2) (dec step) -2)))))

(defn- ellipse-path [x y rx ry phi]
  (let [c (Math/cos (* phi (/ Math/PI 180))) s (Math/sin (* phi (/ Math/PI 180)))]
    (str "M" (- x (* rx c)) "," (- y (* rx s))
         " A" rx "," ry " " phi " 1,0 " (+ x (* rx c)) "," (+ y (* rx s))
         " A" rx "," ry " " phi " 1,0 " (- x (* rx c)) "," (- y (* rx s)) " Z ")))

(defn- head-path [x y] (str (ellipse-path x y 7.2 5 0) (ellipse-path x y 3.4 2.6 -35)))

(defn- clef-glyph
  "Clef glyphs differ a lot between fonts, so measure the glyph and scale it into the given box."
  [_ _ _ _]
  (let [el (atom nil)
        bbox (r/atom nil)]
    (r/create-class
     {:component-did-mount
      (fn [] (when-let [n @el]
               (let [b (.getBBox n)]
                 (when (pos? (.-height b)) (reset! bbox [(.-x b) (.-y b) (.-height b)])))))
      :reagent-render
      (fn [glyph x y-top hgt]
        (let [[bx by bh] @bbox]
          [:text.clef {:ref #(reset! el %) :x 0 :y 0 :font-size 40
                       :visibility (if bh "visible" "hidden")
                       :transform (when bh (str "translate(" x "," y-top ") scale(" (/ hgt bh) ") translate(" (- bx) "," (- by) ")"))}
           glyph]))})))

(defn- acc-text [a] (if (zero? a) "♮" (t/acc-sym a)))

(defn shown-acc
  "The accidental printed before `n`, or nil."
  [keysig n]
  (when (not= (:a n) (t/sig-alt keysig (t/letter n))) (:a n)))

(defn- column-els
  "Notes of one column, lowest first: the upper note of a second moves right, and accidentals of
   close notes stagger leftwards."
  [{:keys [clef keysig]} c x notes cls]
  (let [notes (vec (sort-by :s notes))
        shifted (reduce (fn [acc i]
                          (conj acc (and (= 1 (- (:s (notes i)) (:s (notes (dec i))))) (not (peek acc)))))
                        [false] (range 1 (count notes)))]
    (for [[i n] (map-indexed vector notes)
          :let [nx (if (shifted i) (+ x 14) x)
                acc (shown-acc keysig n)
                crowd (count (filter #(and (shown-acc keysig %) (< 0 (- (:s %) (:s n)) 6)) notes))
                y (y-of clef (:s n))]]
      ^{:key i}
      [:g {:class (str "note " cls)}
       (for [st (ledger-steps clef (:s n))]
         ^{:key st} [:line.ledger {:x1 (- nx 11) :x2 (+ nx 11) :y1 (y-of clef st) :y2 (y-of clef st)}])
       (when acc [:text.acc {:x (- x 11 (* 10 crowd)) :y (+ y 5) :text-anchor "end"} (acc-text acc)])
       [:path.head {:d (head-path nx y) :fill-rule "evenodd"}]])))

(defn- event-point [e]
  (let [svg (.-currentTarget e)
        rect (.getBoundingClientRect svg)
        vb (.. svg -viewBox -baseVal)
        k (/ (.-width vb) (.-width rect))]
    [(* k (- (.-clientX e) (.-left rect))) (* k (- (.-clientY e) (.-top rect)))]))

(defn staff
  "Props: :clef :keysig :cols (vectors of notes) :given (set of columns) :sel [c step]
   :marks {c :good|:bad} :labels {c text} (c may be fractional) :bars (columns a barline follows) :on-pick (fn [c step]); :avail is the width in px it
   may take, the zoom is at most 2. A staff
   you can't write on is cropped to its notes."
  [_]
  (let [hover (r/atom nil)]
    (fn [{:keys [clef keysig cols given sel marks labels on-pick editable avail bars] :as props}]
      (let [n (count cols)
            w (width keysig n)
            z (min 2 (/ (or avail 600) w))
            ys (map #(y-of clef (:s %)) (mapcat identity cols))
            full? (or on-pick editable)
            [vy0 vy1] (if full?
                        [0 label-y]
                        [(min (- y0 14) (- (apply min (+ y0 20) ys) 12))
                         (max (+ y0 54) (+ (apply max (+ y0 20) ys) 12))])
            ly (if full? label-y (+ vy1 4))
            vh (+ (- ly vy0) (if (seq labels) 10 -2))
            xs (x0 keysig)
            cx (fn [c] (+ xs (* col-w c) (/ col-w 2)))
            [lo hi] [(- (top clef) 14) (+ (top clef) 6)]
            pick (fn [e]
                   (let [[x y] (event-point e)
                         c (Math/floor (/ (- x xs) col-w))]
                     (when (and (<= 0 c) (< c n) (or (nil? editable) (editable c)))
                       {:c c :step (max lo (min hi (+ (top clef) (Math/round (/ (- y0 y) sp)))))})))
            sig (if keysig (t/signature keysig) 0)
            sig-steps (take (Math/abs sig) ((if (neg? sig) flat-steps sharp-steps) clef))]
        [:svg.staff {:viewBox (str "0 " vy0 " " w " " vh)
                     :width (* z w) :height (* z vh)
                     :class (when on-pick "interactive")
                     :on-pointer-move (when on-pick #(reset! hover (pick %)))
                     :on-pointer-leave #(reset! hover nil)
                     :on-click (when on-pick #(when-let [{:keys [c step]} (pick %)] (on-pick c step)))}
         (for [c (range n)]
           ^{:key (str "bg" c)}
           [:rect {:class (str "colbg"
                               (when (= c (first sel)) " cur")
                               (some->> (get marks c) name (str " ")))
                   :x (+ xs (* col-w c) 2) :y (+ vy0 4) :width (- col-w 4) :height (- ly vy0) :rx 6}])
         [:g.lines
          (for [k (range 5)]
            ^{:key k} [:line {:x1 2 :x2 (- w 2) :y1 (+ y0 (* 10 k)) :y2 (+ y0 (* 10 k))}])
          [:line {:x1 2 :x2 2 :y1 y0 :y2 (+ y0 40)}]
          [:line.final {:x1 (- w 2) :x2 (- w 2) :y1 y0 :y2 (+ y0 40)}]
          (for [c bars :let [x (+ xs (* col-w (inc c)))]]
            ^{:key (str "b" c)} [:line {:x1 x :x2 x :y1 y0 :y2 (+ y0 40)}])]
         (if (= clef :treble)
           [clef-glyph "𝄞" 6 (- y0 13) 68]
           [clef-glyph "𝄢" 7 (- y0 1) 32])
         (for [[k st] (map-indexed vector sig-steps)]
           ^{:key k}
           [:text.acc {:x (+ 40 (* 9 k)) :y (+ (y-of clef st) 5)} (if (neg? sig) "♭" "♯")])
         (for [c (range n)
               :let [notes (get cols c)]
               :when (seq notes)]
           ^{:key (str "c" c)}
           [:g (column-els props c (cx c) notes
                           (str (some-> (get marks c) name)
                                (when (contains? given c) " given")))])
         (when sel
           (let [[c s] sel]
             (when (some #(= s (:s %)) (get cols c))
               [:circle.sel-ring {:cx (cx c) :cy (y-of clef s) :r 11}])))
         (for [[c text] labels]
           ^{:key (str "l" c)}
           [:text.label {:x (cx c) :y (+ ly 4) :text-anchor "middle"} text])
         (when-let [{:keys [c step]} (and on-pick @hover)]
           (let [x (cx c) y (y-of clef step)]
             [:g.note.ghost
              (for [st (ledger-steps clef step)]
                ^{:key st} [:line.ledger {:x1 (- x 11) :x2 (+ x 11) :y1 (y-of clef st) :y2 (y-of clef st)}])
              [:path.head {:d (head-path x y) :fill-rule "evenodd"}]]))]))))
