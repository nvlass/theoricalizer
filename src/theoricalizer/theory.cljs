(ns theoricalizer.theory)

;; A note is {:s diatonic step, :a alteration}: :s is letter index + 7 × octave, so C4 = 28.
;; A pitch class spelling is {:l letter :a alteration}. A key is {:tonic {:l :a} :mode :major|:minor};
;; its signature is the natural minor one, and minor keys may use three scale forms.

(def letters ["C" "D" "E" "F" "G" "A" "B"])
(def natural-pc [0 2 4 5 7 9 11])
(def letter-fifths [0 2 4 -1 1 3 5])

(defn letter [{:keys [s]}] (mod s 7))
(defn octave [{:keys [s]}] (Math/floor (/ s 7)))
(defn spelling [n] {:l (letter n) :a (:a n)})

(defn midi [{:keys [s a]}]
  (+ 12 (* 12 (Math/floor (/ s 7))) (natural-pc (mod s 7)) a))

(def acc-sym {-2 "𝄫" -1 "♭" 0 "" 1 "♯" 2 "𝄪"})

(defn pc-name [{:keys [l a]} names] (str (names l) (acc-sym a)))

(defn note-name [n names] (str (pc-name (spelling n) names) (octave n)))

;; ---------------------------------------------------------------- scales and keys

(def scale-types [:major :natural :harmonic :melodic])

(def patterns
  {:major [0 2 4 5 7 9 11]
   :natural [0 2 3 5 7 8 10]
   :harmonic [0 2 3 5 7 8 11]
   :melodic [0 2 3 5 7 9 11]})

(def minor-forms [:natural :harmonic :melodic])

(defn mode-of [type] (if (= type :major) :major :minor))

(defn degree
  "Degree `d` (0 = tonic, may pass the octave) of the scale `type` on `tonic` (a note)."
  [tonic type d]
  (let [s (+ (:s tonic) d)
        target (+ (midi tonic) ((patterns type) (mod d 7)) (* 12 (Math/floor (/ d 7))))]
    {:s s :a (- target (midi {:s s :a 0}))}))

(defn scale
  "Eight notes up from `tonic`; with `down?` the octave above back down to the tonic, melodic
   minor descending as natural minor."
  [tonic type & [down?]]
  (if down?
    (vec (for [d (range 7 -1 -1)] (degree tonic (if (= type :melodic) :natural type) d)))
    (vec (for [d (range 8)] (degree tonic type d)))))

(defn signature
  "Sharps (positive) or flats (negative) in the key signature."
  [{:keys [tonic mode]}]
  (+ (letter-fifths (:l tonic)) (* 7 (:a tonic)) (if (= mode :minor) -3 0)))

(def max-acc 7)

(defn keys-of
  "Keys of `mode` with at most `n` sharps or flats, in circle-of-fifths order (flats first)."
  [mode n]
  (sort-by signature
           (for [a [-1 0 1] l (range 7)
                 :let [k {:tonic {:l l :a a} :mode mode}]
                 :when (<= (Math/abs (signature k)) n)]
             k)))

(defn all-keys [n] (concat (keys-of :major n) (keys-of :minor n)))

(def sharp-order [3 0 4 1 5 2 6])

(defn sig-alt
  "The alteration the key signature gives letter `l`."
  [key l]
  (let [n (if key (signature key) 0)]
    (cond (pos? n) (if (some #{l} (take n sharp-order)) 1 0)
          (neg? n) (if (some #{l} (take (- n) (reverse sharp-order))) -1 0)
          :else 0)))

(defn sig-letters
  "Letters the signature alters, in the order they are written."
  [key]
  (let [n (signature key)]
    (if (neg? n) (take (- n) (reverse sharp-order)) (take n sharp-order))))

(defn tonic-note [{:keys [tonic]}] {:s (:l tonic) :a (:a tonic)})

(defn key-scale
  "Spellings of the scale `type` on the key's tonic, degree order."
  [key type]
  (mapv spelling (take 7 (scale (tonic-note key) type))))

(defn key-types
  "Scale types that count for a key: major, or the minor forms in `forms`."
  [{:keys [mode]} forms]
  (if (= mode :major) [:major] forms))

(defn degrees-in
  "Degrees (0–6) at which `sp` appears in the key's scales of `forms`, or nil."
  [key forms sp]
  (seq (distinct (for [type (key-types key forms)
                       [d x] (map-indexed vector (key-scale key type))
                       :when (= x sp)]
                   d))))

(defn key-sig? [key] (some? (:mode key)))

(defn relative [{:keys [tonic mode]}]
  (let [n {:s (:l tonic) :a (:a tonic)}
        r (if (= mode :major) (degree n :major 5) (degree n :natural 2))]
    {:tonic (spelling r) :mode (if (= mode :major) :minor :major)}))

;; ---------------------------------------------------------------- intervals

(def major-ref [0 2 4 5 7 9 11])

(defn perfect-type? [g7] (contains? #{0 3 4} g7))

(def qualities [:P :M :m :A :d])

(defn- delta [g7 q]
  (if (perfect-type? g7)
    ({:P 0 :A 1 :d -1 :AA 2 :dd -2} q)
    ({:M 0 :m -1 :A 1 :d -2 :AA 2 :dd -3} q)))

(defn valid-interval? [{:keys [num q]}]
  (let [g7 (mod (dec num) 7)]
    (and (pos? num)
         (some? (delta g7 q))
         (not (and (= num 1) (= q :d))))))

(defn interval-semis [{:keys [num q]}]
  (let [g (dec num)]
    (+ (major-ref (mod g 7)) (* 12 (quot g 7)) (delta (mod g 7) q))))

(defn interval
  "Interval between two notes regardless of order: {:num :q :semis}, :q nil when it has no name here."
  [x y]
  (let [[lo hi] (if (or (< (:s y) (:s x)) (and (= (:s x) (:s y)) (< (midi y) (midi x)))) [y x] [x y])
        g (- (:s hi) (:s lo))
        g7 (mod g 7)
        semis (- (midi hi) (midi lo))
        d (- semis (major-ref g7) (* 12 (quot g 7)))
        q (if (perfect-type? g7)
            ({0 :P 1 :A -1 :d 2 :AA -2 :dd} d)
            ({0 :M -1 :m 1 :A -2 :d 2 :AA -3 :dd} d))]
    {:num (inc g) :q q :semis semis}))

(defn transpose
  "The note `iv` above (dir :up) or below (:down) `n`."
  [n iv dir]
  (let [g (dec (:num iv))
        semis (interval-semis iv)
        s (if (= dir :up) (+ (:s n) g) (- (:s n) g))
        m (if (= dir :up) (+ (midi n) semis) (- (midi n) semis))]
    {:s s :a (- m (midi {:s s :a 0}))}))

(defn invert [{:keys [num q]}]
  {:num (- 9 (inc (mod (dec num) 7))) :q ({:P :P :M :m :m :M :A :d :d :A :AA :dd :dd :AA} q)})

(defn simple [{:keys [num] :as iv}] (assoc iv :num (inc (mod (dec num) 7))))

(defn compound? [{:keys [num]}] (> num 8))

(defn interval-keys
  "Keys (up to `n` accidentals) whose scales of `forms` contain both notes, with the degrees."
  [x y forms n]
  (for [key (all-keys n)
        :let [dx (degrees-in key forms (spelling x))
              dy (degrees-in key forms (spelling y))]
        :when (and dx dy)]
    {:key key :degrees [(first dx) (first dy)]}))

;; ---------------------------------------------------------------- triads

(def triad-qualities [:maj :min :dim :aug])

(def triad-intervals
  {:maj [{:num 3 :q :M} {:num 5 :q :P}]
   :min [{:num 3 :q :m} {:num 5 :q :P}]
   :dim [{:num 3 :q :m} {:num 5 :q :d}]
   :aug [{:num 3 :q :M} {:num 5 :q :A}]})

(defn triad-tones [root quality]
  (into [root] (map #(transpose root % :up) (triad-intervals quality))))

(defn triad
  "Close position, lowest note first: inversion 1 puts the third in the bass, 2 the fifth."
  [root quality inv]
  (let [[r t f] (triad-tones root quality)
        up #(update % :s + 7)]
    (case inv 0 [r t f] 1 [t f (up r)] 2 [f (up r) (up t)])))

(defn triad-quality [tones]
  (let [[r t f] tones
        q3 (:q (interval r t)) q5 (:q (interval r f))]
    (some (fn [[q [i3 i5]]] (when (and (= q3 (:q i3)) (= q5 (:q i5))) q)) triad-intervals)))

(defn triad-keys
  "Keys (up to `n` accidentals) whose scales of `forms` contain the triad, with the root's degree."
  [root quality forms n]
  (let [sps (map spelling (triad-tones root quality))]
    (for [key (all-keys n)
          type (key-types key forms)
          :let [sc (key-scale key type)
                d (first (keep-indexed #(when (= %2 (first sps)) %1) sc))]
          :when (and d (every? (set sc) sps))]
      {:key key :degree d :type type})))

(def romans ["I" "II" "III" "IV" "V" "VI" "VII"])

(defn numeral [d quality]
  (let [r (romans d)]
    (case quality
      :maj r
      :min (.toLowerCase r)
      :dim (str (.toLowerCase r) "°")
      :aug (str r "+"))))
