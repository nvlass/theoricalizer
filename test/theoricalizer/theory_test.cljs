(ns theoricalizer.theory-test
  (:require [cljs.test :refer [deftest is testing]]
            [theoricalizer.theory :as t]))

(defn n [s] ;; "C4", "F#4", "Bb3", "G##4"
  (let [[_ l acc o] (re-matches #"([A-G])([#b]*)(-?\d)" s)]
    {:s (+ (.indexOf "CDEFGAB" l) (* 7 (js/parseInt o)))
     :a (reduce + (map {\# 1 \b -1} acc))}))

(defn names [ns] (mapv #(t/note-name % t/letters) ns))

(deftest scale-notes
  (is (= ["D4" "E4" "F♯4" "G4" "A4" "B4" "C♯5" "D5"] (names (t/scale (n "D4") :major))))
  (is (= ["A4" "B4" "C5" "D5" "E5" "F5" "G♯5" "A5"] (names (t/scale (n "A4") :harmonic))))
  (is (= ["A4" "B4" "C5" "D5" "E5" "F♯5" "G♯5" "A5"] (names (t/scale (n "A4") :melodic))))
  (is (= ["A5" "G5" "F5" "E5" "D5" "C5" "B4" "A4"] (names (t/scale (n "A4") :melodic true))))
  (is (= "G𝄪5" (last (butlast (names (t/scale (n "A#4") :harmonic)))))))

(deftest key-signatures
  (is (= 15 (count (t/keys-of :major 7)) (count (t/keys-of :minor 7))))
  (is (= -7 (t/signature {:tonic {:l 0 :a -1} :mode :major})))
  (is (= 7 (t/signature {:tonic {:l 5 :a 1} :mode :minor})))
  (is (= {:tonic {:l 5 :a 0} :mode :minor} (t/relative {:tonic {:l 0 :a 0} :mode :major})))
  (testing "every major key's scale matches its signature"
    (doseq [k (t/keys-of :major 7)]
      (is (= (t/key-scale k :major)
             (mapv (fn [sp] (assoc sp :a (t/sig-alt k (:l sp)))) (t/key-scale k :major)))))))

(deftest interval-naming
  (is (= {:num 4 :q :A :semis 6} (t/interval (n "F4") (n "B4"))))
  (is (= {:num 5 :q :d :semis 6} (t/interval (n "B4") (n "F5"))))
  (is (= {:num 2 :q :A :semis 3} (t/interval (n "F4") (n "G#4"))))
  (is (= {:num 10 :q :M :semis 16} (t/interval (n "C4") (n "E5"))))
  (is (= (n "Eb5") (t/transpose (n "C5") {:num 3 :q :m} :up)))
  (is (= (n "Gb3") (t/transpose (n "C4") {:num 4 :q :A} :down)))
  (is (= {:num 5 :q :d} (t/invert {:num 4 :q :A})))
  (testing "transpose and interval agree"
    (doseq [s (range 21 36) a [-1 0 1] num (range 1 16) q t/qualities
            :let [iv {:num num :q q} x {:s s :a a}]
            :when (t/valid-interval? iv)
            dir [:up :down]
            :let [y (t/transpose x iv dir)]]
      (is (= iv (dissoc (t/interval x y) :semis)) (pr-str x iv dir)))))

(deftest keys-of-an-interval
  (let [ks (set (map :key (t/interval-keys (n "F4") (n "B4") [:harmonic] 7)))]
    (is (= #{{:tonic {:l 0 :a 0} :mode :major} {:tonic {:l 0 :a 0} :mode :minor}
             {:tonic {:l 5 :a 0} :mode :minor}}
           ks)))
  (testing "the augmented 2nd is only in harmonic minor"
    (is (= [{:key {:tonic {:l 5 :a 0} :mode :minor} :degrees [5 6]}]
           (t/interval-keys (n "F4") (n "G#4") [:natural :harmonic :melodic] 7)))))

(deftest triad-building
  (is (= ["E4" "G4" "C5"] (names (t/triad (n "C4") :maj 1))))
  (is (= ["F♯4" "B4" "D♯5"] (names (t/triad (n "B3") :maj 2))))
  (is (= :dim (t/triad-quality (t/triad-tones (n "B3") :dim))))
  (is (= #{["C" :major 4] ["G" :major 0] ["D" :major 3] ["C" :minor 4] ["E" :minor 2] ["B" :minor 5]
           ["A" :minor 6] ["D" :minor 3]}
         (set (for [{:keys [key degree type]} (t/triad-keys (n "G4") :maj [:natural :harmonic :melodic] 7)
                    :when (zero? (get-in key [:tonic :a]))]
                [(t/letters (get-in key [:tonic :l])) (:mode key) degree]))))
  (is (= "vii°" (t/numeral 6 :dim)))
  (is (= "III+" (t/numeral 2 :aug))))
