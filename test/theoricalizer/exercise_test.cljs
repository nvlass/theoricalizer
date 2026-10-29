(ns theoricalizer.exercise-test
  (:require [cljs.test :refer [deftest is testing]]
            [theoricalizer.exercise :as ex]
            [theoricalizer.theory :as t]))

(defn- right-answer
  "What a user who knows the answer would enter."
  [{:keys [kind answer cols] :as e}]
  (case kind
    (:interval-name :scale-name :triad-name :interval-keys :triad-keys) answer
    :interval-write (assoc cols (:at e) [answer])
    :scale-write (mapv vector answer)
    :triad-write [(reverse answer)]))

(def option-sets
  (for [max-acc [2 4 7] clef [:treble :bass :any] forms [:harmonic :all]
        flag [false true]]
    {:max-acc max-acc :clef clef :forms forms :compound flag :aug-dim (not flag)
     :with-sig flag :inversions flag}))

(deftest every-exercise-generates-and-its-answer-checks
  (doseq [kind ex/all-kinds opts option-sets _ (range 4)
          :let [settings (assoc opts :kind kind)
                e (ex/generate rand settings)
                a (right-answer e)]]
    (testing (pr-str kind opts)
      (is (ex/complete? e a))
      (is (:ok? (ex/check e a)) (pr-str e))
      (let [[lo hi] (ex/ranges (:clef e))]
        (is (every? #(<= lo (:s %) hi) (concat (mapcat identity (:cols e)) (when (vector? (:answer e)) (:answer e)))))))))

(deftest keys-exercises-have-an-answer-on-the-grid
  (doseq [kind [:interval-keys :triad-keys] opts option-sets _ (range 4)
          :let [e (ex/generate rand (assoc opts :kind kind))]]
    (is (seq (:answer e)))
    (is (every? #(<= (Math/abs (t/signature %)) (:max-acc opts)) (:answer e)))))

(deftest wrong-answers-fail
  (let [e (ex/generate rand {:kind :scale-write :max-acc 4 :clef :treble :forms :harmonic})
        cols (assoc (mapv vector (:answer e)) 3 [(update (get-in e [:answer 3]) :a inc)])]
    (is (not (:ok? (ex/check e cols))))
    (is (= :bad (get-in (ex/check e cols) [:marks 3]))))
  (let [e (ex/generate rand {:kind :triad-write :max-acc 4 :clef :treble :forms :harmonic :inversions true})
        [a b c] (:answer e)]
    (is (not (:ok? (ex/check e [[(update a :s + 7) b c]]))) "same notes, wrong bass")))
