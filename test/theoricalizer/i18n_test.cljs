(ns theoricalizer.i18n-test
  (:require [cljs.test :refer [deftest is]]
            [theoricalizer.i18n :as i18n]
            [theoricalizer.i18n.en :as en]
            [theoricalizer.i18n.el :as el]
            [theoricalizer.exercise :as ex]
            [theoricalizer.theory :as t]))

(defn- flat-keys [m prefix]
  (mapcat (fn [[k v]]
            (let [k (if prefix (keyword (str prefix "." (name k))) k)]
              (if (map? v) (flat-keys v (name k)) [k])))
          m))

(deftest same-keys
  (is (= (set (flat-keys en/dict nil)) (set (flat-keys el/dict nil)))))

(deftest every-exercise-has-a-prompt-and-answer-text
  (doseq [lang [:en :el] names [:letters :solfege]
          :let [loc (i18n/locale lang names)
                tr (fn [k & args] (apply i18n/t loc k args))]
          kind ex/all-kinds
          :let [e (ex/generate rand {:kind kind :max-acc 7 :clef :any :forms :all :inversions true :aug-dim true})]]
    (is (string? (tr (keyword "kind" (name kind)))))
    (is (string? (case kind
                   :interval-write (tr :ui/task-interval-write (:iv e) (:dir e))
                   :scale-write (tr :ui/task-scale-write (:tonic e) (:type e) true)
                   :triad-write (tr :ui/task-triad-write (:root e) (:quality e) (:inv e))
                   (:interval-keys :triad-keys) (tr (keyword "ui" (str "task-" (name kind))) :all)
                   (tr (keyword "ui" (str "task-" (name kind)))))))
    (doseq [k (if (map? (:answer e)) [:fmt/iv] [])]
      (when (= kind :interval-name) (is (string? (tr k (:answer e))))))
    (doseq [key (t/all-keys 7)] (is (string? (tr :fmt/key key))))))
