(ns theoricalizer.subs
  (:require [re-frame.core :as rf]
            [theoricalizer.events :as ev]
            [theoricalizer.i18n :as i18n]))

(doseq [k [:view :settings :stats :ex :answer :result :sel :samples :width]]
  (rf/reg-sub k (fn [db _] (k db))))

(rf/reg-sub :loc :<- [:settings] (fn [s _] (i18n/locale (:lang s) (:note-names s))))

(rf/reg-sub :names :<- [:loc] (fn [loc _] (:names loc)))

(rf/reg-sub :shown-cols (fn [db _] (ev/shown-cols db)))
