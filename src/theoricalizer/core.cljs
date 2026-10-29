(ns theoricalizer.core
  (:require [reagent.dom.client :as rdc]
            [re-frame.core :as rf]
            [clojure.string :as str]
            [theoricalizer.events]
            [theoricalizer.subs]
            [theoricalizer.views :as views]
            [theoricalizer.guard :as guard]
            [theoricalizer.audio :as audio]
            [theoricalizer.storage :as storage]))

(defonce root (atom nil))

(defn ^:dev/after-load mount []
  (rf/clear-subscription-cache!)
  (when-not @root
    (reset! root (rdc/create-root (js/document.getElementById "app"))))
  (rdc/render @root [guard/guard [views/app]]))

(def captured
  "Keys the app takes over, even on a focused button: Enter checks rather than clicking it again."
  #{"ArrowUp" "ArrowDown" "ArrowLeft" "ArrowRight" " " "Backspace" "Enter"})

(defn- on-key [e]
  (let [tag (some-> (.-target e) .-tagName str/lower-case)
        k (.-key e)]
    (when-not (or (#{"input" "select" "textarea"} tag) (.-altKey e) (.-metaKey e) (.-ctrlKey e))
      (when (captured k) (.preventDefault e))
      (rf/dispatch [:key k]))))

(defn init []
  (rf/dispatch-sync [:init])
  (rf/dispatch-sync [:resize (.-innerWidth js/window)])
  (storage/load-all! [:settings :stats] #(rf/dispatch [:storage/loaded %]))
  (audio/install-unlock!)
  (audio/load! #(rf/dispatch [:audio/loaded %]))
  (.addEventListener js/document "keydown" on-key)
  (.addEventListener js/window "resize" #(rf/dispatch [:resize (.-innerWidth js/window)]))
  (when (and (not ^boolean goog.DEBUG) (.-serviceWorker js/navigator))
    (.register (.-serviceWorker js/navigator) "sw.js"))
  (mount))
