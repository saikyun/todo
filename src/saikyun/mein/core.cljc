(ns saikyun.mein.core
  (:require [saikyun.mein.component :as cs]
            [saikyun.mein.listening :as ls]
            [saikyun.mein.extra-core :as ec])
  #?(:cljs (:require-macros [hiccups.core :as hiccups :refer [html]])))

(defn cleanup!
  []
  (reset! ls/listeners {}))

(defn render
  [target view]
  #?(:cljs (do (doseq [{:keys [node problem path]} (ec/validate-hiccup view)]
                 (js/console.error "Node: " (str node))
                 (js/console.error "Has problem: " problem)
                 (js/console.error "In path: " (str (filter some? (map (comp :form meta) path))))
                 (js/console.error "Full tree: " (str path)))
               (let [view (if-not (:component (meta view))
                            (cs/component view)
                            view)]
                 (set! (.-innerHTML target) (html view))
                 (ec/traverse-hiccup #(do (ls/add-event %) %) view)  
                 (ec/traverse-hiccup #(do (cs/trigger-load %) %) view)  
                 (ec/traverse-hiccup #(do (ls/add-listener %) %) view)))))
