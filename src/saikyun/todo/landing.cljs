(ns saikyun.todo.landing
  (:require [clojure.string :as str]
            [saikyun.mein.core :refer [render cleanup!]]
            [saikyun.mein.extra-core :refer [traverse-hiccup]]
            [saikyun.mein.component :refer-macros [defcomp]]
            [alc.x-as-tests.cljs.immediate :refer-macros [run-tests!]])
  (:require-macros [hiccups.core :as hiccups :refer [html]]))

(defn get-or-create-div!
  [id]
  (or (.getElementById js/document id)
      (let [app (.createElement js/document "div")]
        (set! (.-id app) id)
        (.. js/document -body (appendChild app))
        app)))

(defonce app-div (get-or-create-div! "app"))

(comment
  (def test-div (get-or-create-div! "test-div"))
  (type test-div)
  ;;=>  js/HTMLDivElement
  
  (.remove test-div)
  ;;=> nil
  )

(def todos-atom (atom [{:todo "Pet cat"}
                       {:todo "Paint house"}]))

(defn render-items
  [todos]
  (into [:div] (map-indexed
                (fn [i {:keys [todo done]}]
                  [:div
                   ^{:on {:input #(swap! todos-atom update-in [i :done] not)}}
                   [:input {:type "checkbox"
                            :checked done}]
                   [:label (if done {:style {:text-decoration :line-through}} {}) todo]])
                todos)))

(comment
  (render-items @todos-atom)
  
  (def test-todos [{:todo "Pet dog"}])
  
  (render-items test-todos)
  ;;=> [:div [:div [:input {:type "checkbox", :checked nil}] [:label {} "Pet dog"]]]
  
  (binding [*print-meta* true]
    (prn (render-items @todos-atom)))
  )

(defcomp view
  [:div
   {:style {:max-width "800px"
            :padding "20px"
            :padding-bottom "80px"
            :margin "0 auto"
            :background-color :white}}
   [:h1 "ToDo"]
   
   [:input {:id "add-todo"}]
   
   ^{:on {:click #(let [i (.getElementById js/document "add-todo")]
                    (swap! todos-atom conj {:todo (.. i -value)})
                    (set! (.. i -value) ""))}}
   [:button "Add"]
   
   ^{:listen {:atom todos-atom
              :f #(render (:node %2) (render-items %1))}}
   [:div]])

(comment
  ;; there should be an "Add"-button that can be clicked
  (->> (drop 2 view)
       (filter (fn [[tag _ text :as node]]
                 (and (= :button tag)
                      (= "Add" text)
                      (-> (meta node) :on :click))))
       seq
       boolean)
  ;;=> true
  )

(defn start
  []
  (render app-div view)
  
  (println)
  (println ";;; Reloded"))

(defn stop
  []
  
  (cleanup!))

(defn init
  []
  (start))

(run-tests!)
