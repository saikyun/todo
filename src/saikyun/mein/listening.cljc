(ns saikyun.mein.listening
  (:require [clojure.string :as str]
            [saikyun.mein.collections :refer [fconj fmap!]]
            [saikyun.mein.component :as c]
            [hiccups.runtime :as hrt]))

(defn kebab->event
  [s]
  (->> (name s)
       (#(str/split % "-"))
       (str/join "")))

(defmacro listen
  [sym]
  `(let [var# (resolve '~sym)
         id# (str "listener-" (rand-int 10))]
     (swap! listeners update (deref var#) fconj {:id id#, :f 
                                                 #(set! (.-innerHTML %1) %2)})
     [:span {:id id#
             :class (str "listener" (symbol var#))}
      (deref (deref var#))]))


(def listeners (atom {}))
(def to-remove #js [])

(defn notify-listeners
  [_ ref _ new]
  (doseq [{:keys [f node id] :as data} (get @listeners ref)]
    (if-not (.-parentNode node)
      (.push to-remove id)
      (f new data)))
  
  (when-let [r (and (seq to-remove)
                    (into #{} to-remove))]
    (swap! listeners update ref (fn [ls] (into [] (remove #(r (:id %)) ls))))
    (set! (.-length to-remove) nil)))

#?(:cljs (defn add-listener
           [c]
           (let [{:keys [listen id] :as comp} (meta c)
                 listen-and-init
                 (fn [{:keys [atom f]}]
                   (let [node (.getElementById js/document id)
                         data {:id id, :component comp, :f f, :node node}]
                     (swap! listeners update atom fconj data)
                     (add-watch atom :listeners notify-listeners)
                     (f @atom data)))]
             
             (if (map? listen)
               (listen-and-init listen)
               (fmap! listen-and-init listen)))))

(defmacro listen-comp
  [sym comp-f]
  `(let [var# (resolve '~sym)
         comp-f# #(-> (~comp-f %) c/hydrate)
         cb# (fn [elem# value#]
               (let [comp# (comp-f# value#)]
                 (when elem#
                   (set! (.-innerHTML elem#) (hrt/render-html comp#))
                   (c/traverse-hiccup #(do (c/trigger-load %) %) comp#))))
         id# (str "listener-" (rand-int 10))]
     (swap! listeners update (deref var#) fconj {:id id#, :f cb#})
     [:div {:id id#
            :class (str "listener" (symbol var#))}
      (comp-f# (deref (deref var#)))]))

(defn add-event
  [c]
  #?(:cljs (let [{:keys [on id]} (meta c)]
             (doseq [[e cb] on]
               (fmap! #(.addEventListener (.getElementById js/document id) (kebab->event e) %) cb)))))

