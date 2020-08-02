(ns saikyun.mein.component
  (:require [clojure.string :as str]
            [saikyun.mein.extra-core :as ec]
            [saikyun.mein.collections :refer [fmap fmap!]]))

(defonce id (atom 0))

(defn id! 
  ([] (id! "cid-"))
  ([prefix] (id! prefix id))
  ([prefix id-atom] (str prefix (swap! id-atom inc))))

(def events [:press :click :change :keydown :keyup :input])

(defn add-id-if-none
  [c]
  (-> (cond (not (coll? c))
            c
            
            (map? (second c))
            (assoc-in c [1 :id] (or (:id (second c))
                                    (:id (meta c))
                                    (id!)))
            
            (not (map? (second c)))
            (with-meta (into [] (concat [(first c) {:id (or (:id (meta c)) (id!))}]
                                        (rest c))) (meta c))
            
            :else c)
      (#(vary-meta % assoc :id (:id (second %))))))

(defn add-metadata-id
  [c]
  (vary-meta c assoc :id (or (and (coll? c) (map? (second c)) (:id (second c)))
                             (:id (meta c))
                             (id!))))

(defn extract-events
  [[_ props :as comp]]
  (if (not props)
    comp
    (reduce
     (fn [comp e]
       (if-let [cb (get props e)]
         (do
           (println "events ye" events e)
           (vary-meta (update comp 1 dissoc e) assoc-in [:on e] cb))
         comp))
     comp
     events)))

(defn extract-load
  [[_ props :as comp]]
  (if (not props)
    comp
    (if-let [cb (:load props)]
      (vary-meta (update comp 1 dissoc :load) assoc :load cb)
      comp)))

(def materalize (comp add-metadata-id
                      add-id-if-none
                      extract-events
                      extract-load))

(defn map->css
  [m]
  (if (map? m)
    (str/join
     "\n"
     (for [[k v] m]
       (str (name k) ": " (name v) ";")))
    (name m)))

(defmacro css
  [m]
  `(vary-meta ~m assoc
              :form ~(meta &form)
              :css true))

(defn add-css
  [[_ props :as comp]]
  (if (and (map? props)
           (:style props))
    (-> (update-in comp [1 :style] map->css)
        (vary-meta assoc :style (:style props)))
    comp))

(defn trigger-load
  [c]
  (fmap! #(% c) (:load (meta c))))

(defn hydrate
  [c]
  (ec/traverse-hiccup
   (fn [n]
     (-> n
         materalize
         add-css))
   c))

(defn c-form
  [c extra-meta]
  `(-> ~c
       (#(into [] %))
       (vary-meta merge (meta ~c) ~extra-meta)
       hydrate))

(defmacro component
  [c]
  (c-form c {:form (meta &form)}))

(defmacro defcomp
  [sym doc-or-c & [c]]
  (let [[doc c] (if (string? doc-or-c)
                  [doc-or-c c]
                  [nil doc-or-c])
        c (c-form c (-> {:form (meta &form)}
                        (#(if doc (assoc % :doc doc) %))))]
    (if doc
      `(def ~sym ~doc ~c)
      `(def ~sym ~c))))

(defmacro defncomp
  [sym doc-or-args args-or-first-form & body]
  (let [[doc args body]
        (if (string? doc-or-args)
          [doc-or-args args-or-first-form body]
          [nil doc-or-args (concat [args-or-first-form] body)])
        body (c-form `(do ~@body) (-> {:form (meta &form)}
                                      (#(if doc (assoc % :doc doc) %))))]
    (if doc
      `(defn ~sym ~doc ~args ~body)
      `(defn ~sym ~args ~body))))
