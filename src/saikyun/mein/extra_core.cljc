(ns saikyun.mein.extra-core)

(defmacro update!
  [attr & [f & args]]
  `(set! ~attr (~f ~attr ~@args)))

(defn update-props
  ([cmpt f]
   (if (map? (second cmpt))
     (update cmpt 1 f)
     (with-meta [(first cmpt) (f {}) (rest cmpt)] (meta cmpt))))
  ([cmpt f & args]
   (if (map? (second cmpt))
     (apply update cmpt 1 f args)
     (with-meta [(first cmpt) (apply f {} args) (rest cmpt)] (meta cmpt)))))

(defn is-hiccup?
  [node]
  (and (vector? node)
       (keyword? (first node))))

(defn traverse-hiccup
  [f node]
  (if (not (is-hiccup? node))
    node
    (let [[tag props-or-child & children :as new-node] (f node)
          [tag props children] (if (map? props-or-child)
                                 [tag props-or-child children]
                                 [tag {} (concat [props-or-child] children)])
          children (map #(traverse-hiccup f %) children)]
      (with-meta (into [] (concat [tag props] children)) (meta new-node)))))

(defn validate-hiccup
  ([node] (validate-hiccup node [] []))
  ([node path problems]
   (if (not (is-hiccup? node))
     (if (coll? node)
       (conj problems {:problem "Collection inside hiccup form"
                       :node node
                       :path (conj path node)})
       problems)
     (let [[tag props-or-child & children :as new-node] node
           [tag props children] (if (map? props-or-child)
                                  [tag props-or-child children]
                                  [tag {} (concat [props-or-child] children)])
           new-problems (map #(validate-hiccup % (conj path node) problems) children)]
       (apply concat problems new-problems)))))
