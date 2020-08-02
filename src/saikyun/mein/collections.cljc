(ns saikyun.mein.collections)

(defn fconj
  ([c v] (fconj c v []))
  ([c v kind]
   (cond (nil? c)
         (conj kind v)
         
         (coll? c)
         (into kind (conj c v))
         
         :else
         (conj kind c v))))

(comment

  (fact "turns two non-coll values into collection"
        (fconj 1 2)     => [1 2]
        (fconj 1 [2])   => [1 [2]]
        (fconj 1 2 #{}) => #{1 2})

  (fact "turns collection and value into that collection"
        (fconj [1 2] 3) => [1 2 3]
        (fconj [1 2] [3]) => [1 2 [3]])

  (fact "nil and value returns coll with value as only element"
        (fconj nil 2)   => [2]))

(defn fmap
  [f v]
  (cond (nil? v) v
        
        (coll? v)
        (map f v)
        
        :else
        (f v)))

(defn fmap!
  [f v]
  (cond (nil? v) v
        
        (or (coll? v)
            #?(:cljs (.isArray js/Array v)))
        (doall (map f v))
        
        :else
        (f v)))

(comment
  
  (fact "works on values"
        (fmap inc 1)
        => 2)

  (fact "works on collections"
        (fmap inc [1 2 3])
        => [2 3 4]
        
        (fmap inc '(1 2 3))
        => [2 3 4]
        
        (->> (fmap #(update % 1 inc) '{:a 10, :b 20})
             (into {}))
        => {:a 11, :b 21})

  (run-tests)
  )
