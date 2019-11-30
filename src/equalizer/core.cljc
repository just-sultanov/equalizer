(ns equalizer.core
  (:refer-clojure :exclude [compare])
  #?(:clj
     (:import
       (java.util.regex Pattern))))

(defn by-regexp? [dt pt]
  (and
    (= ::string dt)
    (= ::regexp pt)))

(defn by-fn? [pt]
  (= ::function pt))

(defn by-map? [dt pt]
  (= ::map dt pt))



(defn get-type [x]
  (cond
    (nil? x) ::nil
    (boolean? x) ::boolean
    (number? x) ::number
    (char? x) ::char
    (string? x) ::string
    (symbol? x) ::symbol
    (keyword? x) ::keyword
    (list? x) ::list
    (vector? x) ::vector
    (set? x) ::set
    (map? x) ::map
    #?@(:clj  [(instance? Pattern x) ::regexp]
        :cljs [(regexp? x) ::regexp])
    (fn? x) ::function))

(defn get-paths [m]
  (letfn [(walk-map [res path m]
            (reduce-kv
              (fn [acc k v]
                (if (map? v)
                  (walk-map acc (conj path k) v)
                  (conj acc [(conj path k) v])))
              res
              m))]
    (walk-map [] [] m)))



(defn pass [data predicate]
  {:type     :pass
   :expected predicate
   :actual   data})

(defn pass? [x]
  (= :pass (:type x)))


(defn fail [data predicate]
  {:type     :fail
   :expected predicate
   :actual   data})

(defn fail? [x]
  (= :fail (:type x)))



(declare compare)

(defn compare-map [data predicate]
  (let [paths (get-paths predicate)]
    (reduce (fn [acc [path p]]
              (let [v   (get-in data path)
                    res (compare v p)]
                (conj acc (assoc res :path path))))
      []
      paths)))



(defmulti compare
  (fn [data predicate]
    (let [dt (get-type data)
          pt (get-type predicate)]
      (cond
        (by-map? dt pt) ::map
        (by-regexp? dt pt) ::regexp
        (by-fn? pt) ::function
        :else ::equality))))


(defmethod compare ::equality
  [data predicate]
  (if (= data predicate)
    (pass data predicate)
    (fail data predicate)))


(defmethod compare ::map
  [data predicate]
  (if (= data predicate)
    (pass data predicate)
    (if (seq predicate)
      (compare-map data predicate)
      (fail data predicate))))


(defmethod compare ::regexp
  [data predicate]
  (if (re-find predicate data)
    (pass data predicate)
    (fail data predicate)))


(defmethod compare ::function
  [data predicate]
  (if (predicate data)
    (pass data predicate)
    (fail data predicate)))
