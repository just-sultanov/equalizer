(ns equalizer.core
  (:refer-clojure :exclude [compare])
  (:require
    [clojure.set :as set]
    #?@(:clj  [[clojure.main :refer [demunge]]]
        :cljs [[clojure.string :as str]]))
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

(defn by-seq? [dt pt]
  (every? #(contains? #{::list ::vector} %)
    [dt pt]))

(defn by-set? [pt]
  (= ::set pt))



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
  (letfn [(walk-map [paths path m]
            (reduce-kv
              (fn [acc k v]
                (if (map? v)
                  (walk-map acc (conj path k) v)
                  (conj acc [(conj path k) v])))
              paths
              m))]
    (walk-map [] [] m)))

(defn get-fn-name [f]
  #?(:clj
     (as-> (str f) $
       (demunge $)
       (or
         (re-find #"(.+)--\d+@" $)
         (re-find #"(.+)@" $))
       (last $)
       (symbol $))

     :cljs
     (as-> (.-name f) $
       (demunge $)
       (str/split $ #"/")
       ((juxt butlast last) $)
       (update $ 0 #(str/join "." %))
       (str/join "/" $)
       (symbol $))))


(defn pass [data predicate]
  {:type      :pass
   :data      data
   :predicate predicate})

(defn pass? [x]
  (boolean (some #(= :pass (:type %)) x)))



(defn fail [data predicate]
  {:type      :fail
   :data      data
   :predicate predicate})

(defn fail? [x]
  (boolean (some #(= :fail (:type %)) x)))



(declare compare)

(defn compare-map [data predicate]
  (->> predicate
    get-paths
    (reduce (fn [acc [path p]]
              (let [v (get-in data path)]
                (->> (compare v p)
                  (mapv #(update % :path (comp flatten conj) path))
                  (conj acc))))
      [])
    flatten
    vec))


(defn compare-seq [data predicates]
  (->> predicates
    (map-indexed
      (fn [idx predicate]
        [idx predicate]))
    (reduce
      (fn [acc [idx predicate]]
        (let [v (nth data idx)]
          (->> (compare v predicate)
            (mapv #(update % :path concat [idx]))
            (conj acc))))
      [])
    flatten
    vec))


(defn compare-set [data predicates]
  (cond
    (set? data) (if (seq (set/intersection predicates data))
                  [(pass data predicates)]
                  [(fail data predicates)])

    (contains? predicates data) [(pass data predicates)]

    :else [(fail data predicates)]))



(defmulti compare
  (fn [data predicate]
    (let [dt (get-type data)
          pt (get-type predicate)]
      (cond
        (by-map? dt pt) ::map
        (by-seq? dt pt) ::sequential
        (by-set? pt) ::set
        (by-fn? pt) ::function
        (by-regexp? dt pt) ::regexp
        :else ::equality))))


(defmethod compare ::equality
  [data predicate]
  (if (= data predicate)
    [(pass data predicate)]
    [(fail data predicate)]))


(defmethod compare ::map
  [data predicate]
  (if (= data predicate)
    [(pass data predicate)]
    (if (seq predicate)
      (compare-map data predicate)
      [(fail data predicate)])))


(defmethod compare ::sequential
  [data predicates]
  (if (= data predicates)
    [(pass data predicates)]
    (if (seq predicates)
      (compare-seq data predicates)
      [(fail data predicates)])))


(defmethod compare ::set
  [data predicates]
  (if (= data predicates)
    [(pass data predicates)]
    (if (seq predicates)
      (compare-set data predicates)
      [(fail data predicates)])))


(defmethod compare ::function
  [data predicate]
  (if (predicate data)
    [(pass data (get-fn-name predicate))]
    [(fail data (get-fn-name predicate))]))


(defmethod compare ::regexp
  [data predicate]
  (if (re-find predicate data)
    [(pass data predicate)]
    [(fail data predicate)]))
