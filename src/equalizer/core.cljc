(ns equalizer.core
  (:refer-clojure :exclude [compare])
  (:require
    [clojure.set :as set]
    [clojure.walk :as walk]
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

(defn walk-form [form]
  (walk/postwalk
    (fn [el]
      (if (fn? el)
        (get-fn-name el)
        el))
    form))


(defn pass
  [data predicate message]
  {:type     :pass
   :actual   data
   :expected (walk-form predicate)
   :message  message})

(defn pass? [x]
  (every? #(= :pass (:type %)) x))

(defn some-pass? [x]
  (boolean (some #(contains? #{:pass} (:type %)) x)))


(defn fail
  ([data predicate message]
   {:type     :fail
    :actual   data
    :expected (walk-form predicate)
    :message  message})
  ([type data predicate message]
   {:type     type
    :actual   data
    :expected (walk-form predicate)
    :message  message}))

(defn fail? [x]
  (boolean (some #(contains? #{:fail :error} (:type %)) x)))



(declare compare)

(defn compare-map [data predicate]
  (->> predicate
    get-paths
    (reduce
      (fn [acc [path p]]
        (let [v   (get-in data path)
              res (compare v p)]
          (if (pass? res)
            (->> res
              (mapv #(update % :path (comp vec flatten conj) path))
              (conj acc))
            (let [f (fail v p "The `data` isn't satisfies by `predicate`")]
              (conj acc (assoc f :path path))))))
      [])
    flatten
    vec))


(defn compare-seq [data predicates]
  (if (< (count data) (count predicates))
    [(fail :error data predicates
       "The number of `data` elements is less than the number of `predicates`")]
    (->> predicates
      (map-indexed
        (fn [idx predicate]
          [idx predicate]))
      (reduce
        (fn [acc [idx predicate]]
          (let [v    (nth data idx)
                path [idx]
                res  (compare v predicate)]
            (if (pass? res)
              (->> (compare v predicate)
                (mapv #(update % :path (comp vec reverse flatten concat) path))
                (conj acc))
              (let [f (fail v predicate "The `data` isn't satisfies by `predicate`")]
                (conj acc (assoc f :path path))))))
        [])
      flatten
      vec)))


(defn compare-every [data predicates]
  (let [res   (reduce
                (fn [acc predicate]
                  (let [res (->> data
                              (map #(compare % predicate))
                              flatten)]
                    (if (some-pass? res)
                      (conj acc predicate)
                      acc)))
                #{}
                predicates)
        fails (set/difference predicates res)]
    (if (empty? fails)
      [(pass data predicates "All `data` satisfies given `predicate`")]
      [(fail data fails "Not all `data` satisfies given `predicate`")])))


(defn compare-set [data predicates]
  (cond
    (contains? predicates data) [(pass data predicates "The `data` contains in `predicate`")]

    (set? data) (if (set/subset? predicates data)
                  [(pass data predicates "The `predicate` is a subset of `data`")]
                  [(fail data predicates "The `predicate` isn't a subset of `data`")])

    (sequential? data) (compare-every data predicates)

    :else [(fail data predicates "The `data` isn't satisfies by `predicate`")]))



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
    [(pass data predicate "The `data` and `predicate` are equal")]
    [(fail data predicate "The `data` and `predicate` aren't equal")]))


(defmethod compare ::map
  [data predicate]
  (if (= data predicate)
    [(pass data predicate "The `data` and `predicate` are equal")]
    (if (seq predicate)
      (compare-map data predicate)
      [(fail data predicate "The `predicate` aren't defined")])))


(defmethod compare ::sequential
  [data predicates]
  (if (= data predicates)
    [(pass data predicates "The `data` and `predicate` are equal")]
    (if (seq predicates)
      (compare-seq data predicates)
      [(fail data predicates "The `predicate` aren't defined")])))


(defmethod compare ::set
  [data predicates]
  (if (= data predicates)
    [(pass data predicates "The `data` and `predicate` are equal")]
    (if (seq predicates)
      (compare-set data predicates)
      [(fail data predicates "The `predicate` aren't defined")])))


(defmethod compare ::function
  [data predicate]
  (try
    (if (predicate data)
      [(pass data predicate "The `data` satisfies by `function`")]
      [(fail data predicate "The `data` isn't satisfies by `function`")])
    (catch #?@(:clj [Throwable e] :cljs [js/Error e])
           [(fail :error data predicate (ex-message e))])))


(defmethod compare ::regexp
  [data predicate]
  (try
    (if (re-find predicate data)
      [(pass data predicate "The `string` satisfies by `regexp`")]
      [(fail data predicate "The `string` isn't satisfies by `regexp`")])
    (catch #?@(:clj [Throwable e] :cljs [js/Error e])
           [(fail :error data predicate (ex-message e))])))
