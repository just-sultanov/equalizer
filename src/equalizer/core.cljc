(ns equalizer.core
  (:refer-clojure :exclude [compare])
  #?(:clj
     (:import
       (java.util.regex Pattern))))

(def simple-types
  #{::nil ::boolean ::number ::char ::string ::symbol ::keyword})

(defn simple-type? [x]
  (contains? simple-types x))


(def function-types
  #{::function})

(defn function-type? [x]
  (contains? function-types x))


(def sequential-types
  #{::list ::vector})

(defn sequential-type? [x]
  (contains? sequential-types x))



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



(defn pass [actual predicate]
  {:type     :pass
   :expected predicate
   :actual   actual})

(defn pass? [x]
  (= :pass (:type x)))


(defn fail [actual predicate]
  {:type     :fail
   :expected predicate
   :actual   actual})

(defn fail? [x]
  (= :fail (:type x)))



(declare compare)

(defn compare-all [actual predicates]
  (->> predicates
    (map-indexed
      (fn [idx predicate]
        [idx predicate]))
    (reduce
      (fn [acc [idx predicate]]
        (let [res (compare actual predicate)]
          (conj acc (assoc res :path [idx]))))
      [])))



(defmulti compare
  (fn [actual predicate]
    (let [actual-type    (get-type actual)
          predicate-type (get-type predicate)]
      (cond
        (simple-type? predicate-type) ::equality
        (sequential-type? predicate-type) ::sequential
        (function-type? predicate-type) ::function
        :else [actual-type predicate-type]))))


(defmethod compare ::equality
  [actual predicate]
  (if-not (= actual predicate)
    (fail actual predicate)
    (pass actual predicate)))


(defmethod compare ::function
  [actual predicate]
  (if-not (and (boolean (predicate actual)) true)
    (fail actual predicate)
    (pass actual predicate)))


(defmethod compare ::sequential
  [actual predicates]
  (compare-all actual predicates))


(defmethod compare [::string ::regexp]
  [actual predicate]
  (if-not (re-find predicate actual)
    (fail actual predicate)
    (pass actual predicate)))
