(ns equalizer.core
  (:refer-clojure :exclude [compare])
  #?(:clj
     (:import
       (java.util.regex Pattern))))

(def simple-types
  #{::nil ::boolean ::number ::char ::string ::symbol ::keyword})

(defn simple-type? [x]
  (contains? simple-types x))



(defn fail [actual predicate]
  {:actual actual :expected predicate})

(defn pass? [x]
  (nil? x))

(defn fail? [x]
  (some? x))


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



(defmulti compare
  (fn [actual predicate]
    (let [actual-type    (get-type actual)
          predicate-type (get-type predicate)]
      (cond
        (= ::function predicate-type) ::function
        (simple-type? predicate-type) ::default
        :else [actual-type predicate-type]))))


(defmethod compare ::default
  [actual predicate]
  (when-not (= actual predicate)
    (fail actual predicate)))


(defmethod compare ::function
  [actual predicate]
  (when-not (and (boolean (predicate actual)) true)
    (fail actual predicate)))


(defmethod compare [::string ::regexp]
  [actual predicate]
  (when-not (re-find predicate actual)
    (fail actual predicate)))
