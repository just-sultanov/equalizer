(ns equalizer.core-test
  (:require
    #?(:clj  [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer-macros [deftest is testing]])
    [equalizer.core :as sut]))

(deftest test-get-type
  (testing "::nil"
    (is (= ::sut/nil (sut/get-type nil))))


  (testing "::boolean"
    (is (= ::sut/boolean (sut/get-type true)))
    (is (= ::sut/boolean (sut/get-type false))))


  (testing "::number"
    (is (= ::sut/number (sut/get-type 1)))
    (is (= ::sut/number (sut/get-type 1.0)))
    #?(:clj (is (= ::sut/number (sut/get-type 1/2)))))


  (testing "::char"
    (is (= ::sut/char (sut/get-type \a))))


  (testing "::string"
    (is (= ::sut/string (sut/get-type "abc"))))


  (testing "::symbol"
    (is (= ::sut/symbol (sut/get-type 'a)))
    (is (= ::sut/symbol (sut/get-type 'a/b))))


  (testing "::keyword"
    (is (= ::sut/keyword (sut/get-type :a)))
    (is (= ::sut/keyword (sut/get-type :a/b))))


  (testing "::list"
    (is (= ::sut/list (sut/get-type '()))))


  (testing "::vector"
    (is (= ::sut/vector (sut/get-type []))))


  (testing "::set"
    (is (= ::sut/set (sut/get-type #{}))))


  (testing "::map"
    (is (= ::sut/map (sut/get-type {}))))


  (testing "::regexp"
    (is (= ::sut/regexp (sut/get-type #"\w+"))))


  (testing "::function"
    (is (= ::sut/function (sut/get-type odd?)))
    (is (= ::sut/function (sut/get-type #(some? %))))))



(deftest test-compare
  (testing "any pairs should be compared correctly"

    (testing "::nil"
      (testing "[::nil ::nil]"
        (is (sut/pass? (sut/compare nil nil))))

      (testing "[::nil ::function]"
        (is (sut/pass? (sut/compare nil nil?)))
        (is (sut/fail? (sut/compare nil some?))))

      (testing "[::nil ::sequential]"
        (is (every? sut/pass? (sut/compare nil (list nil nil?))))
        (is (some sut/fail? (sut/compare nil (list nil some?))))
        (is (every? sut/pass? (sut/compare nil (vector nil nil?))))
        (is (some sut/fail? (sut/compare nil (vector nil some?)))))

      (testing "[::nil _]"
        (is (->> [1 1.0 #?(:clj 1/2) true false \a "abc" 'a 'a/b :a :a/b]
              (map #(sut/compare nil %))
              (every? sut/fail?)))))


    (testing "::boolean"
      (testing "[::boolean ::boolean]"
        (is (sut/pass? (sut/compare true true)))
        (is (sut/pass? (sut/compare false false)))
        (is (sut/fail? (sut/compare true false)))
        (is (sut/fail? (sut/compare false true))))

      (testing "[::boolean ::function]"
        (is (sut/pass? (sut/compare true true?)))
        (is (sut/pass? (sut/compare false false?)))
        (is (sut/pass? (sut/compare true boolean?)))
        (is (sut/fail? (sut/compare true false?)))
        (is (sut/fail? (sut/compare false true?))))

      (testing "[::boolean ::sequential]"
        (is (every? sut/pass? (sut/compare true (list true true? boolean?))))
        (is (every? sut/pass? (sut/compare false (list false false? boolean?))))
        (is (every? sut/pass? (sut/compare true (vector true true? boolean?))))
        (is (every? sut/pass? (sut/compare false (vector false false? boolean?))))
        (is (some sut/fail? (sut/compare true (list true false? boolean?))))
        (is (some sut/fail? (sut/compare false (list false true? boolean?))))
        (is (some sut/fail? (sut/compare true (vector true false? boolean?))))
        (is (some sut/fail? (sut/compare false (vector false true? boolean?)))))

      (testing "[::boolean _]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) \a "abc" 'a 'a/b :a :a/b]
              (map #(sut/compare true %))
              (every? sut/fail?)))))


    (testing "::number"
      (testing "[::number ::number]"
        (is (sut/pass? (sut/compare 1 1)))
        (is (sut/pass? (sut/compare 1.0 1.0)))
        #?(:clj (is (sut/pass? (sut/compare 1/2 1/2))))
        #?(:clj (is (sut/fail? (sut/compare 1 1/2))))
        #?(:clj (is (sut/fail? (sut/compare 1.0 1/2))))
        #?(:clj (is (sut/fail? (sut/compare 1/2 1))))
        #?(:clj (is (sut/fail? (sut/compare 1/2 1.0)))))

      (testing "[::number ::function]"
        (is (sut/pass? (sut/compare 1 pos-int?)))
        (is (sut/pass? (sut/compare 0 zero?)))
        (is (sut/pass? (sut/compare 1.0 float?)))
        #?(:clj (is (sut/pass? (sut/compare 1/2 ratio?))))
        (is (sut/fail? (sut/compare 1 neg-int?)))
        #?(:clj (is (sut/fail? (sut/compare 1/2 neg?)))))

      (testing "[::number ::sequential]"
        (is (every? sut/pass? (sut/compare 1 (list 1 pos-int?))))
        (is (every? sut/pass? (sut/compare 0 (list 0 zero?))))
        (is (every? sut/pass? (sut/compare 1.0 (list 1.0 pos?))))
        #?(:clj (is (every? sut/pass? (sut/compare 1/2 (list 1/2 ratio?)))))
        (is (some sut/fail? (sut/compare 1 (list 1 neg-int?))))
        #?(:clj (is (some sut/fail? (sut/compare 1/2 (list 1/2 neg?)))))
        (is (every? sut/pass? (sut/compare 1 (vector 1 pos-int?))))
        (is (every? sut/pass? (sut/compare 0 (vector 0 zero?))))
        (is (every? sut/pass? (sut/compare 1.0 (vector 1.0 pos?))))
        #?(:clj (is (every? sut/pass? (sut/compare 1/2 (vector 1/2 ratio?)))))
        (is (some sut/fail? (sut/compare 1 (vector 1 neg-int?))))
        #?(:clj (is (some sut/fail? (sut/compare 1/2 (vector 1/2 neg?))))))

      (testing "[::number _]"
        (is (->> [nil #?(:clj 1/2) \a "abc" 'a 'a/b :a :a/b]
              (map #(sut/compare 1 %))
              (every? sut/fail?)))))


    (testing "::char"
      (testing "[::char ::char]"
        (is (sut/pass? (sut/compare \a \a))))

      (testing "[::char ::function]"
        (is (sut/pass? (sut/compare \a char?)))
        #?(:clj (is (sut/fail? (sut/compare \a string?)))))

      (testing "[::char ::sequential]"
        (is (every? sut/pass? (sut/compare \a (list \a char?))))
        (is (some sut/fail? (sut/compare \a (list \a nil?))))
        (is (every? sut/pass? (sut/compare \a (vector \a char?))))
        (is (some sut/fail? (sut/compare \a (vector \a nil?)))))

      (testing "[::char _]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) true false "abc" 'a 'a/b :a :a/b]
              (map #(sut/compare \a %))
              (every? sut/fail?)))))


    (testing "::string"
      (testing "[::string ::string]"
        (is (sut/pass? (sut/compare "abc" "abc"))))

      (testing "::string ::regexp"
        (is (sut/pass? (sut/compare "abc" #"\w+")))
        (is (sut/fail? (sut/compare "abc" #"\d+"))))

      (testing "[::string ::function]"
        (is (sut/pass? (sut/compare "abc" string?)))
        (is (sut/fail? (sut/compare "abc" char?))))

      (testing "[::char ::sequential]"
        (is (every? sut/pass? (sut/compare "abc" (list "abc" string?))))
        (is (some sut/fail? (sut/compare "abc" (list "abc" nil?))))
        (is (every? sut/pass? (sut/compare "abc" (vector "abc" string?))))
        (is (some sut/fail? (sut/compare "abc" (vector "abc" nil?)))))

      (testing "[::string _]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) true false \a 'a 'a/b :a :a/b]
              (map #(sut/compare "abc" %))
              (every? sut/fail?)))))


    (testing "::symbol"
      (testing "[::symbol ::symbol]"
        (is (sut/pass? (sut/compare 'a 'a)))
        (is (sut/pass? (sut/compare 'a/b 'a/b))))

      (testing "[::symbol ::function]"
        (is (sut/pass? (sut/compare 'a symbol?)))
        (is (sut/pass? (sut/compare 'a/b qualified-symbol?)))
        (is (sut/fail? (sut/compare 'a qualified-symbol?)))
        (is (sut/fail? (sut/compare 'a/b simple-symbol?))))

      (testing "[::symbol ::sequential]"
        (is (every? sut/pass? (sut/compare 'a (list 'a simple-symbol?))))
        (is (some sut/fail? (sut/compare 'a/b (list 'a/b simple-symbol?))))
        (is (every? sut/pass? (sut/compare 'a (vector 'a simple-symbol?))))
        (is (some sut/fail? (sut/compare 'a/b (vector 'a/b simple-symbol?)))))

      (testing "[::symbol _]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) true false \a 'a/b :a :a/b]
              (map #(sut/compare 'a %))
              (every? sut/fail?)))))


    (testing "::keyword"
      (testing "[::keyword ::keyword]"
        (is (sut/pass? (sut/compare :a :a)))
        (is (sut/pass? (sut/compare :a/b :a/b))))

      (testing "[::keyword ::function]"
        (is (sut/pass? (sut/compare :a keyword?)))
        (is (sut/pass? (sut/compare :a/b qualified-keyword?)))
        (is (sut/fail? (sut/compare :a qualified-keyword?)))
        (is (sut/fail? (sut/compare :a/b simple-keyword?))))

      (testing "[::keyword ::sequential]"
        (is (every? sut/pass? (sut/compare :a (list :a simple-keyword?))))
        (is (some sut/fail? (sut/compare :a/b (list :a/b simple-keyword?))))
        (is (every? sut/pass? (sut/compare :a (vector :a simple-keyword?))))
        (is (some sut/fail? (sut/compare :a/b (vector :a/b simple-keyword?)))))

      (testing "[::keyword _]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) true false \a 'a 'a/b :a/b]
              (map #(sut/compare :a %))
              (every? sut/fail?)))))))
