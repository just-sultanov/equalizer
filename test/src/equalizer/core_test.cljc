(ns equalizer.core-test
  (:require
    #?(:clj  [clojure.test :refer [deftest is testing]]
       :cljs [cljs.test :refer-macros [deftest is testing]])
    [equalizer.core :as sut]))

(deftest test-get-type
  (testing "any types should be resolved correctly"
    (testing "resolve ::nil"
      (is (= ::sut/nil (sut/get-type nil))))

    (testing "resolve ::boolean"
      (is (= ::sut/boolean (sut/get-type true)))
      (is (= ::sut/boolean (sut/get-type false))))

    (testing "resolve ::number"
      (is (= ::sut/number (sut/get-type 1)))
      (is (= ::sut/number (sut/get-type 1.0)))
      #?(:clj (is (= ::sut/number (sut/get-type 1/2)))))

    (testing "resolve ::char"
      (is (= ::sut/char (sut/get-type \a))))

    (testing "resolve ::string"
      (is (= ::sut/string (sut/get-type "abc"))))

    (testing "resolve ::symbol"
      (is (= ::sut/symbol (sut/get-type 'a)))
      (is (= ::sut/symbol (sut/get-type 'a/b))))

    (testing "resolve ::keyword"
      (is (= ::sut/keyword (sut/get-type :a)))
      (is (= ::sut/keyword (sut/get-type :a/b))))

    (testing "resolve ::list"
      (is (= ::sut/list (sut/get-type '()))))

    (testing "resolve ::vector"
      (is (= ::sut/vector (sut/get-type []))))

    (testing "resolve ::set"
      (is (= ::sut/set (sut/get-type #{}))))

    (testing "resolve ::map"
      (is (= ::sut/map (sut/get-type {}))))

    (testing "resolve ::regexp"
      (is (= ::sut/regexp (sut/get-type #"\w+"))))


    (testing "resolve ::function"
      (is (= ::sut/function (sut/get-type odd?)))
      (is (= ::sut/function (sut/get-type #(some? %)))))))



(deftest test-get-paths
  (testing ""
    (is (= [[[:a] 1] [[:b :c] 2] [[:b :d] 3] [[:b :e :f] 4]]
          (sut/get-paths {:a 1, :b {:c 2, :d 3, :e {:f 4}}})))

    (is (= [[["a"] 1] [["b" "c"] 2] [["b" "d"] 3] [["b" "e" "f"] 4]]
          (sut/get-paths {"a" 1, "b" {"c" 2, "d" 3, "e" {"f" 4}}})))

    (is (= [[[:a] nil?] [[:b :c] string?] [[:b :d] number?] [[:b :e :f] any?]]
          (sut/get-paths {:a nil?, :b {:c string?, :d number?, :e {:f any?}}})))))



(deftest test-compare
  (testing "any pairs should be compared correctly"

    (testing "compare ::nil"
      (testing "[::nil ::nil]"
        (is (sut/pass? (sut/compare nil nil))))

      (testing "[::nil ::function]"
        (is (sut/pass? (sut/compare nil nil?)))
        (is (sut/fail? (sut/compare nil some?))))

      (testing "[::nil ::any]"
        (is (->> [1 1.0 #?(:clj 1/2) true false \a "abc" #"\w+" 'a 'a/b :a :a/b '() [] #{} {}]
              (map #(sut/compare nil %))
              (every? sut/fail?)))))


    (testing "compare ::boolean"
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

      (testing "[::boolean ::any]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) false \a "abc" #"\w+" 'a 'a/b :a :a/b '() [] #{} {}]
              (map #(sut/compare true %))
              (every? sut/fail?)))))


    (testing "compare ::number"
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

      (testing "[::number ::any]"
        (is (->> [nil 2 2.0 #?(:clj 1/2) \a "abc" #"\w+" 'a 'a/b :a :a/b '() [] #{} {}]
              (map #(sut/compare 1 %))
              (every? sut/fail?)))))


    (testing "compare ::char"
      (testing "[::char ::char]"
        (is (sut/pass? (sut/compare \a \a))))

      (testing "[::char ::function]"
        (is (sut/pass? (sut/compare \a char?)))
        #?(:clj (is (sut/fail? (sut/compare \a string?)))))

      (testing "[::char ::any]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) true false \b "abc" #"\w+" 'a 'a/b :a :a/b '() [] #{} {}]
              (map #(sut/compare \a %))
              (every? sut/fail?)))))


    (testing "compare ::string"
      (testing "[::string ::string]"
        (is (sut/pass? (sut/compare "abc" "abc"))))

      (testing "[::string ::regexp]"
        (is (sut/pass? (sut/compare "abc" #"\w+")))
        (is (sut/fail? (sut/compare "abc" #"\d+"))))

      (testing "[::string ::function]"
        (is (sut/pass? (sut/compare "abc" string?)))
        (is (sut/fail? (sut/compare "abc" char?))))

      (testing "[::string ::any]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) true false \a "cba" #"\d+" 'a 'a/b :a :a/b '() [] #{} {}]
              (map #(sut/compare "abc" %))
              (every? sut/fail?)))))


    (testing "compare ::symbol"
      (testing "[::symbol ::symbol]"
        (is (sut/pass? (sut/compare 'a 'a)))
        (is (sut/pass? (sut/compare 'a/b 'a/b))))

      (testing "[::symbol ::function]"
        (is (sut/pass? (sut/compare 'a symbol?)))
        (is (sut/pass? (sut/compare 'a/b qualified-symbol?)))
        (is (sut/fail? (sut/compare 'a qualified-symbol?)))
        (is (sut/fail? (sut/compare 'a/b simple-symbol?))))

      (testing "[::symbol ::any]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) true false \a "abc" #"\w+" 'b 'a/b :a :a/b '() [] #{} {}]
              (map #(sut/compare 'a %))
              (every? sut/fail?)))))


    (testing "compare ::keyword"
      (testing "[::keyword ::keyword]"
        (is (sut/pass? (sut/compare :a :a)))
        (is (sut/pass? (sut/compare :a/b :a/b))))

      (testing "[::keyword ::function]"
        (is (sut/pass? (sut/compare :a keyword?)))
        (is (sut/pass? (sut/compare :a/b qualified-keyword?)))
        (is (sut/fail? (sut/compare :a qualified-keyword?)))
        (is (sut/fail? (sut/compare :a/b simple-keyword?))))

      (testing "[::keyword ::any]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) true false \a "abc" #"\w+" 'a 'a/b :b :a/b '() [] #{} {}]
              (map #(sut/compare :a %))
              (every? sut/fail?)))))


    (testing "compare ::list"
      (testing "[::list ::list]"
        (is (sut/pass? (sut/compare '() '())))
        (is (sut/pass? (sut/compare '(1 2 3) '(1 2 3)))))

      (testing "[::list ::function]"
        (is (sut/pass? (sut/compare '() list?)))
        (is (sut/pass? (sut/compare '() empty?)))
        (is (sut/fail? (sut/compare '() nil?)))
        (is (sut/fail? (sut/compare '() not-empty))))

      (testing "[::list ::any]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) true false \a "abc" #"\w+" 'a 'a/b :a :a/b '() [] #{} {}]
              (map #(sut/compare '(:a) %))
              (every? sut/fail?)))))

    (testing "compare ::vector"
      (testing "[::vector ::vector]"
        (is (sut/pass? (sut/compare [] [])))
        (is (sut/pass? (sut/compare [1 2 3] [1 2 3]))))

      (testing "[::vector ::function]"
        (is (sut/pass? (sut/compare [] vector?)))
        (is (sut/pass? (sut/compare [] empty?)))
        (is (sut/fail? (sut/compare [] nil?)))
        (is (sut/fail? (sut/compare [] not-empty))))

      (testing "[::vector ::any]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) true false \a "abc" #"\w+" 'a 'a/b :a :a/b '() [] #{} {}]
              (map #(sut/compare [:a] %))
              (every? sut/fail?)))))

    (testing "compare ::set"
      (testing "[::set ::set]"
        (is (sut/pass? (sut/compare #{} #{})))
        (is (sut/pass? (sut/compare #{1 2 3} #{1 2 3}))))

      (testing "[::set ::function]"
        (is (sut/pass? (sut/compare #{} set?)))
        (is (sut/pass? (sut/compare #{} empty?)))
        (is (sut/fail? (sut/compare #{} nil?)))
        (is (sut/fail? (sut/compare #{} not-empty))))

      (testing "[::set ::any]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) true false \a "abc" #"\w+" 'a 'a/b :a :a/b '() [] #{} {}]
              (map #(sut/compare #{:a} %))
              (every? sut/fail?)))))

    (testing "compare ::map"
      (testing "[::map ::map]"
        (is (sut/pass? (sut/compare {} {})))
        (is (sut/pass? (sut/compare {:a 1, :b 2} {:a 1, :b 2}))))

      (testing "[::map ::function]"
        (is (sut/pass? (sut/compare {} map?)))
        (is (sut/pass? (sut/compare {} empty?)))
        (is (sut/fail? (sut/compare {} nil?)))
        (is (sut/fail? (sut/compare {} not-empty)))
        (is (every? sut/pass? (sut/compare {:a 1, :b 2} {:a 1, :b number?})))
        (is (some sut/fail? (sut/compare {:a 1, :b 2} {:a 1, :c number?}))))

      (testing "[::map ::any]"
        (is (->> [nil 1 1.0 #?(:clj 1/2) true false \a "abc" #"\w+" 'a 'a/b :a :a/b '() [] #{} {}]
              (map #(sut/compare {:a 1} %))
              (every? sut/fail?)))))))
