(ns equalizer.core-test
  (:require
    #?(:clj  [clojure.test :refer [deftest is]]
       :cljs [cljs.test :refer-macros [deftest is]])
    [equalizer.core :as sut]))

(deftest test-square
  (is (= 1 (sut/square 1)))
  (is (= 4 (sut/square 2)))
  (is (= 9 (sut/square 3)))
  (is (= 16 (sut/square 4))))
