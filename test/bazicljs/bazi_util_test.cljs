(ns bazicljs.bazi-util-test
  (:require [cljs.test :refer-macros [deftest testing is]]
            [bazicljs.bazi-util :as bu]))

(deftest stem-polarity
  (testing "a"
    (is (= (bu/stem-polarity 0) 0))
    (is (= (bu/stem-polarity 1) 1))))

(deftest branch-polarity
  (testing ""
    (is (= (bu/branch-polarity 0) 0))
    (is (= (bu/branch-polarity 9) 1))))

(deftest stem-god
  (testing ""
    (is (= (bu/stem-god 9 0) 3))
    ))

(deftest branch-god
  (testing ""
    (is (= (bu/branch-god 11 0) 3))
    ))
