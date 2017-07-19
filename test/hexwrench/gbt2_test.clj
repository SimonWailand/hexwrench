(ns hexwrench.gbt2-test
  (:require [clojure.test :refer :all]
            [hexwrench.gbt2 :refer :all]))

(deftest inv-test
  (testing "Inverting a GBT2 address."
    (is (= [6 5 4 3 2 1 0] (inv [1 2 3 4 5 6 0])))))

(deftest int->vec-test
  (testing "Converting an integer to a GBT2 sequence."
    (is (= [1 2 3 4 5 6 0] (int->vec 7r1234560)))))

(deftest vec->int-test
  (testing "Converting a GBT2 sequence to an integer."
    (is (= 7r6543210 (vec->int [6 5 4 3 2 1 0])))))

(deftest add-test
  (testing "Adding some GBT2 addresses"
    (is (= [1 2] (add [1] [1])))))