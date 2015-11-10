(ns hexwrench.gbt2-test
  (:require [clojure.test :refer :all]
            [hexwrench.gbt2 :refer :all]))

(deftest inv-test
  (testing "Inverting a GBT2 address."
    (is (= 7r6543210 (inv 7r1234560)))))
