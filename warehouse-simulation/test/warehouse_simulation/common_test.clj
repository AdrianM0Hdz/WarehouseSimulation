(ns warehouse-simulation.common-test
  (:require [clojure.test :refer :all]
            [warehouse-simulation.domain.common :refer :all]))

(deftest common-filter-value-test-1
  (is (= 21 (filter-value 21 number?))))

(deftest common-filter-value-test-2
  (is (thrown? AssertionError (filter-value "123" number?))))