(ns warehouse-simulation.product-test
  (:require [clojure.test :refer :all]
            [warehouse-simulation.domain.product :refer :all]))

(deftest product-test-1 
    (is (= (:name (build-Product 1 "adsf" 123) "adsf"))))

(deftest product-test-2
    (is (= (:price (build-Product 2 "APPLE" 123) 123))))

(deftest product-test-3
    (is (thrown? AssertionError (build-Product "invalid, ids must be numbers" "adf" 123))))

(deftest product-test-4 
    (is (= (str-Product (build-Product 123 "Apple" 123)) "123 Apple 123")))

(deftest product-test-5
    (is (build-null-Product)))

(deftest product-test-6 
    (is (= (str-Product (build-null-Product)) "-1 NULL PRODUCT 0")))

(deftest product-test-7
    (is (Product? (build-Product 12 "ad" 12))))