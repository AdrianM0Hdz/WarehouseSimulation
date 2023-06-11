(ns warehouse-simulation.container-test
     (:require [clojure.test :refer :all]
               [warehouse-simulation.domain.common :refer [filter-value]]
               [warehouse-simulation.domain.product :refer :all]
               [warehouse-simulation.domain.container :refer :all]))

(deftest container-test-1 
    (is (build-Container 0 0 (build-null-Product) 0)))

(deftest container-test-2 
    (is (Container? (build-empty-Container 0 0))))

(deftest container-test-3
    (is (thrown? AssertionError (build-empty-Container "aad" "aass"))))

(deftest container-test-4 
     (let [container (build-empty-Container 1 23)]
          (is (= (:row container) 1))
          (is (= (:col container) 23))
          (is (= (:product container) (build-null-Product)))))

(deftest container-test-5 
     (let [container (build-empty-Container 0 0)]
          (is (= 
               (str-Product (:product 
                                   (set-Container-product container (build-Product 1 "apple" 12)))) 
               "1 apple 12"))))

(deftest container-test-6
     (let [container (deposit-Container-product (build-empty-Container 0 0) 10)]
          (is (= (:quantity container) 10))))

(deftest container-test-7
     (let [container (build-empty-Container 0 0)]
          (is (= (get-Container-total-value container) 0))))

(deftest contianer-test-8 
     (let [container  (deposit-Container-product 
                         (set-Container-product 
                              (build-empty-Container 0 0)
                              (build-Product 12 "ad" 100))
                         10)]
          (is (= (get-Container-total-value container) 1000))))

(deftest container-test-10 
     (let [container (build-empty-Container 0 0)]
          (is (= (str-Container container) "ROW: 0 COL: 0 PRODUCT: -1 NULL PRODUCT 0 QUANTITY: 0"))))