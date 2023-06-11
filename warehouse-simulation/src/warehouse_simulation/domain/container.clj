(ns warehouse-simulation.domain.container
     (:require [warehouse-simulation.domain.common :refer [filter-value]]
               [warehouse-simulation.domain.product :refer :all]))

(defrecord Container
    [row
     col 
     product
     quantity])

(defn build-Container [row col product quantity]
    (Container. (filter-value row number?)
                (filter-value col number?)
                (filter-value product Product?)
                (filter-value quantity number?)))

(defn build-empty-Container [row col]
    (Container. (filter-value row number?)
                (filter-value col number?)
                ( build-null-Product ) 
                0))

(defn Container? [item]
    (= (type item) Container))

(defn set-Container-product [container product]
    (if ( and (= (type container) Container)
              (Product? product)) 
        (assoc container :product product)
        (throw (AssertionError. "arguments of invalid type"))))

(defn deposit-Container-product [container quantity-to-add]
    (if (and (= (type container) Container)
             (Product? (get container :product)))
        (assoc container :quantity (+ (get container :quantity) quantity-to-add))
        (throw (AssertionError. "arguments of invalid type"))))

(defn get-Container-total-value [container]
    (* (get-in container [:product :price]) (get container :quantity)))

(defn str-Container [container]
    (str "ROW: " (:row container) " COL: " (:col container) " PRODUCT: " (str-Product (:product container)) " QUANTITY: " (:quantity container)))
