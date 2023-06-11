(ns warehouse-simulation.domain.product
    (:require [warehouse-simulation.domain.common :refer [filter-value]]))

(defrecord Product
    [id
     name 
     price])

(defn build-Product [id name price] 
    (Product. (filter-value id  number?) 
              (filter-value name string?) 
              (filter-value price number?)))

(defn build-null-Product []
    (Product. -1
              "NULL PRODUCT"
              0))

(defn Product? [item]
    (= (type item) Product))

(defn str-Product [product]
    (str (:id product) " " (:name product) " " (:price product)))

