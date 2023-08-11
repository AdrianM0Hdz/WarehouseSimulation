; using recursive descent we create a main function that parses a scirpt in an input file route and
; writes output to file
(ns warehouse-simulation.application.warehouse-script-parser 
  (:require [warehouse-simulation.domain.product :refer :all]
            [warehouse-simulation.domain.container :refer :all]
            [warehouse-simulation.domain.warehouse :refer :all]))

; all acitons 

(defn write-serialized-warehouse-to-file [output-path warehouse]
    nil)

(def parse-command-body)

(defn parse-add-to-catalogue [warehouse tokens]
    (let [ product-id (Integer. (nth tokens 0))
           name (nth tokens 1)
           price (Integer. (nth tokens 2))
           product (build-Product product-id name price)]
        (parse-command-body (assoc (add-product-to-Warehouse-catalogue warehouse product)
                                    :movement-history
                                    (conj (:movement-history (add-product-to-Warehouse-catalogue warehouse product))
                                          "ADDED PRODUCT TO CATALOGUE CORRECTLY"))
                            (vec (rest (rest (rest tokens)))))))

(defn parse-product-deposit [warehouse tokens]
    (let [ 
            product-name (if (= (nth tokens 0) "") 
                                nil
                                (nth tokens 0))
            quantity (Integer. (nth tokens 1))]
        (parse-command-body (assoc (add-product-to-Warehouse warehouse product-name quantity)
                                    :movement-history
                                    (conj (:movement-history (add-product-to-Warehouse warehouse product-name quantity))
                                        "PRODUCT DEPOSITED CORRECTLY"))
                            (vec (rest (rest tokens))))))

(defn parse-product-withdrawl [warehouse tokens]
    (let [
            product-name (if (= (nth tokens 0) "") 
                            nil
                            (nth tokens 0))
            quantity (Integer. (nth tokens 1))]
        (parse-command-body (withdraw-product-to-Warehouse warehouse product-name quantity)
                            (vec (rest (rest tokens))))))

(defn parse-move [warehouse tokens]
    (let [move (first tokens)]
        (case move
            "UP" (parse-move (move-up-Warehouse warehouse) (vec (rest tokens)))
            "DOWN" (parse-move (move-down-Warehouse warehouse) (vec (rest tokens)))
            "LEFT" (parse-move (move-left-Warehouse warehouse) (vec (rest tokens)))
            "RIGHT" (parse-move (move-right-Warehouse warehouse) (vec (rest tokens)))
            "END-MOVE" (parse-command-body  (assoc warehouse 
                                                   :movement-history 
                                                   (conj (:movement-history warehouse) 
                                                         (str
                                                            "MOVEMENT SEQUENCE ENDED AT FOLLOWING CONTAINER \n" 
                                                            (str-Container (get-current-container warehouse))))) 
                                            (vec (rest tokens))))))

(defn parse-command-body [warehouse tokens]
    (let [command-type (first tokens)] 
        (case command-type 
            "PRODUCT" (parse-add-to-catalogue (assoc warehouse
                                                     :movement-history
                                                     (conj (:movement-history warehouse) "ADDING PRODUCT TO WAREHOUSE CATALOGUE")) 
                                              (vec (rest tokens)))
            "DEPOSIT-PRODUCT" (parse-product-deposit (assoc warehouse
                                                            :movement-history
                                                            (conj (:movement-history warehouse) "DEPOSITING PRODUCT TO WAREHOUSE")) 
                                                     (vec (rest tokens)))
            "WITHDRAW-PRODUCT" (parse-product-withdrawl (assoc warehouse
                                                               :movement-history
                                                               (conj (:movement-history warehouse) "WITHDRAWING PRODUCT TO WAREHOUSE")) 
                                                        (vec (rest tokens)))
            "MOVE" (parse-move (assoc warehouse
                                      :movement-history
                                      (conj (:movement-history warehouse) "STARTING SEQUENCE OF MOVEMENTS")) 
                               (vec (rest tokens)))
            "<EOF>" (get-scarce-products-of-Warehouse
                        (get-total-value-of-Warehouse warehouse)))))

; returns warehouse after all acitons performed
(defn parse-program [tokens]
    (let [ 
         rows (Integer. (nth tokens 0))
         cols (Integer. (nth tokens 1))
         min-threshold (Integer. (nth tokens 2))
         warehouse (build-empty-Warehouse rows cols min-threshold)]
        (parse-command-body warehouse (vec (rest (rest (rest tokens)))))))

; entrypoint method with recursive descent to parse script and
; write output and returns warehouse object 
(defn parse-script [script-path output-path]
    (let [final-warehouse (parse-program (conj (with-open [reader (clojure.java.io/reader script-path)]
                            (reduce conj [] (line-seq reader))) "<EOF>"))]
        (spit output-path (serialize-Warehouse final-warehouse))
        final-warehouse))
