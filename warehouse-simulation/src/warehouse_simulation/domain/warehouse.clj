(ns warehouse-simulation.domain.warehouse
    (:require [warehouse-simulation.domain.common :refer [filter-value]]
              [warehouse-simulation.domain.product :refer :all]
              [warehouse-simulation.domain.container :refer :all]))

(defrecord Coords [row col])

(defn build-Coords [row col]
    (Coords. row col))


(defrecord Warehouse
    [n-rows 
     n-cols
     cur-row 
     cur-col 
     catalogue ; map of product name to coords
     rows 
     low-quantity-treshold])

(defn build-empty-row [n-row n-cols] 
    (vec (map (fn [col] (build-empty-Container n-row col)) (vec (range 1 (+ n-cols 1))))))

(defn _build-rows [cur-row n-rows n-cols]
    (if (= cur-row 1)
        [(build-empty-row cur-row n-cols)]    
        (conj (_build-rows (- cur-row 1) n-rows n-cols) (build-empty-row cur-row n-cols))))

(defn build-rows [n-rows n-cols]
    (_build-rows n-rows n-rows n-cols))

; for logging 
(defn str-row [row]
    (if (> (count row) 0)
        (str (str-Container (first row)) "\n" 
             (str-row (rest row)))))

(defn str-rows [rows]
    (if (> (count rows) 0)
        (str (str-row (first rows)) (str-rows (rest rows)))))

(defn get-next-row [row n-rows]
    (if (= row n-rows)
        1
        (+ row 1)))

(defn get-previous-row [row n-rows]
    (if (= row 1)
        n-rows
        (- row 1)))

; just for the build-catalougue function
(defn get-next-col [col n-cols] 
    (if (= col n-cols)
        1 
        (+ col 1)))

(defn rotate-up-rows [rows]
    (conj (vec (rest rows)) (first rows)))

(defn rotate-down-rows [rows]
    (into [(last rows)] (pop rows)))

(defn get-next-coord [coord n-rows n-cols]
    (if (= (:col coord) n-cols)
        (Coords. (get-next-row (:row coord) n-rows) 
                1)
        (Coords. (:row coord) 
                (get-next-col (:col coord) n-cols))))

; algo sucio pero aun asi funciona de forma clara
; TESTED
; DEPRECATED
;(defn build-catalogue [catalogue product-list cur-coord n-rows n-cols] 
;    (if (= (count product-list) 0)
;        catalogue
;        (build-catalogue (assoc catalogue (:name (first product-list)) cur-coord) 
;                         (rest product-list) 
;                         (get-next-coord cur-coord n-rows n-cols)
;                         n-rows
;                         n-cols)))

(defn build-empty-Warehouse 
    [n-rows n-cols low-quantity-treshold]
    (Warehouse. n-rows 
                n-cols 
                1 
                1 
                {"empty-product" (build-Coords 0 n-cols)}
                (build-rows n-rows n-cols)
                low-quantity-treshold))

(defn move-up-Warehouse [warehouse]  
    (assoc warehouse :cur-row (get-next-row (:cur-row warehouse) (:n-rows warehouse))
                     :rows (rotate-up-rows (:rows warehouse))))

(defn move-down-Warehouse [warehouse] 
    (assoc warehouse :cur-row (get-previous-row (:cur-row warehouse) (:n-rows warehouse))
                     :rows (rotate-down-rows (:rows warehouse))))

(defn move-left-Warehouse [warehouse]
    (if (= (:cur-col warehouse) 1)
        (throw (Exception. "invalid movement"))
        (assoc warehouse :cur-col (- (:cur-col warehouse) 1))))

(defn move-right-Warehouse [warehouse]
    (if (= (:cur-col warehouse) (:n-cols warehouse))
        (throw (Exception. "invalid movement"))
        (assoc warehouse :cur-col (+ (:cur-col warehouse) 1))))

(defn fetch-current-container [warehouse]
    (nth (first (:rows warehouse)) (- (:cur-col warehouse) 1)))


; movements to reach row mooving up
; TESTED
(defn get-up-row-distance [warehouse row]
    (if (< (:cur-row warehouse) row)
        (- row (:cur-row warehouse))
        (+ (- (:n-rows warehouse) (:cur-row warehouse)) row)))

; movements to reach row moving down
; TESTED
(defn get-down-row-distance [warehouse row]
    (if (> (:cur-row warehouse) row)
        (- (:cur-row warehouse) row)
        (+ (- (:n-rows warehouse) row) (:cur-row warehouse))))

; return a function which is the most optimal operator to apply to reach 
; the dsiered row
; TESTED
(defn get-optimal-row-movement-Warehouse [warehouse row]
    (if (< (get-up-row-distance warehouse row)
           (get-down-row-distance warehouse row))
        move-up-Warehouse
        move-down-Warehouse))

; TESTED
(defn move-to-row-position-Warehouse [warehouse row]
    (if (or (< row 1)
            (> row (:n-rows warehouse)))
        (throw (AssertionError. "row out of range"))
        (if (= (:cur-row warehouse) row) 
            warehouse 
            (move-to-row-position-Warehouse ((get-optimal-row-movement-Warehouse warehouse row) warehouse) 
                                            row))))
; TESTED
(defn get-optimal-col-movement-Warehouse [warehouse col]
    (if (> (:cur-col warehouse) col)
        move-left-Warehouse 
        move-right-Warehouse))

; TESTED
(defn move-to-col-position-Warehouse [warehouse col]
    (if (or (< (:n-cols warehouse) col) 
               (< col 1)) ; check that col is in range
        (throw (AssertionError. "column out of bounds"))
        (if (= (:cur-col warehouse) col)
            warehouse
            (move-to-col-position-Warehouse ((get-optimal-col-movement-Warehouse warehouse col) warehouse)
                                            col))))

; TESTED
(defn move-to-position-Warehouse [warehouse row col]
    (move-to-row-position-Warehouse 
        (move-to-col-position-Warehouse warehouse col)
        row))

(defn replace-container-at-coords [warehouse contianer coords]
    nil)

(defn get-container-at-coords [warehouse contianer coords]
    nil)

; ADDS PRODUCT TO CATALOGUE and maps it to the next unoccupied coordinate
; directly accessing the rows as an array with nth function
; NOTE: ONLY VALID AT THE BEGGINING OF THE SCRIPT since row mutates at execution
(defn add-product-to-Warehouse [warehouse product]
    (let [product-coords (get-next-coord (last (last (:catalogue warehouse))) 
                                         (:n-rows warehouse) 
                                         (:n-cols warehouse))]
        (assoc warehouse
            :catalogue (assoc (:catalogue warehouse) 
                              (:name product) 
                              product-coords)              
            :rows (assoc (:rows warehouse)
                         (- (:row product-coords) 1)
                         (assoc (nth (:rows warehouse) (-(:row product-coords) 1))
                                (- (:col product-coords) 1) 
                                (assoc (get-in (:rows warehouse) [(- (:row product-coords) 1) 
                                                                  (- (:col product-coords) 1)]) 
                                       :product 
                                       product))))))

(defn withdraw-at-coords-Warehouse [warehouse coords])

(defn withdraw-product-to-Warehouse [warehouse product-name])