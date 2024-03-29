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
     low-quantity-treshold
     movement-history])

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
                low-quantity-treshold
                []))

(defn move-up-Warehouse [warehouse]  
    (assoc warehouse :cur-row (get-next-row (:cur-row warehouse) (:n-rows warehouse))
                     :rows (rotate-up-rows (:rows warehouse))
                     :movement-history (conj (:movement-history warehouse) "UP MOVE")))

(defn move-down-Warehouse [warehouse] 
    (assoc warehouse :cur-row (get-previous-row (:cur-row warehouse) (:n-rows warehouse))
                     :rows (rotate-down-rows (:rows warehouse))
                     :movement-history (conj (:movement-history warehouse) "DOWN MOVE")))

(defn move-left-Warehouse [warehouse]
    (if (= (:cur-col warehouse) 1)
        (assoc warehouse :movement-history (conj (:movement-history warehouse) "INVALID LEFT MOVE"))
        (assoc warehouse :cur-col (- (:cur-col warehouse) 1)
                         :movement-history (conj (:movement-history warehouse) "LEFT MOVE"))))

(defn move-right-Warehouse [warehouse]
    (if (= (:cur-col warehouse) (:n-cols warehouse))
        (assoc warehouse :movement-history (conj (:movement-history warehouse) "INVALID RIGHT MOVE"))
        (assoc warehouse :cur-col (+ (:cur-col warehouse) 1)
                         :movement-history (conj (:movement-history warehouse) "RIGHT MOVE"))))

; TESTED
(defn get-current-container [warehouse]
    (nth (first (:rows warehouse)) (- (:cur-col warehouse) 1)))

; TESTED
(defn replace-current-container [warehouse container]
    (assoc-in warehouse 
           [:rows 0 (- (:cur-col warehouse) 1)]
           container))

; TESTED
(defn get-index-of-row [warehouse row]
    (if (< row (get-in warehouse [:rows 0 0 :row]))
        (- (+ (:n-rows warehouse) row) (get-in warehouse [:rows 0 0 :row]))
        (- row (get-in warehouse [:rows 0 0 :row]))))

; to access without triggering movements 
; and calculate total value of stock also stokc 
; TESTED
(defn get-container-at-coords [warehouse coords]
    (get-in warehouse 
            [:rows (get-index-of-row warehouse (:row coords)) (- (:col coords) 1)]))

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

; ADDS PRODUCT TO CATALOGUE and maps it to the next unoccupied coordinate
; directly accessing the rows as an array with nth function
; NOTE: ONLY VALID AT THE BEGGINING OF THE SCRIPT since row mutates at execution
; NOTE: refactor with assoc-in
(defn add-product-to-Warehouse-catalogue [warehouse product]
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

; move to coords and withdraw current product
(defn withdraw-product-at-coords-Warehouse [warehouse coords quantity]
    (let [moved-warehouse (move-to-position-Warehouse warehouse 
                                                      (:row coords)
                                                      (:col coords))
          not-enough (< (:quantity (get-current-container moved-warehouse)) quantity)]               
            (assoc (replace-current-container moved-warehouse
                                       (assoc (get-current-container moved-warehouse)
                                              :quantity
                                              (max (- (:quantity (get-current-container moved-warehouse))
                                                      quantity) 
                                                   0)))
                    :movement-history
                    (conj (:movement-history moved-warehouse)
                          (if not-enough
                            "NOT ENOUGH PRODUCT TO WITHDRAW"
                            "WITHDRAWL DONE CORRECTLY")))))

(defn add-product-at-coords-Warehouse [warehouse coords quantity]
    (let [moved-warehouse (move-to-position-Warehouse warehouse
                                                      (:row coords)
                                                      (:col coords))]
            (replace-current-container moved-warehouse 
                                       (assoc (get-current-container moved-warehouse)
                                              :quantity
                                              (+ (:quantity (get-current-container moved-warehouse)) 
                                                 quantity)))))


(defn perform-mutation-to-product-Warehouse-factory [strategy]
    (fn [warehouse product-name quantity] 
        (strategy warehouse 
                  (if (= product-name nil)
                      (Coords. (:cur-row warehouse)
                               (:cur-col warehouse))
                      (if (= 
                            (get (:catalogue warehouse) product-name)
                            nil)
                          (throw (AssertionError. "product with that name does not exist"))
                          (get (:catalogue warehouse) product-name)))
                  quantity)))

(def withdraw-product-to-Warehouse (perform-mutation-to-product-Warehouse-factory 
                                    withdraw-product-at-coords-Warehouse))

(def add-product-to-Warehouse (perform-mutation-to-product-Warehouse-factory 
                               add-product-at-coords-Warehouse))

; DIAGNOSTICS

(defn get-total-value-of-product [warehouse product-coords] 
    (get-Container-total-value (get-container-at-coords warehouse product-coords)))

(defn get-total-value-of-Warehouse [warehouse] 
    (assoc warehouse :total-value (reduce + (map (fn [catalogue-item] 
                                    (if (= (first catalogue-item) "empty-product")
                                        0
                                        (get-total-value-of-product warehouse (last catalogue-item)))) 
                                    (:catalogue warehouse)))))

(defn stock-is-scarce [warehouse stock]
    (< stock (:low-quantity-treshold warehouse)))

(defn get-scarce-products-of-Warehouse [warehouse] 
    (assoc warehouse :scarce-products (filter (fn [container] 
                                                (and (not= container nil)
                                                     (stock-is-scarce warehouse (:quantity container)))) 
                                              (map (fn [catalogue-item] 
                                                    (if (= (first catalogue-item) "empty-product")
                                                        nil
                                                        (get-container-at-coords warehouse (last catalogue-item))))
                 (:catalogue warehouse)))))

; warehouse serialization into a string 
(defn serialize-Warehouse [warehouse]
    (str "MOVEMENTS PERFORMED\n"
        (reduce (fn [prev item]
                    (str prev item "\n"))
                ""
                (:movement-history warehouse))
        "WAREHOUSE TEXT REPRESENTATION\n"
        "N-ROWS: " (:n-rows warehouse) "\n"
        "N-COLS: " (:n-cols warehouse) "\n"
        "CUR-ROW: " (:cur-row warehouse) "\n"
        "CUR-COL: " (:cur-col warehouse) "\n"
        "CATALOGUE: " (:catalogue warehouse) "\n"
        "ROWS: \n"
        (str-rows (:rows warehouse))
        "LOW QUANTITY-THRESHOLD:" (:low-quantity-treshold warehouse) "\n"
        "TOTAL-VALUE: " (:total-value warehouse) "\n"
        "SCARSE PRODUCTS CONTAINERS:\n" 
        (reduce (fn [prev item]
                    (str prev item "\n"))
                ""
                (map (fn [container]
                        (str-Container container))
                      (:scarce-products warehouse)))))