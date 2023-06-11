(defn filter-value [value is-of-type?]
    (if (is-of-type? value)
        value
        (throw (AssertionError. (str value " is of invalid type")))))

; ================ Product record ==============

(defrecord Product 
    [id
     name 
     price])

(defn build-Product [id name price] 
    (Product. (filter-value id  number?) 
              (filter-value name string?) 
              (filter-value price number?)))

(defn bulid-null-Product []
    (Product. -1
              "NULL PRODUCT"
              0))

(defn str-Product (product)
    nil)

; ================ Container record ==============

(defrecord Container
    [row
     col 
     product
     quantity])

(defn build-Container [row col product quantity]
    (Container. (filter-value row number?)
                (filter-value col number?)
                (filter-value product (fn [item] (= (type item) Product)))
                (filter-value quantity number?)))

(defn build-empty-Container [row col]
    (Container. (filter-value row number?)
                (filter-value col number?)
                (build-null-Product) 
                0))

(defn set-Container-product [contianer product]
    (if ( and (= (type container) Container)
              (= (type product) Product)) 
        (assoc container :product product)
        (throw (AssertionError. "arguments of invalid type"))))

(defn deposit-Container-product [container quantity-to-add]
    (if (and (= (type container) Container)
             (= (type (get container :product)) Product))
        (assoc container :quantity (+ (get container :quantity) quantity-to-add))
        (throw (AssertionError. "arguments of invalid type"))))

(defn get-Container-total-value [container]
    (* (get-in container [:product :price]) (get container :quantity)))

; row does not have a record since it is just an array of containers

(defn build-empty-row [n-row n-cols] 
    (map (fn [col] (build-empty-container n-row col)) (vec (range 1 (+ n-cols 1)))))

; ================ Warehouse record ==============

; first row in rows is always current row
(defrecord Warehouse
    [n-rows 
     n-cols
     cur-row 
     cur-col 
     catalogue
     rows 
     low-quantity-treshold])

(defn _build-rows [cur-row n-rows n-cols]
    (if (= cur-row 1)
        (build-empty-row cur-row n-cols))
    (conj (_build-rows (- cur-row 1) n-rows n-cols) (build-empty-row cur-row n-cols)))

(defn build-rows [n-rows n-cols]
    (_build-rows n-rows n-rows n-cols))

(defn build-empty-Warehouse 
    [n-rows n-cols low-quantity-treshold]
    (Warehouse. n-rows 
                n-cols 
                0 
                0 
                []
                (build-rows n-rows n-cols)
                low-quantity-treshold))

(defn get-next-row [row n-rows]
    (if (= row n-rows)
        1
        (+ row 1)))

(defn get-previous-row [row n-rows]
    (if (= row 1)
        n-rows
        (- row 1)))

(defn rotate-up-rows [rows]
    (conj (rest rows) (first rows)))

(defn rotate-down-rows [rows]
    (conj (pop rows) (last rows)))

(defn move-up-Warehouse [warehouse] 
    (do 
        (assoc warehouse :cur-row (get-next-row (:cur-row warehouse)))
        (assoc warehouse :rows (rotate-up-rows (:rows warehouse)))))

(defn move-down-Warehouse [warehouse]
    (do 
        (assoc warehouse :cur-row (get-previous-row (:cur-row warehouse)))
        (assoc warehouse :rows (rotate-down-rows (:rows warehouse)))))

(defn move-right-Warehouse [warehouse]
    (if (= (:cur-col warehouse) 1)
        (throw (Exception. "invalid movement"))
        (assoc warehouse :cur-col (- (:cur-col warehouse) 1))))

(defn move-left-Warehouse [warehouse]
    (if (= (:cur-col warehouse) (:n-cols warehouse))
        (thow (Exception. "invalid movement"))
        (assoc warehouse :cur-col (+ (:cur-col warehouse) 1))))

(defn fetch-current-container [warehouse]
    (nth (first (:rows warehouse)) (- (:cur-col warehouse) 1)))



(defn main 
    []
    (do
        (println "beggining execution")
        (build-Product 123 "apple" 10)))

(main)