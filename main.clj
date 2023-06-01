(defn filter-value [value is-of-type?]
    (if (is-of-type? value)
        value
        (throw (AssertionError. (str value " is of invalid type")))))

; ================ Product register ==============

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

; ================ Container register ==============

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

; ================ Warehouse register ==============

(defrecord Warehouse
    [n-rows 
     n-cols
     cur-row 
     cur-col 
     catalogue
     rows 
     low-quantity-treshold])

; warehouse register

(defn main 
    []
    (do
        (println "beggining execution")
        (build-Product 123 "apple" 10)))

(main)