(ns warehouse-simulation.end-to-end-test
  (:require [clojure.test :refer :all]
            [warehouse-simulation.domain.product :refer :all]
            [warehouse-simulation.domain.container :refer :all]
            [warehouse-simulation.domain.warehouse :refer :all]
            [warehouse-simulation.application.warehouse-script-parser :refer :all]))

; verifica que dada una entrada in.txt la salida de la serizlizacion del warehouse sea 
; out.txt
(def SCRIPT1 "C:\\Users\\adrhe\\MetodosComputacionales\\WarehouseSimulation\\warehouse-simulation\\assets\\tests\\test1\\in.txt")
(def OUT1 "C:\\Users\\adrhe\\MetodosComputacionales\\WarehouseSimulation\\warehouse-simulation\\assets\\tests\\test1\\out.txt")

(def SCRIPT2 "C:\\Users\\adrhe\\MetodosComputacionales\\WarehouseSimulation\\warehouse-simulation\\assets\\tests\\test2\\in.txt")
(def OUT2 "C:\\Users\\adrhe\\MetodosComputacionales\\WarehouseSimulation\\warehouse-simulation\\assets\\tests\\test2\\out.txt")

(deftest e-2-e-1  
        (is (= (serialize-Warehouse
                (parse-program (conj (with-open [reader (clojure.java.io/reader SCRIPT1)]
                  (reduce conj [] (line-seq reader))) "<EOF>")))
               (clojure.string/replace (slurp OUT1) #"\r" ""))))

(deftest e-2-e-2
        (is (= (serialize-Warehouse
                (parse-program (conj (with-open [reader (clojure.java.io/reader SCRIPT2)]
                  (reduce conj [] (line-seq reader))) "<EOF>")))
               (clojure.string/replace (slurp OUT2) #"\r" ""))))