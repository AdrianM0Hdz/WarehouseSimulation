(ns warehouse-simulation.core 
  (:require [warehouse-simulation.domain.product :refer :all]
            [warehouse-simulation.domain.container :refer :all]
            [warehouse-simulation.domain.warehouse :refer :all]
            [warehouse-simulation.application.warehouse-script-parser :refer :all]))

(def OUTPUT-FILES-LOCATION 
  "C:\\Users\\adrhe\\MetodosComputacionales\\WarehouseSimulation\\warehouse-simulation\\assets\\output\\")

(def INPUT-FILES-LOCATION
  "C:\\Users\\adrhe\\MetodosComputacionales\\WarehouseSimulation\\warehouse-simulation\\assets\\input\\")

(def N-INPUT-FILES 7)

(defn make-file-path-pair-array [n-input-files]
  (map (fn [n] [(str INPUT-FILES-LOCATION n ".txt")
                (str OUTPUT-FILES-LOCATION n ".txt")]) (range n-input-files)))

; (parse-script INPUT-FILE-PATH OUTPUT-FILE-PATH)
(defn -main []
  (println "TOP 10% IN STOCK: "  (take (Math/ceil (/ N-INPUT-FILES 10)) 
                                    (reverse 
                                    (sort
                                    (pmap (fn [pair]
                                          (:total-value (parse-script (first pair) (second pair))))
                                      (make-file-path-pair-array N-INPUT-FILES)))))))
