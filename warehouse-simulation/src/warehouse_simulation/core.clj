(ns warehouse-simulation.core 
  (:require [warehouse-simulation.domain.product :refer :all]
            [warehouse-simulation.domain.container :refer :all]
            [warehouse-simulation.domain.warehouse :refer :all]))

(def FILE-PATH 
  "C:\Users\adrhe\MetodosComputacionales\WarehouseSimulation\warehouse-simulation\assets\in.txt")

(defn write-ouput [output-path]
  (spit output-path "hi there"))

(defn -main []
  )
