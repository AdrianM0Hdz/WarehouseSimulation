(ns warehouse-simulation.domain.common
    (:gen-class))

(defn filter-value [value is-of-type?]
    (if (is-of-type? value)
        value
        (throw (AssertionError. (str value " is of invalid type")))))