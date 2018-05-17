;; gorilla-repl.fileformat = 1

;; **
;;; # Blank Worksheet 
;; **

;; **
;;; (recommended namespace declarations only)
;; **

;; @@
(ns blank
  (:require [gorilla-plot.core :as plot]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [clojure.core.matrix :as mat]
            [anglican.core 
             :refer [doquery]]
            [anglican.emit 
             :refer [query defquery conditional fm defm]]
           	[anglican.inference 
             :refer [equalize collect-by log-marginal]]
            [anglican.stat
             :refer [empirical-distribution empirical-expectation 
                     empirical-mean empirical-variance empirical-std empirical-skew empirical-kurtosis]]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
