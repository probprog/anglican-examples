;; gorilla-repl.fileformat = 1

;; **
;;; # Frank's scratch space
;;; 
;; **

;; @@
(ns franks-scratch
  (:require [gorilla-plot.core :as plot]
   			[embang lmh
                    [state :refer [get-predicts]]
                    [inference :refer [infer warmup]]])
  (:use [embang emit runtime]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(defdist dirac
  "Dirac distribution"
  [x] []
      (sample [this] x)
      (observe [this value] (if (= x value) 0.0 (- (/ 1.0 0.0)))))

(defm non-invertible-function [x] (* x x))

(defquery condition-on-measure-zero-set [x] 
  (let [m (sample (normal 0 1))
        z (non-invertible-function m)]
    (observe (dirac z) x)
    (predict m)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;franks-scratch/condition-on-measure-zero-set</span>","value":"#'franks-scratch/condition-on-measure-zero-set"}
;; <=

;; @@

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-symbol'>c</span>","value":"c"}
;; <=

;; @@

;; @@
