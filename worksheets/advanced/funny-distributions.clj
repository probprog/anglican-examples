;; gorilla-repl.fileformat = 1

;; **
;;; # Funny Distributions
;;; 
;;; Implicitly, we assume that all programs in Anglican halt with probability one.
;;; 
;;; If this isn't the case, then we have defined a very funny distribution!
;; **

;; @@
(ns funny-distributions
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [anglican core runtime emit [state :only [get-predicts]]] 
        [anglib crp]
        [clojure.string :only (join split blank?)]))
;; @@

;; @@
(def infinite-loop #(loop [] (recur)))
(defn funny-dist [] (if (sample* (flip 0.5))
                      1 
                      (if (sample* (flip 0.5))
                        (funny-dist)
                        (infinite-loop))))

(with-primitive-procedures [funny-dist infinite-loop] 
  (defquery induced-dist (predict :x (funny-dist))))
         
;; @@

;; @@
(take 4 (doquery :importance induced-dist nil :number-of-threads 100))
;; @@

;; @@

;; @@
