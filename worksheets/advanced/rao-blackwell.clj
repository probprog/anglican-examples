;; gorilla-repl.fileformat = 1

;; **
;;; # Rao-Blackwellization
;;; 
;;; Scoreability of a distribution corresponds to Rao-Blackwellizing our Monte Carlo estimator.
;; **

;; @@
(ns rao-blackwell
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [anglican core runtime emit]))
;; @@

;; **
;;; We can sample from a geometric distribution by counting the number of independent Bernoulli trials before the first success. We can score this explicitly:
;; **

;; @@
(defdist geometric
  "Geometric distribution on support {0,1,2....}"
  [p] []
  (sample* [this]
          (loop [value 0] 
            (if (sample* (flip p))
              value
              (recur (inc value)))))
  (observe* [this value] (+ (log p) (* value (log (- 1 p))))))


; test
(sample* (geometric 0.2))
(exp (observe* (geometric 0.2) 6))
;; @@

;; **
;;; We're going to compare the difference between using this "collapsed" representation, where we explicitly score the geometric random variable, with an "explicit" representation where we score the individual Bernoulli trials.
;; **

;; @@
(def measurement 4.5)

(defquery noisy-geometric-explicit []
  (let [p (sample (uniform-continuous 0. 1.))
        k (loop [value 0]
            (if (sample (flip p))
              value
              (recur (inc value))))]
    (observe (normal k 0.5) measurement)
    {:p p :k k}))


(defquery noisy-geometric-collapsed []
  (declare :primitive geometric)
  (let [p (sample (uniform-continuous 0. 1.))
        k (sample (geometric p))]
    (observe (normal k 1.0) measurement)
    {:p p :k k}))
;; @@

;; @@
(def num-samples 10000)

(def explicit-query 
  (take num-samples
        (map :result (doquery :lmh noisy-geometric-explicit []))))

(def collapsed-query 
  (take num-samples
        (map :result (doquery :lmh noisy-geometric-collapsed []))))
;; @@

;; **
;;; Let's compare the behaviour of the two different samplers.
;; **

;; @@
(defn running-mean [N results sym]
  (let [p-vals (map sym (take N results))]
    (loop [means [(first p-vals)]
           p-vals (rest p-vals)
           n 1.0]
      (if (empty? p-vals) 
        means
        (recur (conj means (/ (+ (first p-vals) 
                                 (* n (last means)))
                              (inc n)))
               (rest p-vals) 
               (inc n))))))
;; @@

;; **
;;; The "collapsed" version, in RED, mixes more through the space.
;; **

;; @@
(plot/compose
  (plot/list-plot (map :k explicit-query) :color "#0000ff" :joined true
                  :plot-range [[0 num-samples] [0.0 8]])
  (plot/list-plot (map :k collapsed-query) :color "#ff0000" :joined true))

;; @@

;; @@
(plot/compose
  (plot/list-plot (running-mean num-samples explicit-query :k) :color "#0000ff" :joined true
                  :plot-range [[0 num-samples] [0.0 8]])
  (plot/list-plot (running-mean num-samples collapsed-query :k) :color "#ff0000" :joined true))

(plot/compose
  (plot/list-plot (running-mean num-samples explicit-query :p) :color "#0000ff" :joined true
                  :plot-range [[0 num-samples] [0.0 1.0]])
  (plot/list-plot (running-mean num-samples collapsed-query :p) :color "#ff0000" :joined true))
;; @@

;; @@
(plot/compose
  (plot/list-plot (map :p explicit-query) :color "#0000ff" :joined true
                  :plot-range [[0 num-samples] [0.0 1.0]])
  (plot/list-plot (map :p collapsed-query) :color "#ff0000" :joined true))
;; @@

;; @@
(/ (reduce + (map :p explicit-query)) num-samples)
;; @@

;; @@
(/ (reduce + (map :p collapsed-query)) num-samples)
;; @@

;; @@

;; @@
