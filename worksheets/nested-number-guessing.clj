;; gorilla-repl.fileformat = 1

;; **
;;; # Nested Number Guessing
;;; 
;;; This is absolutely completely totally plagiarised from [ForestDB](http://forestdb.org/models/nested-guessing.html).  In fact we'll describe the problem in _exactly_ their words:
;;; 
;;; "Two agents play a game in which each agent needs to name a number between 0 and 9 and they win if their numbers add up to 13. The first player knows this, and he knows that the second player gets to see the number the first player chooses, but the second player mistakenly thinks that the two win if their numbers add up to any number greater than 8 (and the first player knows this as well). What number should the first player choose?"
;;; 
;;; Their code looks like this:
;;; 
;;; ```(define (sample)
;;;   (rejection-query
;;;    (define a (sample-integer 10))
;;;    (define b
;;;      (rejection-query
;;;       (define c (sample-integer 10))
;;;       c
;;;       (> (+ a c) 8)))
;;;    a
;;;    (= (+ a b) 13)))
;;; 
;;; (hist (repeat 10000 sample))```
;;; 
;;; 
;;; Ours looks something like this:
;; **

;; @@
(ns nested-number-guessing
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [mrepl core]
        [embang runtime emit]
        [clojure.string :only (join split blank?)]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; We have to get rejection sampling semantics to impose equality.
;; **

;; @@
(defdist dirac
  "Dirac distribution"
  [x] []
      (sample [this] x)
      (observe [this value] (if (= x value) 0.0 (- (/ 1.0 0.0)))))

(with-primitive-procedures [dirac] 
  (defquery inner [a] 
    (let [c (sample (uniform-discrete 0 10))]
      (observe (dirac (> (+ a c) 8)) true)
      (predict :c c))))

;(:c (get-predicts (first 
(with-primitive-procedures [doquery dirac get-predicts get-log-weight]
  (defquery outer []
    (let [valid-sample (fn [x]  (= (- (/ 0.0 1.0)) (get-log-weight x)))
          a (sample (uniform-discrete 0 10))
          b (:c (get-predicts (first (filter valid-sample (take 100 (doquery :smc inner [a] :debug true :number-of-particles 1))))))
          ;_ (prn b)
          ]
      (observe (dirac (+ a b)) 13)
      (predict :a a))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;nested-number-guessing/outer</span>","value":"#'nested-number-guessing/outer"}
;; <=

;; **
;;; The constant 100 in the take expression above is a horrible hack just to get it to work.
;;; 
;;; We can check that this generator produces a sample distribution that is in accordance with our understanding of the normal distribution.
;; **

;; @@
(plot/histogram (->>
                  (doquery :lmh outer [])
                  (filter #(= (- (/ 0.0 1.0)) (get-log-weight %)))
                  (take 10000)
                  (map get-predicts)
                  (map :a))
                :normalise :probability)
;(->>
;                  (doquery :lmh outer [] :number-of-particles 100)
;                  (filter #(= (- (/ 0.0 1.0)) (get-log-weight %)))
;                  (take 10))
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"c8219500-76c7-4ba4-808a-fe9c7e59ea80","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"c8219500-76c7-4ba4-808a-fe9c7e59ea80","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"c8219500-76c7-4ba4-808a-fe9c7e59ea80"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"c8219500-76c7-4ba4-808a-fe9c7e59ea80","values":[{"x":1.0,"y":0},{"x":1.5333333333333334,"y":0.0016},{"x":2.066666666666667,"y":0.0},{"x":2.6000000000000005,"y":0.0},{"x":3.1333333333333337,"y":0.0},{"x":3.666666666666667,"y":0.0},{"x":4.2,"y":0.1871},{"x":4.733333333333333,"y":0.0},{"x":5.266666666666667,"y":0.2022},{"x":5.8,"y":0.0},{"x":6.333333333333333,"y":0.1712},{"x":6.866666666666666,"y":0.0},{"x":7.3999999999999995,"y":0.15},{"x":7.933333333333333,"y":0.0},{"x":8.466666666666667,"y":0.1683},{"x":9.0,"y":0.0},{"x":9.533333333333333,"y":0.1196},{"x":10.066666666666666,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"c8219500-76c7-4ba4-808a-fe9c7e59ea80\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"c8219500-76c7-4ba4-808a-fe9c7e59ea80\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"c8219500-76c7-4ba4-808a-fe9c7e59ea80\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"c8219500-76c7-4ba4-808a-fe9c7e59ea80\", :values ({:x 1.0, :y 0} {:x 1.5333333333333334, :y 0.0016} {:x 2.066666666666667, :y 0.0} {:x 2.6000000000000005, :y 0.0} {:x 3.1333333333333337, :y 0.0} {:x 3.666666666666667, :y 0.0} {:x 4.2, :y 0.1871} {:x 4.733333333333333, :y 0.0} {:x 5.266666666666667, :y 0.2022} {:x 5.8, :y 0.0} {:x 6.333333333333333, :y 0.1712} {:x 6.866666666666666, :y 0.0} {:x 7.3999999999999995, :y 0.15} {:x 7.933333333333333, :y 0.0} {:x 8.466666666666667, :y 0.1683} {:x 9.0, :y 0.0} {:x 9.533333333333333, :y 0.1196} {:x 10.066666666666666, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@
;(plot/histogram (->> (doquery :lmh inner [8] :number-of-particles 10)
;                	 (take 100)
;                      (map get-predicts)
;                      (map :c))
                  
;                              :normalise :probability)
;; @@
