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
;;; 	(define (sample)
;;; 	  (rejection-query
;;; 	    (define a (sample-integer 10))
;;; 	    (define b
;;; 	      (rejection-query
;;; 	        (define c (sample-integer 10))
;;; 	        c
;;; 	        (> (+ a c) 8)))
;;; 	    a
;;; 	    (= (+ a b) 13)))
;;; 	(hist (repeat 10000 sample))
;;; 
;;; 
;;; Ours looks something like this:
;; **

;; @@
(ns nested-number-guessing
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [anglican
         core 
         [runtime :exclude [distribution]] 
         emit
         [state :only [get-predicts get-log-weight]]]
        distribution
        [anglib crp]
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
      (observe [this value] (if (= x value) 0.0 Double/NEGATIVE_INFINITY)))

(defn inner [a] (with-primitive-procedures [dirac] 
  (distribution [a] 
    (let [c (sample (uniform-discrete 0 10))]
      (observe (dirac (> (+ a c) 8)) true)
      (predict :c c)))))

(def b (with-primitive-procedures [inner] (distribution [] (predict (sample (inner 10))))))

(sample b)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>inner</span>","value":"inner"},{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"}],"value":"(inner 10)"},{"type":"html","content":"<span class='clj-unkown'>#&lt;nested_number_guessing$inner$reify__21498 nested_number_guessing$inner$reify__21498@482de4a9&gt;</span>","value":"#<nested_number_guessing$inner$reify__21498 nested_number_guessing$inner$reify__21498@482de4a9>"}],"value":"[(inner 10) #<nested_number_guessing$inner$reify__21498 nested_number_guessing$inner$reify__21498@482de4a9>]"}],"value":"{(inner 10) #<nested_number_guessing$inner$reify__21498 nested_number_guessing$inner$reify__21498@482de4a9>}"}
;; <=

;; @@
(def outer (with-primitive-procedures [inner dirac]
  (distribution []
    (let [a (sample (uniform-discrete 0 10))
          _ (prn (inner 10))
          b (:c (sample (inner a)))
          _ (prn b)
          ]
      (observe (dirac (+ a b)) 13)
      (predict :a a)))))

(sample outer)
;; @@

;; **
;;; 
;; **

;; @@

;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"63535e4c-d7e1-46fe-9c83-12b72acb4d93","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"63535e4c-d7e1-46fe-9c83-12b72acb4d93","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"63535e4c-d7e1-46fe-9c83-12b72acb4d93"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"63535e4c-d7e1-46fe-9c83-12b72acb4d93","values":[{"x":4.0,"y":0},{"x":4.333333333333333,"y":0.2642},{"x":4.666666666666666,"y":0.0},{"x":4.999999999999999,"y":0.0},{"x":5.333333333333332,"y":0.1862},{"x":5.666666666666665,"y":0.0},{"x":5.999999999999998,"y":0.0},{"x":6.333333333333331,"y":0.1407},{"x":6.666666666666664,"y":0.0},{"x":6.999999999999997,"y":0.0},{"x":7.33333333333333,"y":0.1529},{"x":7.666666666666663,"y":0.0},{"x":7.9999999999999964,"y":0.0},{"x":8.33333333333333,"y":0.1176},{"x":8.666666666666664,"y":0.0},{"x":8.999999999999998,"y":0.0},{"x":9.333333333333332,"y":0.1384},{"x":9.666666666666666,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"63535e4c-d7e1-46fe-9c83-12b72acb4d93\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"63535e4c-d7e1-46fe-9c83-12b72acb4d93\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"63535e4c-d7e1-46fe-9c83-12b72acb4d93\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"63535e4c-d7e1-46fe-9c83-12b72acb4d93\", :values ({:x 4.0, :y 0} {:x 4.333333333333333, :y 0.2642} {:x 4.666666666666666, :y 0.0} {:x 4.999999999999999, :y 0.0} {:x 5.333333333333332, :y 0.1862} {:x 5.666666666666665, :y 0.0} {:x 5.999999999999998, :y 0.0} {:x 6.333333333333331, :y 0.1407} {:x 6.666666666666664, :y 0.0} {:x 6.999999999999997, :y 0.0} {:x 7.33333333333333, :y 0.1529} {:x 7.666666666666663, :y 0.0} {:x 7.9999999999999964, :y 0.0} {:x 8.33333333333333, :y 0.1176} {:x 8.666666666666664, :y 0.0} {:x 8.999999999999998, :y 0.0} {:x 9.333333333333332, :y 0.1384} {:x 9.666666666666666, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@

;; @@
