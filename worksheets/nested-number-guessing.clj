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
;;; Starting with our namespace boilerplate
;; **

;; @@
(ns nested-number-guessing
  (:require [gorilla-plot.core :as plot]
            [anglican.stat :as s])
  (:use clojure.repl
        [anglican core runtime emit [inference :only [collect-by]]
         [state :only [get-predicts]]]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Ours looks like this.  The first player has no prior knowledge so she picks uniformly and conditions on her constraint being satisfied, i.e. the two player sum being 13.  She knows that the second player will only choose numbers whose sum is greater than 8 but has no additional preference.
;; **

;; @@
(defdist dirac
  "Dirac distribution centered on x"
  [x] []
  (sample* [this] x)
  (observe* [this value] 
            (if (= x value) 
              0.0  
              (- (/ 1.0 0.0)))))

(defm secondplayer [a] 
  (let [c (- 8 a)]
    (sample (uniform-discrete c 10))))

(with-primitive-procedures [dirac]
  (defquery firstplayer []
    (let [a (sample (uniform-discrete 0 10))
          b (secondplayer a)]
      (observe (dirac (+ a b)) 13)
      a)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;nested-number-guessing/firstplayer</span>","value":"#'nested-number-guessing/firstplayer"}
;; <=

;; **
;;; When this query is run it suggests that she should never pick a number below 4 and that larger numbers are generally less desirable.
;; **

;; @@
(->> (doquery :smc firstplayer [] :number-of-particles 100)
     (take 50000)
     (collect-by :result)
     (s/empirical-distribution)
     (merge (zipmap (range 0 10) (repeat 10 0)))
     (into (sorted-map))
     (#(plot/bar-chart (keys %) (vals %))))

;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"3486ad11-dbb5-4635-93da-8e827b95f85e","values":[{"x":0,"y":0},{"x":1,"y":0},{"x":2,"y":0},{"x":3,"y":0},{"x":4,"y":0.22799514301133786},{"x":5,"y":0.19478953049109693},{"x":6,"y":0.1693497031840248},{"x":7,"y":0.1490879654614133},{"x":8,"y":0.13749865083647828},{"x":9,"y":0.12127900701564866}]}],"marks":[{"type":"rect","from":{"data":"3486ad11-dbb5-4635-93da-8e827b95f85e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"3486ad11-dbb5-4635-93da-8e827b95f85e","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"3486ad11-dbb5-4635-93da-8e827b95f85e","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"3486ad11-dbb5-4635-93da-8e827b95f85e\", :values ({:x 0, :y 0} {:x 1, :y 0} {:x 2, :y 0} {:x 3, :y 0} {:x 4, :y 0.22799514301133786} {:x 5, :y 0.19478953049109693} {:x 6, :y 0.1693497031840248} {:x 7, :y 0.1490879654614133} {:x 8, :y 0.13749865083647828} {:x 9, :y 0.12127900701564866})}], :marks [{:type \"rect\", :from {:data \"3486ad11-dbb5-4635-93da-8e827b95f85e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"3486ad11-dbb5-4635-93da-8e827b95f85e\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"3486ad11-dbb5-4635-93da-8e827b95f85e\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; It is interesting to note that Anglican' defm is not the same as a nested query.  defm allows code structuring and syntactic decomposition of the overall joint distribution.  In the above conditioning occurs by changing the lower bound of the second player's choice, exactly as desired.  
;;; 
;;; If one instead, wished to directly transcribe the code from ForestDB with the second player making nested query using a uniform draw from a uniform distribution over the (0 10] then in order to turn defm into the conditional distribution equivalent to a nested query one must weight the trace by the conditional distribution normalization constant.  
;;; 
;;; The following example is _not_ how one would write this query but instead serves to highlight some important semantic differences between nesting of queries (which we don't support in the same way as WebPPL) and code structuring decompositions of the joint.
;;; 
;;; To really get the difference consider what it means to have the (observe flip...) line in the following and see what happens if you comment it out.
;; **

;; @@
(with-primitive-procedures [dirac]
  (defm secondplayeralt [a] 
    (let [c (sample (uniform-discrete 0 10))
          Z (- 10 (- 8 a))]
      (observe (dirac (> (+ a c) 8)) true)
      (observe (flip (/ 1. Z)) true)
      c))

  (defquery firstplayeralt []
    (let [a (sample (uniform-discrete 0 10))
          b (secondplayeralt a)]
      (observe (dirac (+ a b)) 13)
      a)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;nested-number-guessing/firstplayeralt</span>","value":"#'nested-number-guessing/firstplayeralt"}
;; <=

;; @@
(->> (doquery :smc firstplayeralt [] :number-of-particles 100)
     (take 50000)
     (collect-by :result)
     (s/empirical-distribution)
     (merge (zipmap (range 0 10) (repeat 10 0)))
     (into (sorted-map))
     (#(plot/bar-chart (keys %) (vals %))))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"48402c9d-1071-4927-b734-10e3ad512204","values":[{"x":0,"y":0},{"x":1,"y":0},{"x":2,"y":0},{"x":3,"y":0},{"x":4,"y":0.1969499008025757},{"x":5,"y":0.19663490261549885},{"x":6,"y":0.1696416603875062},{"x":7,"y":0.15072089012254256},{"x":8,"y":0.15531740678064834},{"x":9,"y":0.1307352392912285}]}],"marks":[{"type":"rect","from":{"data":"48402c9d-1071-4927-b734-10e3ad512204"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"48402c9d-1071-4927-b734-10e3ad512204","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"48402c9d-1071-4927-b734-10e3ad512204","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"48402c9d-1071-4927-b734-10e3ad512204\", :values ({:x 0, :y 0} {:x 1, :y 0} {:x 2, :y 0} {:x 3, :y 0} {:x 4, :y 0.1969499008025757} {:x 5, :y 0.19663490261549885} {:x 6, :y 0.1696416603875062} {:x 7, :y 0.15072089012254256} {:x 8, :y 0.15531740678064834} {:x 9, :y 0.1307352392912285})}], :marks [{:type \"rect\", :from {:data \"48402c9d-1071-4927-b734-10e3ad512204\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"48402c9d-1071-4927-b734-10e3ad512204\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"48402c9d-1071-4927-b734-10e3ad512204\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@

;; @@
