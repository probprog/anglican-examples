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
        [anglican core runtime emit [inference :only [collect-by]]]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Our version looks like this.  The first player has no prior knowledge, so she picks uniformly and conditions on her constraint being satisfied, i.e. the two-player sum being 13.  She knows that the second player will only choose numbers whose sum is greater than 8, but has no additional preference.
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
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-unkown'>#multifn[print-method 0x3c4e70fd]</span>","value":"#multifn[print-method 0x3c4e70fd]"},{"type":"html","content":"<span class='clj-var'>#&#x27;nested-number-guessing/secondplayer</span>","value":"#'nested-number-guessing/secondplayer"}],"value":"[#multifn[print-method 0x3c4e70fd],#'nested-number-guessing/secondplayer]"},{"type":"html","content":"<span class='clj-var'>#&#x27;nested-number-guessing/firstplayer</span>","value":"#'nested-number-guessing/firstplayer"}],"value":"[[#multifn[print-method 0x3c4e70fd],#'nested-number-guessing/secondplayer],#'nested-number-guessing/firstplayer]"}
;; <=

;; **
;;; When this query is run, it suggests that she should never pick a number below 4 and that larger numbers are generally less desirable.
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"7f7edac6-6cf8-4524-a1f8-abf1403f7581","values":[{"x":0,"y":0},{"x":1,"y":0},{"x":2,"y":0},{"x":3,"y":0},{"x":4,"y":0.21642857142857216},{"x":5,"y":0.19169395821569604},{"x":6,"y":0.17092885375493921},{"x":7,"y":0.14847543760587273},{"x":8,"y":0.13732354601919855},{"x":9,"y":0.13514963297572108}]}],"marks":[{"type":"rect","from":{"data":"7f7edac6-6cf8-4524-a1f8-abf1403f7581"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"7f7edac6-6cf8-4524-a1f8-abf1403f7581","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"7f7edac6-6cf8-4524-a1f8-abf1403f7581","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"7f7edac6-6cf8-4524-a1f8-abf1403f7581\", :values ({:x 0, :y 0} {:x 1, :y 0} {:x 2, :y 0} {:x 3, :y 0} {:x 4, :y 0.21642857142857216} {:x 5, :y 0.19169395821569604} {:x 6, :y 0.17092885375493921} {:x 7, :y 0.14847543760587273} {:x 8, :y 0.13732354601919855} {:x 9, :y 0.13514963297572108})}], :marks [{:type \"rect\", :from {:data \"7f7edac6-6cf8-4524-a1f8-abf1403f7581\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"7f7edac6-6cf8-4524-a1f8-abf1403f7581\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"7f7edac6-6cf8-4524-a1f8-abf1403f7581\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; It is interesting to note that Anglican' defm is not the same as a nested query.  defm allows code structuring and syntactic decomposition of the overall joint distribution.  In the above code, conditioning occurs by changing the lower bound of the second player's choice, exactly as desired.  
;;; 
;;; If one instead wished to directly transcribe the code from ForestDB, with the second player making a nested query using a uniform draw from a uniform distribution over the (0 10], then in order to turn defm into the conditional distribution equivalent to a nested query, one must weight the trace by the conditional distribution normalization constant. 
;;; 
;;; The following example is _not_ how one would write this query, but instead serves to highlight some important semantic differences between nesting of queries (which we don't support in the same way as WebPPL) and code structuring decompositions of the joint.
;;; 
;;; To really get the difference, consider what it means to have the `(observe flip ...)` line in the following, and see what happens if you comment it out.
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"daa0de22-03e6-4157-927d-fce74ffd5e3e","values":[{"x":0,"y":0},{"x":1,"y":0},{"x":2,"y":0},{"x":3,"y":0},{"x":4,"y":0.23021090545524456},{"x":5,"y":0.19704181925467817},{"x":6,"y":0.1626161647512414},{"x":7,"y":0.15144704876266618},{"x":8,"y":0.12811886956869453},{"x":9,"y":0.13056519220747545}]}],"marks":[{"type":"rect","from":{"data":"daa0de22-03e6-4157-927d-fce74ffd5e3e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"daa0de22-03e6-4157-927d-fce74ffd5e3e","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"daa0de22-03e6-4157-927d-fce74ffd5e3e","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"daa0de22-03e6-4157-927d-fce74ffd5e3e\", :values ({:x 0, :y 0} {:x 1, :y 0} {:x 2, :y 0} {:x 3, :y 0} {:x 4, :y 0.23021090545524456} {:x 5, :y 0.19704181925467817} {:x 6, :y 0.1626161647512414} {:x 7, :y 0.15144704876266618} {:x 8, :y 0.12811886956869453} {:x 9, :y 0.13056519220747545})}], :marks [{:type \"rect\", :from {:data \"daa0de22-03e6-4157-927d-fce74ffd5e3e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"daa0de22-03e6-4157-927d-fce74ffd5e3e\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"daa0de22-03e6-4157-927d-fce74ffd5e3e\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@

;; @@
