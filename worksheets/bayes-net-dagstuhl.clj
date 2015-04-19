;; gorilla-repl.fileformat = 1

;; **
;;; # Bayes Nets Dagstuhl
;;; 
;; **

;; @@
(ns bayes-net-dagstuhl
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
;;; Bayes nets are easy to express in Anglican.    Take, for example, a pedagogical Bayes net
;;; 
;;; <img src="http://www.robots.ox.ac.uk/~fwood/anglican/examples/bayes_net/bayes_net.png" alt="Bayes net graphical model" style="width: 300px;" />.  
;;; 
;;; It's encoding as a program follows.
;;; 
;;; Note that like in the  immediately following code we use  ```dirac``` to inject observations however this is inefficient because of the general purpose algorithms used; most effectively become rejection samplers if the Bayes net is coded this way.  Fine for small Bayes nets like this one, but disastrous in larger nets.
;;; 
;;; Far preferable practice follows.
;; **

;; @@
(defdist dirac*
  "Dirac distribution"
  [x] []
  (sample [this] x)
  (observe [this value] (if (= x value) 0.0 (- (/ 1.0 0.0)))))
(with-primitive-procedures [dirac*]
  (defm dirac [& args] (apply dirac* args)))

(def bayes-net-natural
   (distribution [] {:algorithm :pcascade :number-of-threads 500 :number-of-paricles 10}
     (let [is-cloudy (sample (flip 0.5))
        
        is-raining (cond (= is-cloudy true ) 
                           (sample (flip 0.8))
                         (= is-cloudy false) 
                           (sample (flip 0.2)))
        
        sprinkler  (cond (= is-cloudy true ) 
                           (sample (flip 0.1))
                         (= is-cloudy false) 
                           (sample (flip 0.5)))
        
        wet-grass  (cond (and (= sprinkler true) 
                              (= is-raining true))
                           (sample (flip 0.99))
                         (and (= sprinkler false) 
                             (= is-raining false)) 
                           (sample (flip 0.0))
                         (or  (= sprinkler true) 
                              (= is-raining true)) 
                           (sample (flip 0.9)))]
    
    	   (observe (dirac sprinkler) true)
           (observe (dirac wet-grass) true)
           

           (predict :s {:is-cloudy  is-cloudy 
                        :is-raining is-raining 
                        :sprinkler  sprinkler})))) 
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net-dagstuhl/bayes-net-natural</span>","value":"#'bayes-net-dagstuhl/bayes-net-natural"}
;; <=

;; @@
(instance? anglican.runtime.distribution bayes-net-natural)
(clojure.pprint/pprint (repeatedly 10 #(sample bayes-net-natural)))
;; @@
;; ->
;;; ({:s {:is-cloudy false, :is-raining false, :sprinkler true}}
;;;  {:s {:is-cloudy true, :is-raining true, :sprinkler true}}
;;;  {:s {:is-cloudy false, :is-raining false, :sprinkler true}}
;;;  {:s {:is-cloudy false, :is-raining false, :sprinkler true}}
;;;  {:s {:is-cloudy false, :is-raining false, :sprinkler true}}
;;;  {:s {:is-cloudy true, :is-raining true, :sprinkler true}}
;;;  {:s {:is-cloudy true, :is-raining true, :sprinkler true}}
;;;  {:s {:is-cloudy false, :is-raining false, :sprinkler true}}
;;;  {:s {:is-cloudy false, :is-raining false, :sprinkler true}}
;;;  {:s {:is-cloudy false, :is-raining false, :sprinkler true}})
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Let's run the query and see what the probability of it raining is given that the sprinkler is on and the grass is wet anyway.
;; **

;; @@
(->> (repeatedly #(sample bayes-net-natural))
     (map :s)
     (map :is-raining)
     (map #(if % 1 0))
     (take 10000)
     (#(plot/histogram % :normalize :probability))
     )
     
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"2d64fc0b-9cdd-4e1a-98e3-022b73ba6c9b","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"2d64fc0b-9cdd-4e1a-98e3-022b73ba6c9b","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"2d64fc0b-9cdd-4e1a-98e3-022b73ba6c9b"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"2d64fc0b-9cdd-4e1a-98e3-022b73ba6c9b","values":[{"x":0.0,"y":0},{"x":0.06666666666666668,"y":0.6815},{"x":0.13333333333333336,"y":0.0},{"x":0.20000000000000004,"y":0.0},{"x":0.2666666666666667,"y":0.0},{"x":0.33333333333333337,"y":0.0},{"x":0.4,"y":0.0},{"x":0.4666666666666667,"y":0.0},{"x":0.5333333333333333,"y":0.0},{"x":0.6,"y":0.0},{"x":0.6666666666666666,"y":0.0},{"x":0.7333333333333333,"y":0.0},{"x":0.7999999999999999,"y":0.0},{"x":0.8666666666666666,"y":0.0},{"x":0.9333333333333332,"y":0.0},{"x":0.9999999999999999,"y":0.0},{"x":1.0666666666666667,"y":0.3185},{"x":1.1333333333333333,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"2d64fc0b-9cdd-4e1a-98e3-022b73ba6c9b\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"2d64fc0b-9cdd-4e1a-98e3-022b73ba6c9b\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"2d64fc0b-9cdd-4e1a-98e3-022b73ba6c9b\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"2d64fc0b-9cdd-4e1a-98e3-022b73ba6c9b\", :values ({:x 0.0, :y 0} {:x 0.06666666666666668, :y 0.6815} {:x 0.13333333333333336, :y 0.0} {:x 0.20000000000000004, :y 0.0} {:x 0.2666666666666667, :y 0.0} {:x 0.33333333333333337, :y 0.0} {:x 0.4, :y 0.0} {:x 0.4666666666666667, :y 0.0} {:x 0.5333333333333333, :y 0.0} {:x 0.6, :y 0.0} {:x 0.6666666666666666, :y 0.0} {:x 0.7333333333333333, :y 0.0} {:x 0.7999999999999999, :y 0.0} {:x 0.8666666666666666, :y 0.0} {:x 0.9333333333333332, :y 0.0} {:x 0.9999999999999999, :y 0.0} {:x 1.0666666666666667, :y 0.3185} {:x 1.1333333333333333, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; It looks like the probability of it raining is around 30%.
;;; 
;;; A method of coding this example that will result in higher inference performance looks like the following where we have moved observes as high up in the program as they can be moved.  This is a manual program transformation that will make its way into the compiler at some point.
;; **

;; @@
(defquery bayes-net-alt [] 
  (let [is-cloudy (sample (flip 0.5))
        
        is-raining (cond (= is-cloudy true ) 
                           (sample (flip 0.8))
                         (= is-cloudy false) 
                           (sample (flip 0.2)))
        sprinkler-dist (cond (= is-cloudy true) 
                            	(flip 0.1)
                         	(= is-cloudy false) 
                            	(flip 0.5))
        
        sprinkler true
        _ (observe sprinkler-dist sprinkler)
        
        wet-grass-dist (cond (and (= sprinkler true) 
                             (= is-raining true))  								  				       (flip 0.99)
                        (and (= sprinkler false) 
                             (= is-raining false))  								  				   (flip 0.0)
                        (or  (= sprinkler true) 
                             (= is-raining true))  								  					   (flip 0.9))
        
        wet-grass true
        _ (observe wet-grass-dist wet-grass)]
    
           (predict :s (hash-map :is-cloudy  is-cloudy 
                                 :is-raining is-raining 
                                 :sprinkler  sprinkler) )))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net-dagstuhl/bayes-net-alt</span>","value":"#'bayes-net-dagstuhl/bayes-net-alt"}
;; <=

;; @@
(->> (doquery :pimh bayes-net-alt nil :number-of-particles 10)
     (map get-predicts)
     (map :s)
     (map :is-raining)
     (map #(if % 1 0))
     (take 2000)
     (#(plot/histogram % :normalize :probability))
     )
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"32dbe85a-2a62-40f0-935c-527775ac8ed4","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"32dbe85a-2a62-40f0-935c-527775ac8ed4","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"32dbe85a-2a62-40f0-935c-527775ac8ed4"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"32dbe85a-2a62-40f0-935c-527775ac8ed4","values":[{"x":0.0,"y":0},{"x":0.08333333333333336,"y":0.671},{"x":0.1666666666666667,"y":0.0},{"x":0.25000000000000006,"y":0.0},{"x":0.3333333333333334,"y":0.0},{"x":0.4166666666666668,"y":0.0},{"x":0.5000000000000001,"y":0.0},{"x":0.5833333333333335,"y":0.0},{"x":0.6666666666666669,"y":0.0},{"x":0.7500000000000002,"y":0.0},{"x":0.8333333333333336,"y":0.0},{"x":0.916666666666667,"y":0.0},{"x":1.0000000000000002,"y":0.329},{"x":1.0833333333333335,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"32dbe85a-2a62-40f0-935c-527775ac8ed4\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"32dbe85a-2a62-40f0-935c-527775ac8ed4\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"32dbe85a-2a62-40f0-935c-527775ac8ed4\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"32dbe85a-2a62-40f0-935c-527775ac8ed4\", :values ({:x 0.0, :y 0} {:x 0.08333333333333336, :y 0.671} {:x 0.1666666666666667, :y 0.0} {:x 0.25000000000000006, :y 0.0} {:x 0.3333333333333334, :y 0.0} {:x 0.4166666666666668, :y 0.0} {:x 0.5000000000000001, :y 0.0} {:x 0.5833333333333335, :y 0.0} {:x 0.6666666666666669, :y 0.0} {:x 0.7500000000000002, :y 0.0} {:x 0.8333333333333336, :y 0.0} {:x 0.916666666666667, :y 0.0} {:x 1.0000000000000002, :y 0.329} {:x 1.0833333333333335, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; Not surprisingly this produces the indistinguishable results, however, in the case of larger Bayes nets this transformation will result in significantly higher inference performance before enumeration query and optimizing program transformations are included in the compiler.
;; **
