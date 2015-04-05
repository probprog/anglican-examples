;; gorilla-repl.fileformat = 1

;; **
;;; # Bayes Nets
;;; 
;; **

;; @@
(ns bayes-net
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [mrepl core]
        [embang runtime emit]
        [anglib crp]
        [clojure.string :only (join split blank?)]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Bayes nets are easy to express in Anglican.  Although you should note that observations appear in slightly different places than you might be used to.
;; **

;; @@
(defdist dirac
  "Dirac distribution"
  [x] []
      (sample [this] x)
      (observe [this value] (if (= x value) 0.0 (- (/ 1.0 0.0)))))

(with-primitive-procedures [dirac] (defquery bayes-net-1 [] 
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
                              (= is-raining true))  								  				    (sample (flip 0.99))
                         (and (= sprinkler false) 
                             (= is-raining false))  								  				    (sample (flip 0.0))
                         (or  (= sprinkler true) 
                              (= is-raining true))  								  					(sample (flip 0.9)))]
    
    	   ;(observe (dirac sprinkler) true)
           (observe (dirac wet-grass) true)
           ;(observe (= sprinkler true))
           (observe (= wet-grass true))

           (predict :s (hash-map :is-cloudy  is-cloudy 
                                 :is-raining is-raining 
                                 :sprinkler  sprinkler)))))  
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/bayes-net-1</span>","value":"#'bayes-net/bayes-net-1"}
;; <=

;; @@
(->> (doquery :pimh bayes-net-1 nil :number-of-particles 100)
     (map get-predicts)
     (map :s)
     (map :is-raining)
     (map #(if % 1 0))
     (take 10000)
     (#(plot/histogram % :normalize :probability))
     )
     
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"62701d86-67b5-44e5-824b-87eb4853d76a","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"62701d86-67b5-44e5-824b-87eb4853d76a","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"62701d86-67b5-44e5-824b-87eb4853d76a"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"62701d86-67b5-44e5-824b-87eb4853d76a","values":[{"x":0.0,"y":0},{"x":0.06666666666666668,"y":0.2937},{"x":0.13333333333333336,"y":0.0},{"x":0.20000000000000004,"y":0.0},{"x":0.2666666666666667,"y":0.0},{"x":0.33333333333333337,"y":0.0},{"x":0.4,"y":0.0},{"x":0.4666666666666667,"y":0.0},{"x":0.5333333333333333,"y":0.0},{"x":0.6,"y":0.0},{"x":0.6666666666666666,"y":0.0},{"x":0.7333333333333333,"y":0.0},{"x":0.7999999999999999,"y":0.0},{"x":0.8666666666666666,"y":0.0},{"x":0.9333333333333332,"y":0.0},{"x":0.9999999999999999,"y":0.0},{"x":1.0666666666666667,"y":0.7063},{"x":1.1333333333333333,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"62701d86-67b5-44e5-824b-87eb4853d76a\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"62701d86-67b5-44e5-824b-87eb4853d76a\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"62701d86-67b5-44e5-824b-87eb4853d76a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"62701d86-67b5-44e5-824b-87eb4853d76a\", :values ({:x 0.0, :y 0} {:x 0.06666666666666668, :y 0.2937} {:x 0.13333333333333336, :y 0.0} {:x 0.20000000000000004, :y 0.0} {:x 0.2666666666666667, :y 0.0} {:x 0.33333333333333337, :y 0.0} {:x 0.4, :y 0.0} {:x 0.4666666666666667, :y 0.0} {:x 0.5333333333333333, :y 0.0} {:x 0.6, :y 0.0} {:x 0.6666666666666666, :y 0.0} {:x 0.7333333333333333, :y 0.0} {:x 0.7999999999999999, :y 0.0} {:x 0.8666666666666666, :y 0.0} {:x 0.9333333333333332, :y 0.0} {:x 0.9999999999999999, :y 0.0} {:x 1.0666666666666667, :y 0.7063} {:x 1.1333333333333333, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@
(defquery bayes-net-2 [] 
  (let [is-cloudy (sample (flip 0.5))
        
        is-raining (cond (= is-cloudy true ) 
                           (sample (flip 0.8))
                         (= is-cloudy false) 
                           (sample (flip 0.2)))
        
        sprinkler  (cond (= is-cloudy true ) 
                           (sample (flip 0.1))
                         (= is-cloudy false) 
                           (sample (flip 0.5)))
        
        wet-grass (cond (and (= sprinkler true) 
                             (= is-raining true))  								  				       (flip 0.99)
                        (and (= sprinkler false) 
                             (= is-raining false))  								  				   (flip 0.0)
                        (or  (= sprinkler true) 
                             (= is-raining true))  								  					   (flip 0.9))]
    
           (observe wet-grass true)
    
           (predict :s (hash-map :is-cloudy  is-cloudy 
                                 :is-raining is-raining 
                                 :sprinkler  sprinkler) )))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/bayes-net-2</span>","value":"#'bayes-net/bayes-net-2"}
;; <=

;; @@
(->> (doquery :pimh bayes-net-2 nil :number-of-particles 100)
     (map get-predicts)
     (map :s)
     (map :is-raining)
     (map #(if % 1 0))
     (take 10000)
     (#(plot/histogram % :normalize :probability))
     )
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"709e413c-b9a2-4f38-aaac-64de9b12a7db","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"709e413c-b9a2-4f38-aaac-64de9b12a7db","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"709e413c-b9a2-4f38-aaac-64de9b12a7db"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"709e413c-b9a2-4f38-aaac-64de9b12a7db","values":[{"x":0.0,"y":0},{"x":0.06666666666666668,"y":0.2945},{"x":0.13333333333333336,"y":0.0},{"x":0.20000000000000004,"y":0.0},{"x":0.2666666666666667,"y":0.0},{"x":0.33333333333333337,"y":0.0},{"x":0.4,"y":0.0},{"x":0.4666666666666667,"y":0.0},{"x":0.5333333333333333,"y":0.0},{"x":0.6,"y":0.0},{"x":0.6666666666666666,"y":0.0},{"x":0.7333333333333333,"y":0.0},{"x":0.7999999999999999,"y":0.0},{"x":0.8666666666666666,"y":0.0},{"x":0.9333333333333332,"y":0.0},{"x":0.9999999999999999,"y":0.0},{"x":1.0666666666666667,"y":0.7055},{"x":1.1333333333333333,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"709e413c-b9a2-4f38-aaac-64de9b12a7db\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"709e413c-b9a2-4f38-aaac-64de9b12a7db\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"709e413c-b9a2-4f38-aaac-64de9b12a7db\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"709e413c-b9a2-4f38-aaac-64de9b12a7db\", :values ({:x 0.0, :y 0} {:x 0.06666666666666668, :y 0.2945} {:x 0.13333333333333336, :y 0.0} {:x 0.20000000000000004, :y 0.0} {:x 0.2666666666666667, :y 0.0} {:x 0.33333333333333337, :y 0.0} {:x 0.4, :y 0.0} {:x 0.4666666666666667, :y 0.0} {:x 0.5333333333333333, :y 0.0} {:x 0.6, :y 0.0} {:x 0.6666666666666666, :y 0.0} {:x 0.7333333333333333, :y 0.0} {:x 0.7999999999999999, :y 0.0} {:x 0.8666666666666666, :y 0.0} {:x 0.9333333333333332, :y 0.0} {:x 0.9999999999999999, :y 0.0} {:x 1.0666666666666667, :y 0.7055} {:x 1.1333333333333333, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@
(defquery bayes-net-3 [] 
  (let [is-cloudy (sample (flip 0.5))
        
        is-raining (cond (= is-cloudy true ) 
                           (sample (flip 0.8))
                         (= is-cloudy false) 
                           (sample (flip 0.2)))
        sprinkler-dist (cond (= is-cloudy true) 
                            	(flip 0.1)
                         	(= is-cloudy false) 
                            	(flip 0.5))
        ;_ (prn sprinkler-dist)
        ;_ (prn (sample sprinkler-dist))
        ;_ (prn ["output" (observe (normal 1 1) 10)])
        sprinkler true
        _ (observe sprinkler-dist sprinkler)
        
        ;_ (prn sprinkler)
        
        wet-grass (cond (and (= sprinkler true) 
                             (= is-raining true))  								  				       (flip 0.99)
                        (and (= sprinkler false) 
                             (= is-raining false))  								  				   (flip 0.0)
                        (or  (= sprinkler true) 
                             (= is-raining true))  								  					   (flip 0.9))]
    
           (observe wet-grass true)
    
           (predict :s (hash-map :is-cloudy  is-cloudy 
                                 :is-raining is-raining 
                                 :sprinkler  sprinkler) )))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/bayes-net-3</span>","value":"#'bayes-net/bayes-net-3"}
;; <=

;; @@
(->> (doquery :pimh bayes-net-3 nil :number-of-particles 10)
     (map get-predicts)
     (map :s)
     (map :is-raining)
     (map #(if % 1 0))
     (take 2000)
     (#(plot/histogram % :normalize :probability))
     )
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"4dd642c1-5de1-49d6-a28f-3a32b651d04c","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"4dd642c1-5de1-49d6-a28f-3a32b651d04c","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"4dd642c1-5de1-49d6-a28f-3a32b651d04c"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"4dd642c1-5de1-49d6-a28f-3a32b651d04c","values":[{"x":0.0,"y":0},{"x":0.08333333333333336,"y":0.702},{"x":0.1666666666666667,"y":0.0},{"x":0.25000000000000006,"y":0.0},{"x":0.3333333333333334,"y":0.0},{"x":0.4166666666666668,"y":0.0},{"x":0.5000000000000001,"y":0.0},{"x":0.5833333333333335,"y":0.0},{"x":0.6666666666666669,"y":0.0},{"x":0.7500000000000002,"y":0.0},{"x":0.8333333333333336,"y":0.0},{"x":0.916666666666667,"y":0.0},{"x":1.0000000000000002,"y":0.298},{"x":1.0833333333333335,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"4dd642c1-5de1-49d6-a28f-3a32b651d04c\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"4dd642c1-5de1-49d6-a28f-3a32b651d04c\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"4dd642c1-5de1-49d6-a28f-3a32b651d04c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"4dd642c1-5de1-49d6-a28f-3a32b651d04c\", :values ({:x 0.0, :y 0} {:x 0.08333333333333336, :y 0.702} {:x 0.1666666666666667, :y 0.0} {:x 0.25000000000000006, :y 0.0} {:x 0.3333333333333334, :y 0.0} {:x 0.4166666666666668, :y 0.0} {:x 0.5000000000000001, :y 0.0} {:x 0.5833333333333335, :y 0.0} {:x 0.6666666666666669, :y 0.0} {:x 0.7500000000000002, :y 0.0} {:x 0.8333333333333336, :y 0.0} {:x 0.916666666666667, :y 0.0} {:x 1.0000000000000002, :y 0.298} {:x 1.0833333333333335, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@

;; @@

;; @@

;; @@
