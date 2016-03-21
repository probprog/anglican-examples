;; gorilla-repl.fileformat = 1

;; **
;;; # Bayes Nets
;;; 
;; **

;; @@
(ns bayes-net
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [anglican core runtime emit [state :only [get-predicts]]]
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
;;; Note that in the immediately following code we use  ```dirac``` to inject observations as hard constraints. However: this is inefficient because of the general purpose algorithms used; we effectively define a rejection sampler if the Bayes net is coded this way. Although this may be acceptable for small Bayes nets like this one, it is **disastrous** in larger nets, or in models with continuous random variables.
;;; 
;;; We will demonstrate far preferable practice after.
;; **

;; @@
(defdist dirac
  "Dirac distribution"
  [x] []
  (sample [this] x)
  (observe [this value] (if (= x value) 0.0 (- (/ 1.0 0.0)))))

(with-primitive-procedures [dirac] 
  (defquery bayes-net-rejection [] 
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


      (predict :s (hash-map :is-cloudy  is-cloudy 
                            :is-raining is-raining 
                            :sprinkler  sprinkler)))))  
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/bayes-net-rejection</span>","value":"#'bayes-net/bayes-net-rejection"}
;; <=

;; **
;;; Let's run the query and see what the probability of it raining is given that the sprinkler is on and the grass is wet anyway.
;; **

;; @@
(->> (doquery :pimh bayes-net-rejection nil :number-of-particles 100)
     (map get-predicts)
     (map :s)
     (map :is-raining)
     (map #(if % 1 0))
     (take 10000)
     (#(plot/histogram % :normalize :probability))
     )
     
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"81b6446f-14f2-49f7-bc0e-f3c8664d8b5e","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"81b6446f-14f2-49f7-bc0e-f3c8664d8b5e","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"81b6446f-14f2-49f7-bc0e-f3c8664d8b5e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"81b6446f-14f2-49f7-bc0e-f3c8664d8b5e","values":[{"x":0.0,"y":0},{"x":0.06666666666666668,"y":0.6766},{"x":0.13333333333333336,"y":0.0},{"x":0.20000000000000004,"y":0.0},{"x":0.2666666666666667,"y":0.0},{"x":0.33333333333333337,"y":0.0},{"x":0.4,"y":0.0},{"x":0.4666666666666667,"y":0.0},{"x":0.5333333333333333,"y":0.0},{"x":0.6,"y":0.0},{"x":0.6666666666666666,"y":0.0},{"x":0.7333333333333333,"y":0.0},{"x":0.7999999999999999,"y":0.0},{"x":0.8666666666666666,"y":0.0},{"x":0.9333333333333332,"y":0.0},{"x":0.9999999999999999,"y":0.0},{"x":1.0666666666666667,"y":0.3234},{"x":1.1333333333333333,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"81b6446f-14f2-49f7-bc0e-f3c8664d8b5e\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"81b6446f-14f2-49f7-bc0e-f3c8664d8b5e\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"81b6446f-14f2-49f7-bc0e-f3c8664d8b5e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"81b6446f-14f2-49f7-bc0e-f3c8664d8b5e\", :values ({:x 0.0, :y 0} {:x 0.06666666666666668, :y 0.6766} {:x 0.13333333333333336, :y 0.0} {:x 0.20000000000000004, :y 0.0} {:x 0.2666666666666667, :y 0.0} {:x 0.33333333333333337, :y 0.0} {:x 0.4, :y 0.0} {:x 0.4666666666666667, :y 0.0} {:x 0.5333333333333333, :y 0.0} {:x 0.6, :y 0.0} {:x 0.6666666666666666, :y 0.0} {:x 0.7333333333333333, :y 0.0} {:x 0.7999999999999999, :y 0.0} {:x 0.8666666666666666, :y 0.0} {:x 0.9333333333333332, :y 0.0} {:x 0.9999999999999999, :y 0.0} {:x 1.0666666666666667, :y 0.3234} {:x 1.1333333333333333, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"}
;; <=

;; **
;;; It looks like the probability of it raining is around 30%.
;;; 
;;; A method of coding this example that will result in higher inference performance looks like the following.
;;; Instead of sampling _values_ for `sprinkler` and `wet-grass`, which get compared with a `dirac` likelihood,
;;; we use the distributions as arguments to the `observe`.
;; **

;; @@
(defquery bayes-net-efficient [sprinkler wet-grass]
  (let [is-cloudy (sample (flip 0.5))

        is-raining (cond (= is-cloudy true ) 
                         (sample (flip 0.8))
                         (= is-cloudy false) 
                         (sample (flip 0.2)))
        sprinkler-dist (cond (= is-cloudy true) 
                             (flip 0.1)
                             (= is-cloudy false) 
                             (flip 0.5))
        wet-grass-dist (cond 
                         (and (= sprinkler true) 
                              (= is-raining true))  								  				       
                         (flip 0.99)
                         (and (= sprinkler false) 
                              (= is-raining false))
                         (flip 0.0)
                         (or  (= sprinkler true) 
                              (= is-raining true))
                         (flip 0.9))]
    (observe sprinkler-dist sprinkler)
    (observe wet-grass-dist wet-grass)

    (predict :s {:is-cloudy  is-cloudy 
                 :is-raining is-raining})))
                         ; :sprinkler  sprinkler) )))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/bayes-net-efficient</span>","value":"#'bayes-net/bayes-net-efficient"}
;; <=

;; @@
(->> (doquery :pimh bayes-net-efficient [true true] :number-of-particles 10)
     (map get-predicts)
     (map :s)
     (map :is-raining)
     (map #(if % 1 0))
     (take 2000)
     (#(plot/histogram % :normalize :probability))
     )
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"752777a5-411a-431b-848b-8578692ff3cc","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"752777a5-411a-431b-848b-8578692ff3cc","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"752777a5-411a-431b-848b-8578692ff3cc"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"752777a5-411a-431b-848b-8578692ff3cc","values":[{"x":0.0,"y":0},{"x":0.08333333333333336,"y":0.6735},{"x":0.1666666666666667,"y":0.0},{"x":0.25000000000000006,"y":0.0},{"x":0.3333333333333334,"y":0.0},{"x":0.4166666666666668,"y":0.0},{"x":0.5000000000000001,"y":0.0},{"x":0.5833333333333335,"y":0.0},{"x":0.6666666666666669,"y":0.0},{"x":0.7500000000000002,"y":0.0},{"x":0.8333333333333336,"y":0.0},{"x":0.916666666666667,"y":0.0},{"x":1.0000000000000002,"y":0.3265},{"x":1.0833333333333335,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"752777a5-411a-431b-848b-8578692ff3cc\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"752777a5-411a-431b-848b-8578692ff3cc\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"752777a5-411a-431b-848b-8578692ff3cc\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"752777a5-411a-431b-848b-8578692ff3cc\", :values ({:x 0.0, :y 0} {:x 0.08333333333333336, :y 0.6735} {:x 0.1666666666666667, :y 0.0} {:x 0.25000000000000006, :y 0.0} {:x 0.3333333333333334, :y 0.0} {:x 0.4166666666666668, :y 0.0} {:x 0.5000000000000001, :y 0.0} {:x 0.5833333333333335, :y 0.0} {:x 0.6666666666666669, :y 0.0} {:x 0.7500000000000002, :y 0.0} {:x 0.8333333333333336, :y 0.0} {:x 0.916666666666667, :y 0.0} {:x 1.0000000000000002, :y 0.3265} {:x 1.0833333333333335, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"}
;; <=

;; **
;;; Not surprisingly this produces the indistinguishable results, however, in the case of larger Bayes nets this transformation will result in significantly higher inference performance.
;; **
