;; gorilla-repl.fileformat = 1

;; **
;;; # Bayes Nets
;;; 
;; **

;; @@
(ns bayes-net
  (:require [gorilla-plot.core :as plot]
            [anglican.stat :as s])
  (:use clojure.repl
        [anglican core runtime emit 
         [state :only [get-predicts]]
         [inference :only [collect-by]]]
        [clojure.string :only (join split blank?)]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Bayes nets are easy to express in Anglican.    Take for example a simple Bayes net
;;; 
;;; <img src="http://www.robots.ox.ac.uk/~fwood/anglican/examples/bayes_net/bayes_net.png" alt="Bayes net graphical model" style="width: 300px;" />.  
;;; 
;;; It's encoding as a program follows.
;;; 
;;; 
;; **

;; @@
(defquery sprinkler-bayes-net [sprinkler wet-grass]
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

    is-raining))
                        
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/sprinkler-bayes-net</span>","value":"#'bayes-net/sprinkler-bayes-net"}
;; <=

;; **
;;; Let's run the query and see what the probability of it raining is given that the sprinkler is on and the grass is wet anyway.
;; **

;; @@
(->> (doquery :smc sprinkler-bayes-net [true true] :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"720ff0f1-56a6-4940-a5c2-4976bba22e3d","values":[{"x":false,"y":0.681787458043074},{"x":true,"y":0.31821254195692605}]}],"marks":[{"type":"rect","from":{"data":"720ff0f1-56a6-4940-a5c2-4976bba22e3d"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"720ff0f1-56a6-4940-a5c2-4976bba22e3d","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"720ff0f1-56a6-4940-a5c2-4976bba22e3d","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"720ff0f1-56a6-4940-a5c2-4976bba22e3d\", :values ({:x false, :y 0.681787458043074} {:x true, :y 0.31821254195692605})}], :marks [{:type \"rect\", :from {:data \"720ff0f1-56a6-4940-a5c2-4976bba22e3d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"720ff0f1-56a6-4940-a5c2-4976bba22e3d\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"720ff0f1-56a6-4940-a5c2-4976bba22e3d\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; It looks like the probability of it raining is around 30%.
;; **

;; **
;;; This is not the only way one can write Bayes nets in Anglican.  In the following, less statistically efficient code we use  ```dirac``` to impose observations as hard constraints. However: this is inefficient because of the general purpose algorithms used; we effectively define a rejection sampler if the Bayes net is coded this way. Although this may be acceptable for small Bayes nets like this one, it is **disastrous** in larger nets, or in models with continuous random variables.
;; **

;; @@
(defdist dirac
  "Dirac distribution"
  [x] []
  (sample* [this] x)
  (observe* [this value] (if (= x value) 0.0 (- (/ 1.0 0.0)))))

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


      is-raining)))  
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/bayes-net-rejection</span>","value":"#'bayes-net/bayes-net-rejection"}
;; <=

;; @@
(->> (doquery :smc bayes-net-rejection nil :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"c41aa815-c0bb-4a1f-aa64-c14fd91221b6","values":[{"x":false,"y":0.6832953296407251},{"x":true,"y":0.316704670359275}]}],"marks":[{"type":"rect","from":{"data":"c41aa815-c0bb-4a1f-aa64-c14fd91221b6"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"c41aa815-c0bb-4a1f-aa64-c14fd91221b6","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"c41aa815-c0bb-4a1f-aa64-c14fd91221b6","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"c41aa815-c0bb-4a1f-aa64-c14fd91221b6\", :values ({:x false, :y 0.6832953296407251} {:x true, :y 0.316704670359275})}], :marks [{:type \"rect\", :from {:data \"c41aa815-c0bb-4a1f-aa64-c14fd91221b6\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c41aa815-c0bb-4a1f-aa64-c14fd91221b6\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c41aa815-c0bb-4a1f-aa64-c14fd91221b6\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; Not surprisingly this produces the indistinguishable results, however, in the case of larger Bayes nets this transformation will result in significantly lower inference performance.
;; **
