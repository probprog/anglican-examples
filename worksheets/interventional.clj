;; gorilla-repl.fileformat = 1

;; **
;;; # Interventional Operator
;;; 
;;; This example shows how to implement an interventional operator (the 'do' operator from the causal literature, but the name was already taken in closure).
;;; 
;;; Once we define a model, the _do_ operator represents external interventions to the model. As we are writing the model, there is nothing that forbids us of writing the same model (queries) with the required intervention. The problem with that approach is that we end up duplicating the model’s logic. To solve this problem, we could implement the intervention as an operator, i.e., a function from the space of queries to the space of queries. 
;;; 
;;; In this Worksheet we are going to use the famous _sprinkler_ example. 
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
;;; ## Components
;;; 
;;; The operator contains three main components:
;;; 1. The actual macro: takes a query, the name of a variable and the value to use for the intervention. This value could be any arbitrary function as long as uses variables previously defined in the scope of the intervention.
;;; 1. A helper method to analyse the query looking for bindings (_let_ staments).
;;; 1. A helper method to modify the bindings and apply the intervention.
;; **

;; @@
(defn parse-and-replace-let [target-variable new-value [variable value & other]]
  (if (= target-variable variable)
    (concat [target-variable new-value] other)
    (concat [variable value] (if (empty? other) [] (parse-and-replace-let target-variable new-value other)))))


;; @@

;; @@
(defn intervene-on-code [target-variable value code]
  (let [myself (fn [c] (intervene-on-code target-variable value c))]
    (if (seq? code)
          (case (count code)
            0 '()
            1  (myself (nth code 1))
            (let [[stament bindings & other] code]
              (if (= stament 'let)
              	;TODO: use the undocumented detruscture method: http://clojure.org/guides/destructuring
                (let [new-bindings (parse-and-replace-let target-variable value bindings)
                       new-other (doall (for [element other] (myself element)))]
                   (cons stament (cons (vec new-bindings) new-other))) ;(cons new-bindings new-other)
                 (doall (for [element code] (myself element))))
              )
            )
          code)
   )
)
;; @@

;; @@
(defmacro intervene [function target-variable value]
   	(let [code ((meta (eval function)) :source)
         modified (intervene-on-code target-variable value code)
         new_function (eval modified)
         ]
    ^{:source modified} new_function)
  )
;; @@

;; **
;;; ## Sprinkler bayesian network.
;;; This example is a very simple DAG that models the state of the weather (`is-cloudy`, `is-raning`), a sprinkler device (`sprinkler-dist`, `sprinkler`), and whether the grass is wet (`wet-grass-dist`) or not. 
;;; 
;;; For this example, we are conditioning on the fact that the grass is wet, and querying the probability of raining. 
;; **

;; @@
(defquery sprinkler-bayes-net
  (let [is-cloudy (sample (flip 0.5))

        is-raining (cond (= is-cloudy true ) 
                         (sample (flip 0.8))
                         (= is-cloudy false) 
                         (sample (flip 0.2)))
        sprinkler-dist (cond (= is-cloudy true) 
                             (flip 0.1)
                             (= is-cloudy false) 
                             (flip 0.5))
        sprinkler (sample sprinkler-dist)
        wet-grass-dist (cond 
                         (and (= sprinkler true) 
                              (= is-raining true))  								  				       
                         (flip 0.99)
                         (and (= sprinkler false) 
                              (= is-raining false))
                         (flip 0.0)
                         (or  (= sprinkler true) 
                              (= is-raining true))
                         (flip 0.7))
						]
    (observe wet-grass-dist true)

    is-raining))
                        
;; @@

;; @@
(->> (doquery :smc sprinkler-bayes-net [] :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"46173b7e-6048-440e-afc7-cff3d6ea9870","values":[{"x":false,"y":0.28461377494376106},{"x":true,"y":0.7153862250562391}]}],"marks":[{"type":"rect","from":{"data":"46173b7e-6048-440e-afc7-cff3d6ea9870"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"46173b7e-6048-440e-afc7-cff3d6ea9870","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"46173b7e-6048-440e-afc7-cff3d6ea9870","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"46173b7e-6048-440e-afc7-cff3d6ea9870\", :values ({:x false, :y 0.28461377494376106} {:x true, :y 0.7153862250562391})}], :marks [{:type \"rect\", :from {:data \"46173b7e-6048-440e-afc7-cff3d6ea9870\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"46173b7e-6048-440e-afc7-cff3d6ea9870\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"46173b7e-6048-440e-afc7-cff3d6ea9870\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; ## Intervention.
;;; We could intervene the `sprinkler`. In this case, we are going to turn on the sprinkler independently of the weather condition. 
;;; This intervention creates a new `query`, and we could sample from this new query to get the distribution of `is-raining` under the intervention. 
;; **

;; @@
(def intervened (intervene sprinkler-bayes-net sprinkler true))
;; @@

;; **
;;; The source of the intervened function was modified to reflect the intervention
;; **

;; @@
(meta intervened) :source
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-keyword'>:source</span>","value":":source"}
;; <=

;; **
;;; And @@p(R|W, \text{do}(S=\text{True})) = 0.586@@
;;; as expected
;; **

;; @@
(->> (doquery :smc intervened [] :number-of-particles 100)
     (take 100000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"0d998e1e-3989-4887-91a3-0dbd7d68b87a","values":[{"x":true,"y":0.58203461895259},{"x":false,"y":0.41796538104741}]}],"marks":[{"type":"rect","from":{"data":"0d998e1e-3989-4887-91a3-0dbd7d68b87a"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"0d998e1e-3989-4887-91a3-0dbd7d68b87a","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"0d998e1e-3989-4887-91a3-0dbd7d68b87a","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"0d998e1e-3989-4887-91a3-0dbd7d68b87a\", :values ({:x true, :y 0.58203461895259} {:x false, :y 0.41796538104741})}], :marks [{:type \"rect\", :from {:data \"0d998e1e-3989-4887-91a3-0dbd7d68b87a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0d998e1e-3989-4887-91a3-0dbd7d68b87a\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0d998e1e-3989-4887-91a3-0dbd7d68b87a\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; ##Conditioning on sprinkle
;;; Additionally we could modify the original query to _observe_ the sprinkler, which is equivalent to condition on the fact that the sprinkler is on. This will show us the well known fact that the interventional distribution is not necessarily the same as the conditioned distribution.
;; **

;; @@
(defquery sprinkler-conditioning-on-sprinkler
  (let [is-cloudy (sample (flip 0.5))

        is-raining (cond (= is-cloudy true ) 
                         (sample (flip 0.8))
                         (= is-cloudy false) 
                         (sample (flip 0.2)))
        sprinkler-dist (cond (= is-cloudy true) 
                             (flip 0.1)
                             (= is-cloudy false) 
                             (flip 0.5))
        sprinkler (sample sprinkler-dist)
        wet-grass-dist (cond 
                         (and (= sprinkler true) 
                              (= is-raining true))  								  				       
                         (flip 0.99)
                         (and (= sprinkler false) 
                              (= is-raining false))
                         (flip 0.0)
                         (or  (= sprinkler true) 
                              (= is-raining true))
                         (flip 0.7))
						]
    (observe wet-grass-dist true)
    (observe sprinkler-dist true)

    is-raining))
                        
;; @@

;; @@
(->> (doquery :smc sprinkler-conditioning-on-sprinkler [] :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"681fe645-bc78-4fca-87b6-a9442e7b15ce","values":[{"x":false,"y":0.4865937266604867},{"x":true,"y":0.5134062733395134}]}],"marks":[{"type":"rect","from":{"data":"681fe645-bc78-4fca-87b6-a9442e7b15ce"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"681fe645-bc78-4fca-87b6-a9442e7b15ce","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"681fe645-bc78-4fca-87b6-a9442e7b15ce","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"681fe645-bc78-4fca-87b6-a9442e7b15ce\", :values ({:x false, :y 0.4865937266604867} {:x true, :y 0.5134062733395134})}], :marks [{:type \"rect\", :from {:data \"681fe645-bc78-4fca-87b6-a9442e7b15ce\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"681fe645-bc78-4fca-87b6-a9442e7b15ce\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"681fe645-bc78-4fca-87b6-a9442e7b15ce\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; ## More complex interventions
;;; 
;;; The power of macros in Clojure allows us to use more complex interventions. In particular, we could use any variable from the query as long as those variables were previously defined. 
;;; 
;;; In the following example, the sprinkler will be a sample that depends on the state of the `is-cloudy` variable.
;; **

;; @@
 (def intervened-complex (intervene sprinkler-bayes-net sprinkler (sample (if (true? is-cloudy) (flip 0.5) (flip 0.8)))))
;; @@

;; @@
(meta intervened-complex) :source
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-keyword'>:source</span>","value":":source"}
;; <=

;; @@
(->> (doquery :smc intervened-complex [] :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"38409b20-d0f0-4a81-afe5-fde11f75d376","values":[{"x":true,"y":0.627680345652937},{"x":false,"y":0.37231965434706304}]}],"marks":[{"type":"rect","from":{"data":"38409b20-d0f0-4a81-afe5-fde11f75d376"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"38409b20-d0f0-4a81-afe5-fde11f75d376","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"38409b20-d0f0-4a81-afe5-fde11f75d376","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"38409b20-d0f0-4a81-afe5-fde11f75d376\", :values ({:x true, :y 0.627680345652937} {:x false, :y 0.37231965434706304})}], :marks [{:type \"rect\", :from {:data \"38409b20-d0f0-4a81-afe5-fde11f75d376\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"38409b20-d0f0-4a81-afe5-fde11f75d376\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"38409b20-d0f0-4a81-afe5-fde11f75d376\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=
