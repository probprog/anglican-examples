;; gorilla-repl.fileformat = 1

;; **
;;; # Sprinkler/rain Bayes net example
;;; 
;;; This is the very simple example of Bayesian networks. We are going to intervene in the sprinkler
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

;; @@

;; @@

;; @@

;; @@

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
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/sprinkler-bayes-net</span>","value":"#'bayes-net/sprinkler-bayes-net"}
;; <=

;; @@
(def intervened (intervene sprinkler-bayes-net sprinkler (if (true? is-cloudy) (sample (flip 1)) (sample (flip 0.5)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/intervened</span>","value":"#'bayes-net/intervened"}
;; <=

;; @@
(meta intervened) :source
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-keyword'>:source</span>","value":":source"}
;; <=

;; @@
(def intervened (intervene sprinkler-bayes-net sprinkler (sample (if (true? is-cloudy) (flip 0.5) (flip 0.8)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/intervened</span>","value":"#'bayes-net/intervened"}
;; <=

;; @@
(meta intervened) :source
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-keyword'>:source</span>","value":":source"}
;; <=

;; @@
(meta intervened)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:source</span>","value":":source"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>query</span>","value":"query"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>let</span>","value":"let"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>is-cloudy</span>","value":"is-cloudy"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>sample</span>","value":"sample"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>flip</span>","value":"flip"},{"type":"html","content":"<span class='clj-double'>0.5</span>","value":"0.5"}],"value":"(flip 0.5)"}],"value":"(sample (flip 0.5))"},{"type":"html","content":"<span class='clj-symbol'>is-raining</span>","value":"is-raining"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>cond</span>","value":"cond"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>=</span>","value":"="},{"type":"html","content":"<span class='clj-symbol'>is-cloudy</span>","value":"is-cloudy"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}],"value":"(= is-cloudy true)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>sample</span>","value":"sample"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>flip</span>","value":"flip"},{"type":"html","content":"<span class='clj-double'>0.8</span>","value":"0.8"}],"value":"(flip 0.8)"}],"value":"(sample (flip 0.8))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>=</span>","value":"="},{"type":"html","content":"<span class='clj-symbol'>is-cloudy</span>","value":"is-cloudy"},{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}],"value":"(= is-cloudy false)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>sample</span>","value":"sample"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>flip</span>","value":"flip"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"(flip 0.2)"}],"value":"(sample (flip 0.2))"}],"value":"(cond (= is-cloudy true) (sample (flip 0.8)) (= is-cloudy false) (sample (flip 0.2)))"},{"type":"html","content":"<span class='clj-symbol'>sprinkler-dist</span>","value":"sprinkler-dist"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>cond</span>","value":"cond"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>=</span>","value":"="},{"type":"html","content":"<span class='clj-symbol'>is-cloudy</span>","value":"is-cloudy"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}],"value":"(= is-cloudy true)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>flip</span>","value":"flip"},{"type":"html","content":"<span class='clj-double'>0.1</span>","value":"0.1"}],"value":"(flip 0.1)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>=</span>","value":"="},{"type":"html","content":"<span class='clj-symbol'>is-cloudy</span>","value":"is-cloudy"},{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}],"value":"(= is-cloudy false)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>flip</span>","value":"flip"},{"type":"html","content":"<span class='clj-double'>0.5</span>","value":"0.5"}],"value":"(flip 0.5)"}],"value":"(cond (= is-cloudy true) (flip 0.1) (= is-cloudy false) (flip 0.5))"},{"type":"html","content":"<span class='clj-symbol'>sprinkler</span>","value":"sprinkler"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"},{"type":"html","content":"<span class='clj-symbol'>wet-grass-dist</span>","value":"wet-grass-dist"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>cond</span>","value":"cond"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>and</span>","value":"and"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>=</span>","value":"="},{"type":"html","content":"<span class='clj-symbol'>sprinkler</span>","value":"sprinkler"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}],"value":"(= sprinkler true)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>=</span>","value":"="},{"type":"html","content":"<span class='clj-symbol'>is-raining</span>","value":"is-raining"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}],"value":"(= is-raining true)"}],"value":"(and (= sprinkler true) (= is-raining true))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>flip</span>","value":"flip"},{"type":"html","content":"<span class='clj-double'>0.99</span>","value":"0.99"}],"value":"(flip 0.99)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>and</span>","value":"and"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>=</span>","value":"="},{"type":"html","content":"<span class='clj-symbol'>sprinkler</span>","value":"sprinkler"},{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}],"value":"(= sprinkler false)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>=</span>","value":"="},{"type":"html","content":"<span class='clj-symbol'>is-raining</span>","value":"is-raining"},{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}],"value":"(= is-raining false)"}],"value":"(and (= sprinkler false) (= is-raining false))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>flip</span>","value":"flip"},{"type":"html","content":"<span class='clj-double'>0.0</span>","value":"0.0"}],"value":"(flip 0.0)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>or</span>","value":"or"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>=</span>","value":"="},{"type":"html","content":"<span class='clj-symbol'>sprinkler</span>","value":"sprinkler"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}],"value":"(= sprinkler true)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>=</span>","value":"="},{"type":"html","content":"<span class='clj-symbol'>is-raining</span>","value":"is-raining"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}],"value":"(= is-raining true)"}],"value":"(or (= sprinkler true) (= is-raining true))"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>flip</span>","value":"flip"},{"type":"html","content":"<span class='clj-double'>0.7</span>","value":"0.7"}],"value":"(flip 0.7)"}],"value":"(cond (and (= sprinkler true) (= is-raining true)) (flip 0.99) (and (= sprinkler false) (= is-raining false)) (flip 0.0) (or (= sprinkler true) (= is-raining true)) (flip 0.7))"}],"value":"[is-cloudy (sample (flip 0.5)) is-raining (cond (= is-cloudy true) (sample (flip 0.8)) (= is-cloudy false) (sample (flip 0.2))) sprinkler-dist (cond (= is-cloudy true) (flip 0.1) (= is-cloudy false) (flip 0.5)) sprinkler true wet-grass-dist (cond (and (= sprinkler true) (= is-raining true)) (flip 0.99) (and (= sprinkler false) (= is-raining false)) (flip 0.0) (or (= sprinkler true) (= is-raining true)) (flip 0.7))]"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-symbol'>observe</span>","value":"observe"},{"type":"html","content":"<span class='clj-symbol'>wet-grass-dist</span>","value":"wet-grass-dist"},{"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}],"value":"(observe wet-grass-dist true)"},{"type":"html","content":"<span class='clj-symbol'>is-raining</span>","value":"is-raining"}],"value":"(let [is-cloudy (sample (flip 0.5)) is-raining (cond (= is-cloudy true) (sample (flip 0.8)) (= is-cloudy false) (sample (flip 0.2))) sprinkler-dist (cond (= is-cloudy true) (flip 0.1) (= is-cloudy false) (flip 0.5)) sprinkler true wet-grass-dist (cond (and (= sprinkler true) (= is-raining true)) (flip 0.99) (and (= sprinkler false) (= is-raining false)) (flip 0.0) (or (= sprinkler true) (= is-raining true)) (flip 0.7))] (observe wet-grass-dist true) is-raining)"}],"value":"(query (let [is-cloudy (sample (flip 0.5)) is-raining (cond (= is-cloudy true) (sample (flip 0.8)) (= is-cloudy false) (sample (flip 0.2))) sprinkler-dist (cond (= is-cloudy true) (flip 0.1) (= is-cloudy false) (flip 0.5)) sprinkler true wet-grass-dist (cond (and (= sprinkler true) (= is-raining true)) (flip 0.99) (and (= sprinkler false) (= is-raining false)) (flip 0.0) (or (= sprinkler true) (= is-raining true)) (flip 0.7))] (observe wet-grass-dist true) is-raining))"}],"value":"[:source (query (let [is-cloudy (sample (flip 0.5)) is-raining (cond (= is-cloudy true) (sample (flip 0.8)) (= is-cloudy false) (sample (flip 0.2))) sprinkler-dist (cond (= is-cloudy true) (flip 0.1) (= is-cloudy false) (flip 0.5)) sprinkler true wet-grass-dist (cond (and (= sprinkler true) (= is-raining true)) (flip 0.99) (and (= sprinkler false) (= is-raining false)) (flip 0.0) (or (= sprinkler true) (= is-raining true)) (flip 0.7))] (observe wet-grass-dist true) is-raining))]"}],"value":"{:source (query (let [is-cloudy (sample (flip 0.5)) is-raining (cond (= is-cloudy true) (sample (flip 0.8)) (= is-cloudy false) (sample (flip 0.2))) sprinkler-dist (cond (= is-cloudy true) (flip 0.1) (= is-cloudy false) (flip 0.5)) sprinkler true wet-grass-dist (cond (and (= sprinkler true) (= is-raining true)) (flip 0.99) (and (= sprinkler false) (= is-raining false)) (flip 0.0) (or (= sprinkler true) (= is-raining true)) (flip 0.7))] (observe wet-grass-dist true) is-raining))}"}
;; <=

;; @@
(def intervened (intervene sprinkler-bayes-net sprinkler (if (true? is-raining) true false)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/intervened</span>","value":"#'bayes-net/intervened"}
;; <=

;; @@
(->> (doquery :smc sprinkler-bayes-net [] :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"43423180-b130-40cb-87cb-3232c32d6230","values":[{"x":true,"y":0.7232705769135304},{"x":false,"y":0.27672942308646964}]}],"marks":[{"type":"rect","from":{"data":"43423180-b130-40cb-87cb-3232c32d6230"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"43423180-b130-40cb-87cb-3232c32d6230","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"43423180-b130-40cb-87cb-3232c32d6230","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"43423180-b130-40cb-87cb-3232c32d6230\", :values ({:x true, :y 0.7232705769135304} {:x false, :y 0.27672942308646964})}], :marks [{:type \"rect\", :from {:data \"43423180-b130-40cb-87cb-3232c32d6230\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"43423180-b130-40cb-87cb-3232c32d6230\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"43423180-b130-40cb-87cb-3232c32d6230\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@
(defn parse-and-replace-let [target-variable new-value [variable value & other]]
  (if (= target-variable variable)
    (concat [target-variable new-value] other)
    (concat [variable value] (if (empty? other) [] (parse-and-replace-let target-variable new-value other)))))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/parse-and-replace-let</span>","value":"#'bayes-net/parse-and-replace-let"}
;; <=

;; @@
(defn intervene-on-code [target-variable value code]
  (let [myself (fn [c] (intervene-on-code target-variable value c))]
    (if (seq? code)
          (case (count code)
            0 '()
            1  (myself (nth code 1))
            (let [[stament bindings & other] code]
              (if (= stament 'let)
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
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/intervene-on-code</span>","value":"#'bayes-net/intervene-on-code"}
;; <=

;; @@
(defmacro intervene [function target-variable value]
   	(let [code ((meta (eval function)) :source)
         modified (intervene-on-code target-variable value code)
         new_function (eval modified)
         ]
    ^{:source modified} new_function)
  )
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/intervene</span>","value":"#'bayes-net/intervene"}
;; <=

;; **
;;; ##Intervene on sprinkler
;;; What is the probability of a rain given that we observe that the grass is wet and we intervened in the sprinkpler?
;;; The sprinkler is always on
;;; 
;;; The magic happens in here:
;;; ```clojure
;;; 	modified (intervene 'sprinkler true code)
;;; ```
;;; In that line of code we are replacing the definition of the variable 'sprinkler with a true
;;; 
;; **

;; @@
(macroexpand '(intervene sprinkler-bayes-net sprinkler true))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#function[clojure.lang.AFunction/1]</span>","value":"#function[clojure.lang.AFunction/1]"}
;; <=

;; @@
(meta sprinkler-bayes-net) :source
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-keyword'>:source</span>","value":":source"}
;; <=

;; @@
(meta intervened) :source
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-keyword'>:source</span>","value":":source"}
;; <=

;; @@
(def intervened (intervene sprinkler-bayes-net sprinkler (sample (if (true? is-raining) (flip 0.5) (flip 0.0)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/intervened</span>","value":"#'bayes-net/intervened"}
;; <=

;; @@
(->> (doquery :smc intervened [] :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"222fc1ca-b519-4477-b49a-3713caa4f472","values":[{"x":true,"y":1.0}]}],"marks":[{"type":"rect","from":{"data":"222fc1ca-b519-4477-b49a-3713caa4f472"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"222fc1ca-b519-4477-b49a-3713caa4f472","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"222fc1ca-b519-4477-b49a-3713caa4f472","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"222fc1ca-b519-4477-b49a-3713caa4f472\", :values ({:x true, :y 1.0})}], :marks [{:type \"rect\", :from {:data \"222fc1ca-b519-4477-b49a-3713caa4f472\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"222fc1ca-b519-4477-b49a-3713caa4f472\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"222fc1ca-b519-4477-b49a-3713caa4f472\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; ##Conditioning on sprinkler
;;; What is the probability of a rain given that we observe that the grass is wet and the sprinkler is on?
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
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;bayes-net/sprinkler-conditioning-on-sprinkler</span>","value":"#'bayes-net/sprinkler-conditioning-on-sprinkler"}
;; <=

;; @@
(->> (doquery :smc sprinkler-conditioning-on-sprinkler [] :number-of-particles 100)
     (take 10000)
     (collect-by :result)
     (s/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"1d08cc9f-b022-445e-80bb-a3d76fe70958","values":[{"x":false,"y":0.49743090931165007},{"x":true,"y":0.5025690906883501}]}],"marks":[{"type":"rect","from":{"data":"1d08cc9f-b022-445e-80bb-a3d76fe70958"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"1d08cc9f-b022-445e-80bb-a3d76fe70958","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"1d08cc9f-b022-445e-80bb-a3d76fe70958","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"1d08cc9f-b022-445e-80bb-a3d76fe70958\", :values ({:x false, :y 0.49743090931165007} {:x true, :y 0.5025690906883501})}], :marks [{:type \"rect\", :from {:data \"1d08cc9f-b022-445e-80bb-a3d76fe70958\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"1d08cc9f-b022-445e-80bb-a3d76fe70958\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"1d08cc9f-b022-445e-80bb-a3d76fe70958\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@

;; @@
