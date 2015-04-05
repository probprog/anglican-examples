;; gorilla-repl.fileformat = 1

;; **
;;; # Marsaglia
;;; 
;;; In probabilistic programming systems like Anglican that support _continuous_ variables, recursion, and branching on nondeterminism it is possible to write procedures in the language itself that sample from distributions with uncountable support.  Another way of saying this is that we can denote constructive definitions of distribution over uncountable support and sample from the same.
;; **

;; @@
(ns marsaglia
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
;;; We can generate from a normal distribution in a number of ways.  A canonical method is to use the Marsaglia algorithm.
;; **

;; @@
(defn marsaglia-normal [mean var]
  (let [d (uniform-continuous -1.0 1.0)
        x (sample d)
        y (sample d)
        s (+ (* x x ) (* y y ))]
          (if (< s 1)
            (+ mean (* (sqrt var)
				       (* x (sqrt (* -2 (/ ( log s) s ))))))
            (marsaglia-normal mean var))))
     
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;marsaglia/marsaglia-normal</span>","value":"#'marsaglia/marsaglia-normal"}
;; <=

;; **
;;; We can check that this generator produces a sample distribution that is in accordance with our understanding of the normal distribution.
;; **

;; @@
(plot/histogram (repeatedly 10000 
                  (partial marsaglia-normal 2 4)) 
                :normalise :probability)

;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"ca5713b9-7f20-44a2-a0f9-c477e9de5752","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"ca5713b9-7f20-44a2-a0f9-c477e9de5752","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"ca5713b9-7f20-44a2-a0f9-c477e9de5752"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"ca5713b9-7f20-44a2-a0f9-c477e9de5752","values":[{"x":-5.783807445696503,"y":0},{"x":-4.781623183541953,"y":7.0E-4},{"x":-3.7794389213874022,"y":0.0024},{"x":-2.7772546592328515,"y":0.008},{"x":-1.7750703970783008,"y":0.0213},{"x":-0.7728861349237501,"y":0.0505},{"x":0.22929812723080056,"y":0.1037},{"x":1.2314823893853513,"y":0.1583},{"x":2.233666651539902,"y":0.1972},{"x":3.2358509136944527,"y":0.1882},{"x":4.238035175849003,"y":0.1361},{"x":5.240219438003553,"y":0.0763},{"x":6.242403700158103,"y":0.0395},{"x":7.244587962312654,"y":0.0131},{"x":8.246772224467204,"y":0.0038},{"x":9.248956486621754,"y":8.0E-4},{"x":10.251140748776304,"y":1.0E-4},{"x":11.253325010930855,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"ca5713b9-7f20-44a2-a0f9-c477e9de5752\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"ca5713b9-7f20-44a2-a0f9-c477e9de5752\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"ca5713b9-7f20-44a2-a0f9-c477e9de5752\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"ca5713b9-7f20-44a2-a0f9-c477e9de5752\", :values ({:x -5.783807445696503, :y 0} {:x -4.781623183541953, :y 7.0E-4} {:x -3.7794389213874022, :y 0.0024} {:x -2.7772546592328515, :y 0.008} {:x -1.7750703970783008, :y 0.0213} {:x -0.7728861349237501, :y 0.0505} {:x 0.22929812723080056, :y 0.1037} {:x 1.2314823893853513, :y 0.1583} {:x 2.233666651539902, :y 0.1972} {:x 3.2358509136944527, :y 0.1882} {:x 4.238035175849003, :y 0.1361} {:x 5.240219438003553, :y 0.0763} {:x 6.242403700158103, :y 0.0395} {:x 7.244587962312654, :y 0.0131} {:x 8.246772224467204, :y 0.0038} {:x 9.248956486621754, :y 8.0E-4} {:x 10.251140748776304, :y 1.0E-4} {:x 11.253325010930855, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; What is interesting, however, is that one can't immediately deduce, even in this very simple example, that the return value from this procedure is "scorable" in the sense that it can be "observed" in Anglican. 
;;; 
;;; In order to make these procedures scorable one must, for now, be able to provide a log-pdf/pmf calculator.  
;; **

;; @@
(defdist my-normal
  "univatiate normal with parameters mean and _variance_"
  [m v] []
    (sample [this] (marsaglia-normal m v))
    (observe [this value] (observe (normal m (sqrt v)) value)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@714df2c8&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@714df2c8>"}
;; <=

;; **
;;; This can now be included in any Anglican program and used as a distribution, to be observed, etc.
;;; 
;;; Alternatively we can also include the sampling procedure directly by defm'ing it.
;; **

;; @@
(defm marsaglia-normal [mean var]
  (let [d (uniform-continuous -1.0 1.0)
        x (sample d)
        y (sample d)
        s (+ (* x x ) (* y y ))]
          (if (< s 1)
            (+ mean (* (sqrt var)
				       (* x (sqrt (* -2 (/ ( log s) s ))))))
            (marsaglia-normal mean var))))
;; @@

;; **
;;; What is interesting about this is that it exposes the internal random state of the `marsaglia-normal` to the inference engine.  This may or may not be a good idea depending on a number of issues.
;;; 
;;; Here is the Gaussian unknown mean example from the Anglican paper.
;; **

;; @@

  (defquery unknown-mean [] 
    (let [sigma (sqrt 2)
          mu (marsaglia-normal 1 5)]
          (observe (normal mu sigma) 9)
          (observe (normal mu sigma) 8)
          (predict :mu mu)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;marsaglia/unknown-mean</span>","value":"#'marsaglia/unknown-mean"}
;; <=

;; **
;;; And here are the results.  One can use any of the available inference engines to draw samples from the conditional distribution defined by this query.
;; **

;; **
;;; 
;; **

;; @@


[:lmh :pgibbs :pcascade :smc]


(->> (doquery :lmh unknown-mean nil :number-of-particles 1000)
     (map get-predicts)
     (map :mu)
     (take 10000)
     (#(plot/histogram % :normalize :probability)))

(->> (doquery :lmh unknown-mean nil :number-of-particles 1000)
     (map get-predicts)
     (map :mu)
     (take 10000)
     (#(/ (reduce + %) (count %))))
      
  
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>7.205971502344663</span>","value":"7.205971502344663"}
;; <=

;; @@

;; @@
