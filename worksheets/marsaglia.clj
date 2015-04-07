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
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"4513c37f-070e-43cf-bdc9-dafee20f591b","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"4513c37f-070e-43cf-bdc9-dafee20f591b","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"4513c37f-070e-43cf-bdc9-dafee20f591b"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"4513c37f-070e-43cf-bdc9-dafee20f591b","values":[{"x":-5.0939393840176255,"y":0},{"x":-4.120815714113567,"y":0.0019},{"x":-3.147692044209508,"y":0.0045},{"x":-2.1745683743054496,"y":0.0136},{"x":-1.201444704401391,"y":0.0361},{"x":-0.22832103449733232,"y":0.082},{"x":0.7448026354067263,"y":0.1293},{"x":1.717926305310785,"y":0.1798},{"x":2.6910499752148436,"y":0.1922},{"x":3.664173645118902,"y":0.1589},{"x":4.637297315022961,"y":0.1057},{"x":5.6104209849270195,"y":0.0568},{"x":6.583544654831078,"y":0.026},{"x":7.556668324735137,"y":0.0101},{"x":8.529791994639195,"y":0.0023},{"x":9.502915664543254,"y":8.0E-4},{"x":10.476039334447313,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"4513c37f-070e-43cf-bdc9-dafee20f591b\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"4513c37f-070e-43cf-bdc9-dafee20f591b\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"4513c37f-070e-43cf-bdc9-dafee20f591b\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"4513c37f-070e-43cf-bdc9-dafee20f591b\", :values ({:x -5.0939393840176255, :y 0} {:x -4.120815714113567, :y 0.0019} {:x -3.147692044209508, :y 0.0045} {:x -2.1745683743054496, :y 0.0136} {:x -1.201444704401391, :y 0.0361} {:x -0.22832103449733232, :y 0.082} {:x 0.7448026354067263, :y 0.1293} {:x 1.717926305310785, :y 0.1798} {:x 2.6910499752148436, :y 0.1922} {:x 3.664173645118902, :y 0.1589} {:x 4.637297315022961, :y 0.1057} {:x 5.6104209849270195, :y 0.0568} {:x 6.583544654831078, :y 0.026} {:x 7.556668324735137, :y 0.0101} {:x 8.529791994639195, :y 0.0023} {:x 9.502915664543254, :y 8.0E-4} {:x 10.476039334447313, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
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
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@5110ec0c&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@5110ec0c>"}
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
;(marsaglia-normal 8 9 (prn) {})
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;marsaglia/marsaglia-normal</span>","value":"#'marsaglia/marsaglia-normal"}
;; <=

;; **
;;; What is interesting about this is that it exposes the internal random state of the `marsaglia-normal` to the inference engine.  This may or may not be a good idea depending on a number of issues.
;;; 
;;; Here is the Gaussian unknown mean example from the Anglican paper.
;; **

;; @@
(defquery unknown-mean-1 [d] 
    (let [sigma (sqrt 2)
          mu (marsaglia-normal 1 5)]
      
          (observe (normal mu sigma) 9)
          (observe (normal mu sigma) 8)

          (predict :mu mu)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;marsaglia/unknown-mean-1</span>","value":"#'marsaglia/unknown-mean-1"}
;; <=

;; **
;;; And here is the same in a more idiomatically Clojure-esq way.
;; **

;; @@
  (def data [9 8])

  (defquery unknown-mean-2 [d] 
    (let [sigma (sqrt 2)
          mu (marsaglia-normal 1 5)
          obsdist (normal mu sigma)]
          (map (fn [d] (observe obsdist d)) data)
          (predict :mu mu)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;marsaglia/unknown-mean-2</span>","value":"#'marsaglia/unknown-mean-2"}
;; <=

;; **
;;; |And here are the results.  One can use any of the available inference engines to draw samples from the conditional distribution defined by this query.
;; **

;; **
;;; 
;; **

;; @@


[:lmh :pgibbs :pcascade :smc]


(->> (doquery :lmh unknown-mean-1 nil :number-of-particles 1000)
     (map get-predicts)
     (map :mu)
     (take 10000)
     (#(plot/histogram % :normalize :probability)))

(->> (doquery :lmh unknown-mean-1 nil :number-of-particles 1000)
     (map get-predicts)
     (map :mu)
     (take 10000)
     (#(/ (reduce + %) (count %))))
      
  
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>7.210233842940729</span>","value":"7.210233842940729"}
;; <=

;; **
;;; Which is near the analytical posterior.
;; **
