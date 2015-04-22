;; gorilla-repl.fileformat = 1

;; **
;;; # Dirichlet Process Mixture Model
;; **

;; @@
(ns dp-mixture-model
  (:require [gorilla-plot.core :as plot]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:use [anglican core emit runtime [state :only [get-predicts get-log-weight]]]
        [anglib crp]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; The code and techniques on this page are related to and inspired by the original
;;; [Church example code](http://projects.csail.mit.edu/church/wiki/Non-Parametric_Models).
;;; 
;;; Bayesian mixture modeling is one way to do unsupervised clustering with an unknown number 
;;; of components.  Alternatively and perhaps more defensibly it is a nice way to do density estimation in a way that naturally bridges between parametric and non-parametric.  The  approach is to assume an infinite dimensional class distribution parameter and impose regularization that predisposes the model towards using few elements of this vector, i.e. few mixture components, to explain the observed data distribution.
;;; 
;;; The fundamental object in Dirichlet process mixture modeling is the Dirichlet process (DP).  To a first approximation the Dirichlet process is the infinite dimensional analog of the Dirichlet distribution.  Excellent introductory and review materials are available on [Yee Whye Teh](http://www.stats.ox.ac.uk/~teh/)'s [Bayesian nonparametrics website](http://www.stats.ox.ac.uk/~teh/npbayes.html).
;;; 
;;; Computation in Dirichlet process mixture modeling is dominated by two distinct yet complimentary methods for constructing and sampling from the Dirichlet process.  One is the stick breaking representation which represents the infinite dimensional vector as a recursive partitioning of the unit interval.  The other is a Polya-urn scheme that arises when the infinite dimensional vector is integrated out. 
;;; 
;;; ### DP Mixture Modeling via the Stick Breaking Construction
;;; 
;;; To begin, the mathematical formalism for a slight generalization of the Dirichlet process stick breaking construction is written as
;;; 
;;; $$ 
;;; \begin{align} 
;;; 	  V\_k &\sim \mathrm{Beta}(1-d,c + kd) \\\\
;;;       p\_k &\sim V\_k \prod_{i=1}^{k-1}(1-V_i) \\\\
;;; \end{align} 
;;; $$ 
;;; 
;;; This is a two parameter stick breaking process – the Dirichlet process stick breaking construction arises from setting @@ d=0 @@.  In Anglican this representation can be computed _lazily_ in the following way.  This code examines the number of unique classes in 10 draws from @@ p = \[p\_1, \ldots p\_\infty\] @@.
;; **

;; @@
(defquery stick-breaking
  []
  (let
    [; define the breaking rule
     V (mem (fn [c d k] (sample (beta (+ c (* k d)) (- 1 d)))))
      
     ; lazily construct new breaks as necessary when sampling discrete dist. bin sizes
     sample-from-lazy-discrete
       (fn sample-from-lazy-discrete [c d k] 
               (if (sample (flip (V c d k))) 
                 k 
                 (sample-from-lazy-discrete c d (+ k 1))))
      
     ; close over concentration parameter and discount and memoize stick index given draw num.
     get-stick-index (mem (fn [i] 
                            (let 
                              [c 1.72 
                               d 0] 
                              (sample-from-lazy-discrete c d 1))))]
    
    ; predict the number of unique indices in a sample of stick indices of size ten
    (predict 'count (count (distinct (map get-stick-index (range 1 10)))))))

(def samples
  (take 1000
        (map get-predicts
             (doquery :pgibbs
                      stick-breaking
                      []
                      :number-of-particles 100))))

(def counts (map #(get % 'count) samples))

(print counts)

(plot/histogram counts)
;; @@
;; ->
;;; (4 3 2 1 2 3 2 2 2 2 2 3 1 2 2 3 3 3 2 3 3 1 1 3 2 4 4 2 2 3 2 3 4 2 1 2 2 3 3 1 3 3 2 1 3 5 2 1 4 2 3 3 2 3 4 2 2 3 1 2 3 2 3 2 1 4 4 3 3 2 3 1 1 3 2 3 2 2 1 3 3 3 3 2 2 1 3 3 4 3 2 2 3 1 2 2 1 2 2 3 3 2 3 3 1 2 2 3 2 2 2 1 5 2 2 3 4 1 2 1 3 2 2 2 2 2 2 2 1 1 2 4 3 2 2 3 2 2 3 4 1 4 2 4 3 2 2 2 3 4 2 3 2 2 1 2 2 4 2 3 3 2 4 1 2 3 2 2 3 2 4 2 2 1 2 2 4 1 2 4 2 2 1 2 3 2 2 3 3 3 2 3 2 2 2 2 3 2 6 3 4 1 2 2 1 2 4 2 2 2 3 3 2 3 3 1 1 2 5 2 1 4 2 2 2 2 3 1 2 4 2 4 3 2 2 1 3 1 3 3 1 2 3 2 2 2 4 3 2 3 2 3 2 2 2 2 2 3 3 4 4 4 2 3 2 2 3 2 3 4 2 1 3 3 2 4 2 4 3 3 2 2 1 3 4 1 2 3 4 4 2 2 1 1 1 2 4 2 3 2 3 3 1 3 2 2 2 2 1 4 2 2 2 4 2 2 2 4 2 3 1 2 3 2 4 4 3 3 3 3 3 2 2 3 1 1 3 3 2 2 3 3 3 2 1 1 2 3 1 2 2 3 2 5 2 1 3 1 3 5 4 3 3 3 1 2 4 3 2 2 4 2 4 3 5 2 2 2 1 2 2 1 4 3 1 3 4 3 2 2 2 2 3 2 2 5 1 3 2 3 4 4 3 2 3 2 5 3 1 3 1 2 3 2 2 2 3 4 1 4 1 3 3 2 1 2 2 3 2 2 2 3 2 5 2 1 2 2 1 2 1 3 1 3 3 2 2 3 2 2 3 4 2 3 2 5 2 2 2 2 2 2 3 2 1 3 3 5 2 1 3 1 3 2 1 2 1 2 4 4 3 3 3 2 3 2 3 2 3 3 3 2 2 2 4 3 2 2 2 3 2 2 2 2 1 3 3 2 3 3 1 3 2 2 3 2 2 1 2 2 1 3 2 4 2 2 2 3 1 3 2 1 3 3 3 1 2 2 3 1 3 2 3 5 3 4 1 2 2 4 3 1 2 3 3 2 2 3 3 4 2 3 3 2 2 3 4 2 2 1 2 2 2 2 3 3 1 2 3 3 1 3 2 2 2 2 3 3 4 4 2 3 3 4 2 2 3 3 3 4 2 1 1 3 2 2 4 1 3 4 3 3 2 1 4 2 3 2 3 1 3 3 2 4 2 2 2 3 1 4 3 2 2 2 3 2 2 3 2 3 2 2 3 2 1 3 3 3 2 3 3 2 2 4 3 2 3 1 2 3 2 2 1 1 3 2 3 3 4 4 4 3 4 2 2 1 2 3 3 4 4 2 3 2 2 4 2 3 1 3 2 3 1 2 1 3 3 1 1 2 2 2 3 4 3 2 2 2 3 1 1 2 3 2 2 2 3 5 3 3 3 3 3 3 3 3 2 3 1 2 2 2 2 4 2 1 2 3 2 3 1 1 2 4 2 2 2 3 4 2 1 2 1 2 1 1 3 1 2 3 2 1 1 3 2 3 2 3 1 2 2 1 3 2 1 2 1 3 2 2 3 2 3 2 3 5 2 2 1 5 3 3 4 3 4 2 2 3 3 4 2 2 4 3 3 2 2 2 3 3 1 3 2 3 1 1 2 1 2 2 4 2 2 1 1 3 1 2 3 4 1 3 2 2 1 3 3 1 4 3 2 4 3 3 2 3 4 3 3 2 2 2 1 2 1 3 3 3 1 3 1 3 1 1 1 2 3 3 1 2 2 3 2 1 2 2 3 3 4 4 5 2 2 1 3 1 1 2 2 2 1 5 1 4 1 2 3 2 3 2 2 2 2 1 3 2 2 2 1 2 2 2 3 2 2 3 1 1 2 2 1 1 2 2 3 2 1 4 2 4 2 3 3 3 3 2 2 4 2 2 2 1 2 3 5 2 2 2 4 3 3 3 2 1 2 1 4 2 4 2 2 2 2 3 3 1 5 1 3 4 2 2 2 1 2 2 3 1 4 3 3 4 2 3 1 2 2 3 3 2 1 4 3 3 3 2 3 2 3 1)
;; <-
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"4794f46f-6e58-4bc2-aa94-f31f064da3c1","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"4794f46f-6e58-4bc2-aa94-f31f064da3c1","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"4794f46f-6e58-4bc2-aa94-f31f064da3c1"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"4794f46f-6e58-4bc2-aa94-f31f064da3c1","values":[{"x":1.0,"y":0},{"x":1.4545454545454546,"y":161.0},{"x":1.9090909090909092,"y":0.0},{"x":2.3636363636363638,"y":412.0},{"x":2.8181818181818183,"y":0.0},{"x":3.272727272727273,"y":304.0},{"x":3.7272727272727275,"y":0.0},{"x":4.1818181818181825,"y":103.0},{"x":4.636363636363638,"y":0.0},{"x":5.090909090909093,"y":19.0},{"x":5.545454545454548,"y":0.0},{"x":6.000000000000003,"y":1.0},{"x":6.454545454545458,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"4794f46f-6e58-4bc2-aa94-f31f064da3c1\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"4794f46f-6e58-4bc2-aa94-f31f064da3c1\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"4794f46f-6e58-4bc2-aa94-f31f064da3c1\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"4794f46f-6e58-4bc2-aa94-f31f064da3c1\", :values ({:x 1.0, :y 0} {:x 1.4545454545454546, :y 161.0} {:x 1.9090909090909092, :y 0.0} {:x 2.3636363636363638, :y 412.0} {:x 2.8181818181818183, :y 0.0} {:x 3.272727272727273, :y 304.0} {:x 3.7272727272727275, :y 0.0} {:x 4.1818181818181825, :y 103.0} {:x 4.636363636363638, :y 0.0} {:x 5.090909090909093, :y 19.0} {:x 5.545454545454548, :y 0.0} {:x 6.000000000000003, :y 1.0} {:x 6.454545454545458, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; It is well established that a draw from a Dirichlet process is an infinite sum of weighted atoms; if @@G@@ is a draw Dirichlet process with with base measure @@ \theta\_k \sim H @@ then
;;; 
;;; $$
;;; G \sim \mathrm{DP}(c,H) \Leftrightarrow G = \sum\_{k=1}^\infty p\_k \delta\_{\theta\_k}
;;; $$
;;; 
;;; This correspondence leads directly to a utilization in a mixture modeling framework if the @@\theta\_k@@'s are taken to be observation distribution parameters.
;;; 
;;; If we use indicator variables @@z\_i \sim p@@ for each observation @@i \in {1, \ldots, N}@@, then, given a class assignment each observation is drawn iid from a generative mixture component with parameter @@\theta_{z\_i}@@.  The stick breaking representation of the DP is nearly a direct representation of such mixture model, albeit one with an infinite number of components.
;;; 
;;; While clearly the entire set of @@\\{p\_k\\}@@'s cannot be computed eagerly; it is entirely 
;;; reasonable to compute them lazily as we have above.  Augmenting that code with a 
;;; generative model of likelihood distribution parameters effectively completes a 
;;; program that corresponds to the DP mixture modeling using the "stick-breaking" representation.
;; **

;; @@
; some basic synthetic data

(def input-data
  (list -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
        7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7 7
        3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;dp-mixture-model/input-data</span>","value":"#'dp-mixture-model/input-data"}
;; <=

;; @@
(defquery dp-example-1
  [input-data]
  (let
    [
      ; sample-stick-index is a procedure that samples an index from
      ; a potentially infinite dimensional discrete distribution 
      ; lazily constructed by a stick breaking rule
      sample-stick-index
      (fn sample-stick-index [breaking-rule index]
        (if (sample (flip (breaking-rule index)))
          index
          (sample-stick-index breaking-rule (+ index 1))))

      ; sethuraman-stick-picking-procedure returns a procedure that picks
      ; a stick each time its called from the set of sticks lazily constructed
      ; via the closed-over one-parameter stick breaking rule
      make-sethuraman-stick-picking-procedure
      (fn [concentration]
        (let [V (mem (fn [x] (sample (beta 1.0 concentration))))]
          (fn [] (sample-stick-index V 1))))

      ; DPmem is a procedure that takes two arguments -- the concentration
      ; to a Dirichlet process and a base sampling procedure
      ; DPmem returns a procedure 
      DPmem (fn [concentration base]
              (let [get-value-from-cache-or-sample (mem (fn [args stick-index] 
                                                          (apply base args)))
                    get-stick-picking-procedure-from-cache (mem (fn [args] 
                                                                  (make-sethuraman-stick-picking-procedure concentration)))]
                (fn [& varargs]
                  ; when the returned function is called, the first thing it does is get
                  ; the cached stick breaking procedure for the passed in arguments
                  ; and _calls_ it to get an index
                  (let [index ((get-stick-picking-procedure-from-cache varargs))]
                    ; if, for the given set of arguments and just sampled index
                    ; a return value has already been computed, get it from the cache
                    ; and return it, otherwise sample a new value
                    (get-value-from-cache-or-sample varargs index)))))

      H
      (fn []
        (let
          [v (/ 1.0 (sample (gamma 1 0.1)))]
          (list (sample (normal 0 (sqrt (* v 30)))) (sqrt v))))
      
      gaussian-mixture-model-parameters (DPmem 10.0 H)]

    ; observe draws from the DP mixture
    (map
      (fn [value]
        (observe (apply normal (gaussian-mixture-model-parameters)) value))
      input-data)

    ; predict a data point from the DP mixture
    (predict 'prediction (sample (apply normal (gaussian-mixture-model-parameters))))))

(def samples
  (take 10000
        (map get-predicts
             (doquery :pgibbs
                      dp-example-1
                      [input-data]
                      :number-of-particles 100))))

(def predictions (map #(get % 'prediction) samples))

(plot/histogram predictions :bins 100 :plot-range [[-8 15] :all])
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-8,15]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"b5eb9a28-444e-4b2a-80b6-c94c8282a53e","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"b5eb9a28-444e-4b2a-80b6-c94c8282a53e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"b5eb9a28-444e-4b2a-80b6-c94c8282a53e","values":[{"x":-8.0,"y":0},{"x":-7.77,"y":1.0},{"x":-7.539999999999999,"y":4.0},{"x":-7.309999999999999,"y":3.0},{"x":-7.079999999999998,"y":4.0},{"x":-6.849999999999998,"y":1.0},{"x":-6.619999999999997,"y":2.0},{"x":-6.389999999999997,"y":2.0},{"x":-6.159999999999997,"y":7.0},{"x":-5.929999999999996,"y":5.0},{"x":-5.699999999999996,"y":5.0},{"x":-5.469999999999995,"y":5.0},{"x":-5.239999999999995,"y":7.0},{"x":-5.0099999999999945,"y":7.0},{"x":-4.779999999999994,"y":4.0},{"x":-4.549999999999994,"y":3.0},{"x":-4.319999999999993,"y":9.0},{"x":-4.089999999999993,"y":11.0},{"x":-3.8599999999999928,"y":8.0},{"x":-3.629999999999993,"y":10.0},{"x":-3.399999999999993,"y":13.0},{"x":-3.169999999999993,"y":10.0},{"x":-2.939999999999993,"y":18.0},{"x":-2.709999999999993,"y":29.0},{"x":-2.479999999999993,"y":28.0},{"x":-2.249999999999993,"y":22.0},{"x":-2.019999999999993,"y":36.0},{"x":-1.789999999999993,"y":55.0},{"x":-1.559999999999993,"y":108.0},{"x":-1.329999999999993,"y":418.0},{"x":-1.099999999999993,"y":1232.0},{"x":-0.869999999999993,"y":1662.0},{"x":-0.639999999999993,"y":986.0},{"x":-0.40999999999999304,"y":313.0},{"x":-0.17999999999999303,"y":131.0},{"x":0.05000000000000698,"y":114.0},{"x":0.280000000000007,"y":102.0},{"x":0.510000000000007,"y":100.0},{"x":0.740000000000007,"y":74.0},{"x":0.970000000000007,"y":70.0},{"x":1.200000000000007,"y":61.0},{"x":1.430000000000007,"y":70.0},{"x":1.660000000000007,"y":74.0},{"x":1.890000000000007,"y":91.0},{"x":2.120000000000007,"y":71.0},{"x":2.350000000000007,"y":96.0},{"x":2.580000000000007,"y":180.0},{"x":2.810000000000007,"y":265.0},{"x":3.040000000000007,"y":423.0},{"x":3.270000000000007,"y":373.0},{"x":3.500000000000007,"y":216.0},{"x":3.730000000000007,"y":142.0},{"x":3.960000000000007,"y":87.0},{"x":4.1900000000000075,"y":64.0},{"x":4.420000000000008,"y":57.0},{"x":4.650000000000008,"y":49.0},{"x":4.880000000000009,"y":44.0},{"x":5.110000000000009,"y":33.0},{"x":5.34000000000001,"y":36.0},{"x":5.57000000000001,"y":28.0},{"x":5.8000000000000105,"y":42.0},{"x":6.030000000000011,"y":40.0},{"x":6.260000000000011,"y":39.0},{"x":6.490000000000012,"y":78.0},{"x":6.720000000000012,"y":104.0},{"x":6.950000000000013,"y":156.0},{"x":7.180000000000013,"y":187.0},{"x":7.4100000000000135,"y":197.0},{"x":7.640000000000014,"y":169.0},{"x":7.870000000000014,"y":142.0},{"x":8.100000000000014,"y":118.0},{"x":8.330000000000014,"y":91.0},{"x":8.560000000000015,"y":88.0},{"x":8.790000000000015,"y":66.0},{"x":9.020000000000016,"y":61.0},{"x":9.250000000000016,"y":43.0},{"x":9.480000000000016,"y":50.0},{"x":9.710000000000017,"y":44.0},{"x":9.940000000000017,"y":28.0},{"x":10.170000000000018,"y":29.0},{"x":10.400000000000018,"y":25.0},{"x":10.630000000000019,"y":14.0},{"x":10.860000000000019,"y":12.0},{"x":11.09000000000002,"y":5.0},{"x":11.32000000000002,"y":11.0},{"x":11.55000000000002,"y":7.0},{"x":11.78000000000002,"y":1.0},{"x":12.010000000000021,"y":6.0},{"x":12.240000000000022,"y":3.0},{"x":12.470000000000022,"y":3.0},{"x":12.700000000000022,"y":3.0},{"x":12.930000000000023,"y":3.0},{"x":13.160000000000023,"y":2.0},{"x":13.390000000000024,"y":5.0},{"x":13.620000000000024,"y":3.0},{"x":13.850000000000025,"y":3.0},{"x":14.080000000000025,"y":3.0},{"x":14.310000000000025,"y":1.0},{"x":14.540000000000026,"y":3.0},{"x":14.770000000000026,"y":1.0},{"x":15.000000000000027,"y":0.0},{"x":15.230000000000027,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-8 15]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"b5eb9a28-444e-4b2a-80b6-c94c8282a53e\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"b5eb9a28-444e-4b2a-80b6-c94c8282a53e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"b5eb9a28-444e-4b2a-80b6-c94c8282a53e\", :values ({:x -8.0, :y 0} {:x -7.77, :y 1.0} {:x -7.539999999999999, :y 4.0} {:x -7.309999999999999, :y 3.0} {:x -7.079999999999998, :y 4.0} {:x -6.849999999999998, :y 1.0} {:x -6.619999999999997, :y 2.0} {:x -6.389999999999997, :y 2.0} {:x -6.159999999999997, :y 7.0} {:x -5.929999999999996, :y 5.0} {:x -5.699999999999996, :y 5.0} {:x -5.469999999999995, :y 5.0} {:x -5.239999999999995, :y 7.0} {:x -5.0099999999999945, :y 7.0} {:x -4.779999999999994, :y 4.0} {:x -4.549999999999994, :y 3.0} {:x -4.319999999999993, :y 9.0} {:x -4.089999999999993, :y 11.0} {:x -3.8599999999999928, :y 8.0} {:x -3.629999999999993, :y 10.0} {:x -3.399999999999993, :y 13.0} {:x -3.169999999999993, :y 10.0} {:x -2.939999999999993, :y 18.0} {:x -2.709999999999993, :y 29.0} {:x -2.479999999999993, :y 28.0} {:x -2.249999999999993, :y 22.0} {:x -2.019999999999993, :y 36.0} {:x -1.789999999999993, :y 55.0} {:x -1.559999999999993, :y 108.0} {:x -1.329999999999993, :y 418.0} {:x -1.099999999999993, :y 1232.0} {:x -0.869999999999993, :y 1662.0} {:x -0.639999999999993, :y 986.0} {:x -0.40999999999999304, :y 313.0} {:x -0.17999999999999303, :y 131.0} {:x 0.05000000000000698, :y 114.0} {:x 0.280000000000007, :y 102.0} {:x 0.510000000000007, :y 100.0} {:x 0.740000000000007, :y 74.0} {:x 0.970000000000007, :y 70.0} {:x 1.200000000000007, :y 61.0} {:x 1.430000000000007, :y 70.0} {:x 1.660000000000007, :y 74.0} {:x 1.890000000000007, :y 91.0} {:x 2.120000000000007, :y 71.0} {:x 2.350000000000007, :y 96.0} {:x 2.580000000000007, :y 180.0} {:x 2.810000000000007, :y 265.0} {:x 3.040000000000007, :y 423.0} {:x 3.270000000000007, :y 373.0} {:x 3.500000000000007, :y 216.0} {:x 3.730000000000007, :y 142.0} {:x 3.960000000000007, :y 87.0} {:x 4.1900000000000075, :y 64.0} {:x 4.420000000000008, :y 57.0} {:x 4.650000000000008, :y 49.0} {:x 4.880000000000009, :y 44.0} {:x 5.110000000000009, :y 33.0} {:x 5.34000000000001, :y 36.0} {:x 5.57000000000001, :y 28.0} {:x 5.8000000000000105, :y 42.0} {:x 6.030000000000011, :y 40.0} {:x 6.260000000000011, :y 39.0} {:x 6.490000000000012, :y 78.0} {:x 6.720000000000012, :y 104.0} {:x 6.950000000000013, :y 156.0} {:x 7.180000000000013, :y 187.0} {:x 7.4100000000000135, :y 197.0} {:x 7.640000000000014, :y 169.0} {:x 7.870000000000014, :y 142.0} {:x 8.100000000000014, :y 118.0} {:x 8.330000000000014, :y 91.0} {:x 8.560000000000015, :y 88.0} {:x 8.790000000000015, :y 66.0} {:x 9.020000000000016, :y 61.0} {:x 9.250000000000016, :y 43.0} {:x 9.480000000000016, :y 50.0} {:x 9.710000000000017, :y 44.0} {:x 9.940000000000017, :y 28.0} {:x 10.170000000000018, :y 29.0} {:x 10.400000000000018, :y 25.0} {:x 10.630000000000019, :y 14.0} {:x 10.860000000000019, :y 12.0} {:x 11.09000000000002, :y 5.0} {:x 11.32000000000002, :y 11.0} {:x 11.55000000000002, :y 7.0} {:x 11.78000000000002, :y 1.0} {:x 12.010000000000021, :y 6.0} {:x 12.240000000000022, :y 3.0} {:x 12.470000000000022, :y 3.0} {:x 12.700000000000022, :y 3.0} {:x 12.930000000000023, :y 3.0} {:x 13.160000000000023, :y 2.0} {:x 13.390000000000024, :y 5.0} {:x 13.620000000000024, :y 3.0} {:x 13.850000000000025, :y 3.0} {:x 14.080000000000025, :y 3.0} {:x 14.310000000000025, :y 1.0} {:x 14.540000000000026, :y 3.0} {:x 14.770000000000026, :y 1.0} {:x 15.000000000000027, :y 0.0} {:x 15.230000000000027, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; The code above corresponds to sampling from the posterior predictive of the following model.
;;; 
;;; $$
;;; \begin{align}
;;; G | \alpha, H &\sim \mathrm{DP}(\alpha,H) \\\\
;;; \theta\_i | G &\sim G \\\\
;;; x\_i | \theta\_i &\sim F(\theta\_i) \\\\
;;; \end{align}
;;; $$
;;; 
;;; where `concentration` @@=\alpha@@, `base` @@=H@@ is a Normal-Gamma prior on the mean and variance of the `normal` @@=F@@ observation distributions' parameters.
;; **

;; **
;;; ### DP Mixture Modeling via the Chinese Restaurant Process
;;; 
;;; A complementary method for performing DP mixture modeling is to draw the class identifiers directly using the Chinese restaurant process (CRP) (also called the Polya-urn process).  The CRP can be derived as a Dirichlet-Multinomial posterior predictive in the limit of the dimension of the multinomial probability vector going to infinity.  Anglican includes a primitive, `crp` that creates a procedure that generates sequences of CRP draws distributed according to the following rule.
;;; 
;;; $$
;;; P(c\_i = k | \mathcal{C}\_{-i}) 
;;; = 
;;; \left\\{
;;; \begin{array}{l} 
;;;        \frac{m\_k}{i-1+\alpha} \quad k \leq K\_+\\\\
;;; 	\frac{\alpha}{i-1+\alpha} \quad k>K\_+ \end{array}
;;; \right..
;;; $$
;;; 
;;; 
;;; In the above @@c\_i@@ is a class indicator variable for observation @@i@@, @@\alpha@@ is the concentration parameter of the Dirichlet process, @@m\_k@@ is the number of class indicator variables that take the value @@k@@ in the set of indicator variables up to @@i@@, and @@K\_+@@ is the maximum value of any indicator variable up to @@i@@.
;;; Constructing a DP mixture starting from the CRP is straightforward.  This code computes the predictive distribution for the same example data as the code above.
;; **

;; @@
(defquery dp-example-2
  [input-data]
  (let
    [
      ; class generateor samples a sequence of integers according to the Chinese rest. process
      class-generator (crp 10.0)
      ; lazily sample class identifiers
      get-class (mem (fn [n] (class-generator)))
      ; iid sample observation distribution parameters
      get-var (mem (fn [c] (/ 1.0 (sample (gamma 1 0.1)))))
      get-std (fn [c] (sqrt (get-var c)))
      get-mean (mem (fn [c] (sample (normal 0 (sqrt (* (get-var c) 30))))))]

    ; observe draws from the DP mixture
    (map
      (fn [value point-id]
        (observe (normal (get-mean (get-class point-id)) (get-std (get-class point-id))) value))
      input-data
      (range (count input-data)))

    ; predict a data point from the DP mixture
    (predict 'prediction (sample (normal (get-mean (get-class 'new)) (get-std (get-class 'new)))))))
    
(def samples
  (take 10000
        (map get-predicts
             (doquery :pgibbs
                      dp-example-2
                      [input-data]
                      :number-of-particles 100))))

(def predictions (map #(get % 'prediction) samples))

(plot/histogram predictions :bins 100 :plot-range [[-8 15] :all])
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-8,15]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"f4807993-040a-4c2d-b6c6-e662b7d82e66","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"f4807993-040a-4c2d-b6c6-e662b7d82e66"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"f4807993-040a-4c2d-b6c6-e662b7d82e66","values":[{"x":-8.0,"y":0},{"x":-7.77,"y":1.0},{"x":-7.539999999999999,"y":0.0},{"x":-7.309999999999999,"y":4.0},{"x":-7.079999999999998,"y":2.0},{"x":-6.849999999999998,"y":1.0},{"x":-6.619999999999997,"y":1.0},{"x":-6.389999999999997,"y":0.0},{"x":-6.159999999999997,"y":1.0},{"x":-5.929999999999996,"y":0.0},{"x":-5.699999999999996,"y":3.0},{"x":-5.469999999999995,"y":3.0},{"x":-5.239999999999995,"y":3.0},{"x":-5.0099999999999945,"y":2.0},{"x":-4.779999999999994,"y":1.0},{"x":-4.549999999999994,"y":5.0},{"x":-4.319999999999993,"y":4.0},{"x":-4.089999999999993,"y":3.0},{"x":-3.8599999999999928,"y":8.0},{"x":-3.629999999999993,"y":10.0},{"x":-3.399999999999993,"y":3.0},{"x":-3.169999999999993,"y":5.0},{"x":-2.939999999999993,"y":3.0},{"x":-2.709999999999993,"y":5.0},{"x":-2.479999999999993,"y":16.0},{"x":-2.249999999999993,"y":7.0},{"x":-2.019999999999993,"y":17.0},{"x":-1.789999999999993,"y":30.0},{"x":-1.559999999999993,"y":86.0},{"x":-1.329999999999993,"y":227.0},{"x":-1.099999999999993,"y":212.0},{"x":-0.869999999999993,"y":150.0},{"x":-0.639999999999993,"y":247.0},{"x":-0.40999999999999304,"y":348.0},{"x":-0.17999999999999303,"y":368.0},{"x":0.05000000000000698,"y":256.0},{"x":0.280000000000007,"y":295.0},{"x":0.510000000000007,"y":203.0},{"x":0.740000000000007,"y":113.0},{"x":0.970000000000007,"y":55.0},{"x":1.200000000000007,"y":34.0},{"x":1.430000000000007,"y":26.0},{"x":1.660000000000007,"y":24.0},{"x":1.890000000000007,"y":37.0},{"x":2.120000000000007,"y":49.0},{"x":2.350000000000007,"y":94.0},{"x":2.580000000000007,"y":205.0},{"x":2.810000000000007,"y":500.0},{"x":3.040000000000007,"y":928.0},{"x":3.270000000000007,"y":867.0},{"x":3.500000000000007,"y":523.0},{"x":3.730000000000007,"y":367.0},{"x":3.960000000000007,"y":240.0},{"x":4.1900000000000075,"y":148.0},{"x":4.420000000000008,"y":75.0},{"x":4.650000000000008,"y":67.0},{"x":4.880000000000009,"y":67.0},{"x":5.110000000000009,"y":81.0},{"x":5.34000000000001,"y":81.0},{"x":5.57000000000001,"y":91.0},{"x":5.8000000000000105,"y":96.0},{"x":6.030000000000011,"y":89.0},{"x":6.260000000000011,"y":125.0},{"x":6.490000000000012,"y":157.0},{"x":6.720000000000012,"y":179.0},{"x":6.950000000000013,"y":212.0},{"x":7.180000000000013,"y":235.0},{"x":7.4100000000000135,"y":239.0},{"x":7.640000000000014,"y":208.0},{"x":7.870000000000014,"y":176.0},{"x":8.100000000000014,"y":140.0},{"x":8.330000000000014,"y":119.0},{"x":8.560000000000015,"y":93.0},{"x":8.790000000000015,"y":102.0},{"x":9.020000000000016,"y":86.0},{"x":9.250000000000016,"y":85.0},{"x":9.480000000000016,"y":81.0},{"x":9.710000000000017,"y":50.0},{"x":9.940000000000017,"y":56.0},{"x":10.170000000000018,"y":50.0},{"x":10.400000000000018,"y":51.0},{"x":10.630000000000019,"y":40.0},{"x":10.860000000000019,"y":22.0},{"x":11.09000000000002,"y":19.0},{"x":11.32000000000002,"y":13.0},{"x":11.55000000000002,"y":24.0},{"x":11.78000000000002,"y":5.0},{"x":12.010000000000021,"y":8.0},{"x":12.240000000000022,"y":8.0},{"x":12.470000000000022,"y":2.0},{"x":12.700000000000022,"y":1.0},{"x":12.930000000000023,"y":3.0},{"x":13.160000000000023,"y":2.0},{"x":13.390000000000024,"y":1.0},{"x":13.620000000000024,"y":1.0},{"x":13.850000000000025,"y":1.0},{"x":14.080000000000025,"y":1.0},{"x":14.310000000000025,"y":0.0},{"x":14.540000000000026,"y":0.0},{"x":14.770000000000026,"y":1.0},{"x":15.000000000000027,"y":0.0},{"x":15.230000000000027,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-8 15]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"f4807993-040a-4c2d-b6c6-e662b7d82e66\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"f4807993-040a-4c2d-b6c6-e662b7d82e66\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"f4807993-040a-4c2d-b6c6-e662b7d82e66\", :values ({:x -8.0, :y 0} {:x -7.77, :y 1.0} {:x -7.539999999999999, :y 0.0} {:x -7.309999999999999, :y 4.0} {:x -7.079999999999998, :y 2.0} {:x -6.849999999999998, :y 1.0} {:x -6.619999999999997, :y 1.0} {:x -6.389999999999997, :y 0.0} {:x -6.159999999999997, :y 1.0} {:x -5.929999999999996, :y 0.0} {:x -5.699999999999996, :y 3.0} {:x -5.469999999999995, :y 3.0} {:x -5.239999999999995, :y 3.0} {:x -5.0099999999999945, :y 2.0} {:x -4.779999999999994, :y 1.0} {:x -4.549999999999994, :y 5.0} {:x -4.319999999999993, :y 4.0} {:x -4.089999999999993, :y 3.0} {:x -3.8599999999999928, :y 8.0} {:x -3.629999999999993, :y 10.0} {:x -3.399999999999993, :y 3.0} {:x -3.169999999999993, :y 5.0} {:x -2.939999999999993, :y 3.0} {:x -2.709999999999993, :y 5.0} {:x -2.479999999999993, :y 16.0} {:x -2.249999999999993, :y 7.0} {:x -2.019999999999993, :y 17.0} {:x -1.789999999999993, :y 30.0} {:x -1.559999999999993, :y 86.0} {:x -1.329999999999993, :y 227.0} {:x -1.099999999999993, :y 212.0} {:x -0.869999999999993, :y 150.0} {:x -0.639999999999993, :y 247.0} {:x -0.40999999999999304, :y 348.0} {:x -0.17999999999999303, :y 368.0} {:x 0.05000000000000698, :y 256.0} {:x 0.280000000000007, :y 295.0} {:x 0.510000000000007, :y 203.0} {:x 0.740000000000007, :y 113.0} {:x 0.970000000000007, :y 55.0} {:x 1.200000000000007, :y 34.0} {:x 1.430000000000007, :y 26.0} {:x 1.660000000000007, :y 24.0} {:x 1.890000000000007, :y 37.0} {:x 2.120000000000007, :y 49.0} {:x 2.350000000000007, :y 94.0} {:x 2.580000000000007, :y 205.0} {:x 2.810000000000007, :y 500.0} {:x 3.040000000000007, :y 928.0} {:x 3.270000000000007, :y 867.0} {:x 3.500000000000007, :y 523.0} {:x 3.730000000000007, :y 367.0} {:x 3.960000000000007, :y 240.0} {:x 4.1900000000000075, :y 148.0} {:x 4.420000000000008, :y 75.0} {:x 4.650000000000008, :y 67.0} {:x 4.880000000000009, :y 67.0} {:x 5.110000000000009, :y 81.0} {:x 5.34000000000001, :y 81.0} {:x 5.57000000000001, :y 91.0} {:x 5.8000000000000105, :y 96.0} {:x 6.030000000000011, :y 89.0} {:x 6.260000000000011, :y 125.0} {:x 6.490000000000012, :y 157.0} {:x 6.720000000000012, :y 179.0} {:x 6.950000000000013, :y 212.0} {:x 7.180000000000013, :y 235.0} {:x 7.4100000000000135, :y 239.0} {:x 7.640000000000014, :y 208.0} {:x 7.870000000000014, :y 176.0} {:x 8.100000000000014, :y 140.0} {:x 8.330000000000014, :y 119.0} {:x 8.560000000000015, :y 93.0} {:x 8.790000000000015, :y 102.0} {:x 9.020000000000016, :y 86.0} {:x 9.250000000000016, :y 85.0} {:x 9.480000000000016, :y 81.0} {:x 9.710000000000017, :y 50.0} {:x 9.940000000000017, :y 56.0} {:x 10.170000000000018, :y 50.0} {:x 10.400000000000018, :y 51.0} {:x 10.630000000000019, :y 40.0} {:x 10.860000000000019, :y 22.0} {:x 11.09000000000002, :y 19.0} {:x 11.32000000000002, :y 13.0} {:x 11.55000000000002, :y 24.0} {:x 11.78000000000002, :y 5.0} {:x 12.010000000000021, :y 8.0} {:x 12.240000000000022, :y 8.0} {:x 12.470000000000022, :y 2.0} {:x 12.700000000000022, :y 1.0} {:x 12.930000000000023, :y 3.0} {:x 13.160000000000023, :y 2.0} {:x 13.390000000000024, :y 1.0} {:x 13.620000000000024, :y 1.0} {:x 13.850000000000025, :y 1.0} {:x 14.080000000000025, :y 1.0} {:x 14.310000000000025, :y 0.0} {:x 14.540000000000026, :y 0.0} {:x 14.770000000000026, :y 1.0} {:x 15.000000000000027, :y 0.0} {:x 15.230000000000027, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; ## Twist
;;; 
;;; It should be apparent that modifying these DP mixture examples is straightforward.  For instance using the `DPmem` construct is is easy to build nested and hierarchical DPs.  We have not exploited conjugacy in any way suggesting that novel DP mixture models can be written simply by changing the prior (@@H@@) and the likelihood (@@F@@) in any of these examples. 
;; **

;; @@

;; @@
