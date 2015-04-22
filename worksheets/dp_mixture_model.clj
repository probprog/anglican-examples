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
(defm create-stick-breaking-process
  [c d]
  (let
    [
      ; V returns a procedure that picks
      ; a stick each time its called from the set of sticks lazily constructed
      ; via the closed-over two-parameter stick breaking rule
      V (mem (fn [k] (sample (beta (- 1 d) (+ c (* k d))))))

      ; sample-stick-index is a procedure that samples an index from
      ; a potentially infinite dimensional discrete distribution 
      ; lazily constructed by a stick breaking rule
      sample-stick-index (fn sample-stick-index [k] 
                           (if (sample (flip (V k))) 
                             k 
                             (sample-stick-index (+ k 1))))
      
      ; wrapping a stick breaking process to return it
      stick-breaking-process (fn [] (sample-stick-index 1))]
    stick-breaking-process))

(defquery stick-breaking
  []
  (let
    [
      ; create a stick-breaking process given concentration parameter and discount
      pick-a-stick (create-stick-breaking-process 10.0 0.0)

      ; memoize stick index given draw num.
      get-stick-index (mem (fn [i] (pick-a-stick)))]

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
;;; (8 7 6 6 8 5 9 8 6 8 5 7 9 8 7 8 6 8 8 8 6 8 5 8 7 8 6 7 8 8 6 8 6 6 6 7 7 6 7 6 6 8 7 7 6 4 5 6 6 6 6 5 6 7 8 7 6 7 4 8 6 6 8 7 6 7 8 7 6 8 6 7 8 7 6 8 8 8 6 6 9 7 7 6 5 7 8 7 5 5 5 7 9 5 8 8 6 8 8 7 9 7 9 6 5 7 8 8 6 8 8 8 8 7 8 7 7 6 5 6 7 6 7 8 8 6 6 8 6 7 7 8 6 7 8 6 5 5 4 6 9 6 6 6 8 6 7 6 7 7 7 8 7 5 6 7 6 6 7 7 8 7 6 5 7 8 8 5 7 6 9 8 7 6 7 7 6 6 7 6 7 8 7 6 5 8 6 7 6 6 6 6 7 5 9 7 6 7 7 6 4 6 7 8 7 3 6 8 7 6 9 6 4 6 6 6 7 6 8 8 7 5 6 6 7 6 6 6 8 8 7 5 7 7 7 7 7 7 6 5 6 8 7 5 6 7 7 8 5 7 7 6 8 7 6 6 7 7 9 8 7 5 7 4 8 5 6 7 7 7 4 5 9 7 8 6 5 7 6 7 9 7 6 7 5 7 6 7 9 6 7 6 6 5 7 5 4 6 6 6 6 3 9 7 6 7 7 5 3 6 6 6 7 9 5 9 8 6 6 8 6 6 4 7 5 7 6 7 6 8 6 8 7 9 7 9 6 7 9 7 5 7 8 6 9 6 8 8 6 7 8 4 8 7 6 6 6 8 7 6 6 4 7 8 7 7 7 6 6 6 5 8 6 6 6 9 6 8 6 7 7 8 7 7 8 9 7 6 8 7 8 6 7 5 4 7 9 9 6 5 8 7 5 5 8 8 7 7 6 4 7 5 7 6 6 7 6 6 6 5 8 5 6 4 5 5 6 7 8 8 7 7 7 6 6 8 6 6 7 6 6 6 8 7 7 6 9 5 7 6 6 6 5 8 6 7 8 8 6 6 8 6 6 6 7 5 8 7 5 6 6 8 5 6 4 8 7 6 7 6 8 6 6 7 4 8 8 7 8 6 8 8 7 5 9 6 7 8 7 7 8 9 4 6 8 5 7 9 8 9 6 7 7 5 8 8 8 6 7 8 6 5 7 6 6 6 8 6 7 7 6 6 6 7 7 8 5 7 9 7 6 8 9 8 9 7 8 7 7 5 7 5 6 6 6 6 7 5 9 6 6 5 6 7 7 8 4 9 6 7 5 7 8 7 6 5 6 5 6 7 6 5 8 4 8 7 7 7 7 8 4 8 8 9 7 6 7 7 6 6 8 6 5 7 5 7 8 5 8 6 8 7 7 6 7 6 7 6 8 5 7 6 5 8 9 3 6 7 9 8 7 5 6 5 8 4 6 7 9 7 6 5 7 7 7 5 7 6 8 7 9 6 6 8 6 5 6 5 3 9 7 8 6 7 9 5 4 6 7 5 7 7 6 6 7 8 6 4 6 6 4 3 6 8 7 7 4 8 7 7 7 6 7 9 8 5 6 7 7 7 5 6 8 6 7 6 8 6 6 7 5 6 8 7 5 8 8 5 7 7 8 5 8 8 5 7 4 7 8 6 7 5 7 6 7 8 8 7 7 9 7 8 7 6 6 5 6 6 9 8 9 6 7 6 6 8 8 6 8 5 8 7 7 9 5 9 8 6 8 7 5 3 5 6 8 7 6 9 8 7 6 8 7 7 6 4 8 7 6 6 8 6 7 8 9 8 7 5 8 7 7 5 7 8 7 7 8 7 8 7 7 8 8 5 6 9 7 9 8 7 9 6 5 7 8 8 8 8 7 7 7 7 5 7 9 8 6 7 7 8 6 7 6 6 8 7 8 4 7 8 5 7 7 9 7 6 6 8 6 6 9 6 6 7 8 6 8 9 7 4 6 6 7 7 4 5 7 7 5 7 8 5 8 6 6 6 7 8 5 6 4 7 7 5 4 7 6 9 8 7 7 6 8 8 7 3 5 7 6 7 7 5 8 6 9 6 8 4 6 6 4 8 7 8 7 7 8 8 5 8 7 9 6 6 7 7 6 9 7 6 8 6 5 6 7 6 8 6 5 4 6 8 8 7 6 4 8 7 6 8 3 6 6 6 7 6 6 6 5 4 7 9 9 8 7 6 5 5 7 4 3 8 7 7 8 6 7 7 8 6 7 5 3 7 9 7 7 8 5 6)
;; <-
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"157b0009-ef2a-42d9-bf6b-240fd26a1014","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"157b0009-ef2a-42d9-bf6b-240fd26a1014","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"157b0009-ef2a-42d9-bf6b-240fd26a1014"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"157b0009-ef2a-42d9-bf6b-240fd26a1014","values":[{"x":3.0,"y":0},{"x":3.545454545454546,"y":11.0},{"x":4.090909090909092,"y":38.0},{"x":4.636363636363638,"y":0.0},{"x":5.181818181818183,"y":110.0},{"x":5.727272727272729,"y":0.0},{"x":6.272727272727275,"y":282.0},{"x":6.818181818181821,"y":0.0},{"x":7.363636363636367,"y":291.0},{"x":7.909090909090913,"y":0.0},{"x":8.454545454545459,"y":204.0},{"x":9.000000000000004,"y":64.0},{"x":9.545454545454549,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"157b0009-ef2a-42d9-bf6b-240fd26a1014\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"157b0009-ef2a-42d9-bf6b-240fd26a1014\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"157b0009-ef2a-42d9-bf6b-240fd26a1014\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"157b0009-ef2a-42d9-bf6b-240fd26a1014\", :values ({:x 3.0, :y 0} {:x 3.545454545454546, :y 11.0} {:x 4.090909090909092, :y 38.0} {:x 4.636363636363638, :y 0.0} {:x 5.181818181818183, :y 110.0} {:x 5.727272727272729, :y 0.0} {:x 6.272727272727275, :y 282.0} {:x 6.818181818181821, :y 0.0} {:x 7.363636363636367, :y 291.0} {:x 7.909090909090913, :y 0.0} {:x 8.454545454545459, :y 204.0} {:x 9.000000000000004, :y 64.0} {:x 9.545454545454549, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
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
      

      ; DPmem is a procedure that takes two arguments -- the concentration
      ; to a Dirichlet process and a base sampling procedure
      ; DPmem returns a procedure 
      DPmem (fn [concentration base]
              (let [get-value-from-cache-or-sample (mem (fn [args stick-index] 
                                                          (apply base args)))
                    get-stick-picking-procedure-from-cache (mem (fn [args] 
                                                                  (create-stick-breaking-process concentration 0.0)))]
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
             (doquery :smc
                      dp-example-1
                      [input-data]
                      :number-of-particles 100))))

(def predictions (map #(get % 'prediction) samples))

(plot/histogram predictions :bins 100 :plot-range [[-8 15] :all])
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-8,15]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"bd1740c2-578d-4cbf-bb4b-37b6dc44e974","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"bd1740c2-578d-4cbf-bb4b-37b6dc44e974"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"bd1740c2-578d-4cbf-bb4b-37b6dc44e974","values":[{"x":-8.0,"y":0},{"x":-7.77,"y":3.0},{"x":-7.539999999999999,"y":4.0},{"x":-7.309999999999999,"y":7.0},{"x":-7.079999999999998,"y":0.0},{"x":-6.849999999999998,"y":6.0},{"x":-6.619999999999997,"y":2.0},{"x":-6.389999999999997,"y":2.0},{"x":-6.159999999999997,"y":2.0},{"x":-5.929999999999996,"y":6.0},{"x":-5.699999999999996,"y":4.0},{"x":-5.469999999999995,"y":4.0},{"x":-5.239999999999995,"y":7.0},{"x":-5.0099999999999945,"y":4.0},{"x":-4.779999999999994,"y":5.0},{"x":-4.549999999999994,"y":5.0},{"x":-4.319999999999993,"y":12.0},{"x":-4.089999999999993,"y":5.0},{"x":-3.8599999999999928,"y":10.0},{"x":-3.629999999999993,"y":14.0},{"x":-3.399999999999993,"y":8.0},{"x":-3.169999999999993,"y":16.0},{"x":-2.939999999999993,"y":15.0},{"x":-2.709999999999993,"y":22.0},{"x":-2.479999999999993,"y":25.0},{"x":-2.249999999999993,"y":26.0},{"x":-2.019999999999993,"y":43.0},{"x":-1.789999999999993,"y":86.0},{"x":-1.559999999999993,"y":175.0},{"x":-1.329999999999993,"y":461.0},{"x":-1.099999999999993,"y":1149.0},{"x":-0.869999999999993,"y":1632.0},{"x":-0.639999999999993,"y":1181.0},{"x":-0.40999999999999304,"y":518.0},{"x":-0.17999999999999303,"y":250.0},{"x":0.05000000000000698,"y":143.0},{"x":0.280000000000007,"y":91.0},{"x":0.510000000000007,"y":70.0},{"x":0.740000000000007,"y":68.0},{"x":0.970000000000007,"y":55.0},{"x":1.200000000000007,"y":59.0},{"x":1.430000000000007,"y":72.0},{"x":1.660000000000007,"y":65.0},{"x":1.890000000000007,"y":71.0},{"x":2.120000000000007,"y":73.0},{"x":2.350000000000007,"y":112.0},{"x":2.580000000000007,"y":169.0},{"x":2.810000000000007,"y":225.0},{"x":3.040000000000007,"y":240.0},{"x":3.270000000000007,"y":230.0},{"x":3.500000000000007,"y":165.0},{"x":3.730000000000007,"y":123.0},{"x":3.960000000000007,"y":56.0},{"x":4.1900000000000075,"y":57.0},{"x":4.420000000000008,"y":58.0},{"x":4.650000000000008,"y":52.0},{"x":4.880000000000009,"y":38.0},{"x":5.110000000000009,"y":71.0},{"x":5.34000000000001,"y":74.0},{"x":5.57000000000001,"y":83.0},{"x":5.8000000000000105,"y":74.0},{"x":6.030000000000011,"y":105.0},{"x":6.260000000000011,"y":141.0},{"x":6.490000000000012,"y":145.0},{"x":6.720000000000012,"y":117.0},{"x":6.950000000000013,"y":137.0},{"x":7.180000000000013,"y":141.0},{"x":7.4100000000000135,"y":135.0},{"x":7.640000000000014,"y":114.0},{"x":7.870000000000014,"y":93.0},{"x":8.100000000000014,"y":107.0},{"x":8.330000000000014,"y":70.0},{"x":8.560000000000015,"y":60.0},{"x":8.790000000000015,"y":46.0},{"x":9.020000000000016,"y":37.0},{"x":9.250000000000016,"y":24.0},{"x":9.480000000000016,"y":32.0},{"x":9.710000000000017,"y":15.0},{"x":9.940000000000017,"y":23.0},{"x":10.170000000000018,"y":14.0},{"x":10.400000000000018,"y":14.0},{"x":10.630000000000019,"y":5.0},{"x":10.860000000000019,"y":7.0},{"x":11.09000000000002,"y":5.0},{"x":11.32000000000002,"y":1.0},{"x":11.55000000000002,"y":2.0},{"x":11.78000000000002,"y":8.0},{"x":12.010000000000021,"y":6.0},{"x":12.240000000000022,"y":4.0},{"x":12.470000000000022,"y":2.0},{"x":12.700000000000022,"y":4.0},{"x":12.930000000000023,"y":5.0},{"x":13.160000000000023,"y":0.0},{"x":13.390000000000024,"y":4.0},{"x":13.620000000000024,"y":0.0},{"x":13.850000000000025,"y":3.0},{"x":14.080000000000025,"y":2.0},{"x":14.310000000000025,"y":4.0},{"x":14.540000000000026,"y":0.0},{"x":14.770000000000026,"y":0.0},{"x":15.000000000000027,"y":1.0},{"x":15.230000000000027,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-8 15]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"bd1740c2-578d-4cbf-bb4b-37b6dc44e974\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"bd1740c2-578d-4cbf-bb4b-37b6dc44e974\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"bd1740c2-578d-4cbf-bb4b-37b6dc44e974\", :values ({:x -8.0, :y 0} {:x -7.77, :y 3.0} {:x -7.539999999999999, :y 4.0} {:x -7.309999999999999, :y 7.0} {:x -7.079999999999998, :y 0.0} {:x -6.849999999999998, :y 6.0} {:x -6.619999999999997, :y 2.0} {:x -6.389999999999997, :y 2.0} {:x -6.159999999999997, :y 2.0} {:x -5.929999999999996, :y 6.0} {:x -5.699999999999996, :y 4.0} {:x -5.469999999999995, :y 4.0} {:x -5.239999999999995, :y 7.0} {:x -5.0099999999999945, :y 4.0} {:x -4.779999999999994, :y 5.0} {:x -4.549999999999994, :y 5.0} {:x -4.319999999999993, :y 12.0} {:x -4.089999999999993, :y 5.0} {:x -3.8599999999999928, :y 10.0} {:x -3.629999999999993, :y 14.0} {:x -3.399999999999993, :y 8.0} {:x -3.169999999999993, :y 16.0} {:x -2.939999999999993, :y 15.0} {:x -2.709999999999993, :y 22.0} {:x -2.479999999999993, :y 25.0} {:x -2.249999999999993, :y 26.0} {:x -2.019999999999993, :y 43.0} {:x -1.789999999999993, :y 86.0} {:x -1.559999999999993, :y 175.0} {:x -1.329999999999993, :y 461.0} {:x -1.099999999999993, :y 1149.0} {:x -0.869999999999993, :y 1632.0} {:x -0.639999999999993, :y 1181.0} {:x -0.40999999999999304, :y 518.0} {:x -0.17999999999999303, :y 250.0} {:x 0.05000000000000698, :y 143.0} {:x 0.280000000000007, :y 91.0} {:x 0.510000000000007, :y 70.0} {:x 0.740000000000007, :y 68.0} {:x 0.970000000000007, :y 55.0} {:x 1.200000000000007, :y 59.0} {:x 1.430000000000007, :y 72.0} {:x 1.660000000000007, :y 65.0} {:x 1.890000000000007, :y 71.0} {:x 2.120000000000007, :y 73.0} {:x 2.350000000000007, :y 112.0} {:x 2.580000000000007, :y 169.0} {:x 2.810000000000007, :y 225.0} {:x 3.040000000000007, :y 240.0} {:x 3.270000000000007, :y 230.0} {:x 3.500000000000007, :y 165.0} {:x 3.730000000000007, :y 123.0} {:x 3.960000000000007, :y 56.0} {:x 4.1900000000000075, :y 57.0} {:x 4.420000000000008, :y 58.0} {:x 4.650000000000008, :y 52.0} {:x 4.880000000000009, :y 38.0} {:x 5.110000000000009, :y 71.0} {:x 5.34000000000001, :y 74.0} {:x 5.57000000000001, :y 83.0} {:x 5.8000000000000105, :y 74.0} {:x 6.030000000000011, :y 105.0} {:x 6.260000000000011, :y 141.0} {:x 6.490000000000012, :y 145.0} {:x 6.720000000000012, :y 117.0} {:x 6.950000000000013, :y 137.0} {:x 7.180000000000013, :y 141.0} {:x 7.4100000000000135, :y 135.0} {:x 7.640000000000014, :y 114.0} {:x 7.870000000000014, :y 93.0} {:x 8.100000000000014, :y 107.0} {:x 8.330000000000014, :y 70.0} {:x 8.560000000000015, :y 60.0} {:x 8.790000000000015, :y 46.0} {:x 9.020000000000016, :y 37.0} {:x 9.250000000000016, :y 24.0} {:x 9.480000000000016, :y 32.0} {:x 9.710000000000017, :y 15.0} {:x 9.940000000000017, :y 23.0} {:x 10.170000000000018, :y 14.0} {:x 10.400000000000018, :y 14.0} {:x 10.630000000000019, :y 5.0} {:x 10.860000000000019, :y 7.0} {:x 11.09000000000002, :y 5.0} {:x 11.32000000000002, :y 1.0} {:x 11.55000000000002, :y 2.0} {:x 11.78000000000002, :y 8.0} {:x 12.010000000000021, :y 6.0} {:x 12.240000000000022, :y 4.0} {:x 12.470000000000022, :y 2.0} {:x 12.700000000000022, :y 4.0} {:x 12.930000000000023, :y 5.0} {:x 13.160000000000023, :y 0.0} {:x 13.390000000000024, :y 4.0} {:x 13.620000000000024, :y 0.0} {:x 13.850000000000025, :y 3.0} {:x 14.080000000000025, :y 2.0} {:x 14.310000000000025, :y 4.0} {:x 14.540000000000026, :y 0.0} {:x 14.770000000000026, :y 0.0} {:x 15.000000000000027, :y 1.0} {:x 15.230000000000027, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
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
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-8,15]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"9d09ddf7-86fd-404c-bf3d-ffae313a5906","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"9d09ddf7-86fd-404c-bf3d-ffae313a5906"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"9d09ddf7-86fd-404c-bf3d-ffae313a5906","values":[{"x":-8.0,"y":0},{"x":-7.77,"y":8.0},{"x":-7.539999999999999,"y":6.0},{"x":-7.309999999999999,"y":11.0},{"x":-7.079999999999998,"y":11.0},{"x":-6.849999999999998,"y":16.0},{"x":-6.619999999999997,"y":24.0},{"x":-6.389999999999997,"y":22.0},{"x":-6.159999999999997,"y":33.0},{"x":-5.929999999999996,"y":30.0},{"x":-5.699999999999996,"y":48.0},{"x":-5.469999999999995,"y":48.0},{"x":-5.239999999999995,"y":38.0},{"x":-5.0099999999999945,"y":38.0},{"x":-4.779999999999994,"y":48.0},{"x":-4.549999999999994,"y":55.0},{"x":-4.319999999999993,"y":56.0},{"x":-4.089999999999993,"y":37.0},{"x":-3.8599999999999928,"y":38.0},{"x":-3.629999999999993,"y":39.0},{"x":-3.399999999999993,"y":34.0},{"x":-3.169999999999993,"y":24.0},{"x":-2.939999999999993,"y":29.0},{"x":-2.709999999999993,"y":31.0},{"x":-2.479999999999993,"y":26.0},{"x":-2.249999999999993,"y":18.0},{"x":-2.019999999999993,"y":13.0},{"x":-1.789999999999993,"y":14.0},{"x":-1.559999999999993,"y":30.0},{"x":-1.329999999999993,"y":116.0},{"x":-1.099999999999993,"y":334.0},{"x":-0.869999999999993,"y":641.0},{"x":-0.639999999999993,"y":580.0},{"x":-0.40999999999999304,"y":279.0},{"x":-0.17999999999999303,"y":88.0},{"x":0.05000000000000698,"y":33.0},{"x":0.280000000000007,"y":31.0},{"x":0.510000000000007,"y":35.0},{"x":0.740000000000007,"y":43.0},{"x":0.970000000000007,"y":52.0},{"x":1.200000000000007,"y":64.0},{"x":1.430000000000007,"y":66.0},{"x":1.660000000000007,"y":93.0},{"x":1.890000000000007,"y":117.0},{"x":2.120000000000007,"y":154.0},{"x":2.350000000000007,"y":158.0},{"x":2.580000000000007,"y":291.0},{"x":2.810000000000007,"y":734.0},{"x":3.040000000000007,"y":1258.0},{"x":3.270000000000007,"y":747.0},{"x":3.500000000000007,"y":234.0},{"x":3.730000000000007,"y":76.0},{"x":3.960000000000007,"y":37.0},{"x":4.1900000000000075,"y":33.0},{"x":4.420000000000008,"y":23.0},{"x":4.650000000000008,"y":46.0},{"x":4.880000000000009,"y":64.0},{"x":5.110000000000009,"y":81.0},{"x":5.34000000000001,"y":112.0},{"x":5.57000000000001,"y":128.0},{"x":5.8000000000000105,"y":147.0},{"x":6.030000000000011,"y":179.0},{"x":6.260000000000011,"y":208.0},{"x":6.490000000000012,"y":219.0},{"x":6.720000000000012,"y":215.0},{"x":6.950000000000013,"y":240.0},{"x":7.180000000000013,"y":228.0},{"x":7.4100000000000135,"y":215.0},{"x":7.640000000000014,"y":191.0},{"x":7.870000000000014,"y":152.0},{"x":8.100000000000014,"y":120.0},{"x":8.330000000000014,"y":78.0},{"x":8.560000000000015,"y":61.0},{"x":8.790000000000015,"y":53.0},{"x":9.020000000000016,"y":40.0},{"x":9.250000000000016,"y":17.0},{"x":9.480000000000016,"y":10.0},{"x":9.710000000000017,"y":6.0},{"x":9.940000000000017,"y":3.0},{"x":10.170000000000018,"y":3.0},{"x":10.400000000000018,"y":1.0},{"x":10.630000000000019,"y":0.0},{"x":10.860000000000019,"y":0.0},{"x":11.09000000000002,"y":0.0},{"x":11.32000000000002,"y":0.0},{"x":11.55000000000002,"y":1.0},{"x":11.78000000000002,"y":1.0},{"x":12.010000000000021,"y":2.0},{"x":12.240000000000022,"y":1.0},{"x":12.470000000000022,"y":0.0},{"x":12.700000000000022,"y":0.0},{"x":12.930000000000023,"y":0.0},{"x":13.160000000000023,"y":0.0},{"x":13.390000000000024,"y":0.0},{"x":13.620000000000024,"y":0.0},{"x":13.850000000000025,"y":0.0},{"x":14.080000000000025,"y":0.0},{"x":14.310000000000025,"y":0.0},{"x":14.540000000000026,"y":1.0},{"x":14.770000000000026,"y":0.0},{"x":15.000000000000027,"y":0.0},{"x":15.230000000000027,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-8 15]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"9d09ddf7-86fd-404c-bf3d-ffae313a5906\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"9d09ddf7-86fd-404c-bf3d-ffae313a5906\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"9d09ddf7-86fd-404c-bf3d-ffae313a5906\", :values ({:x -8.0, :y 0} {:x -7.77, :y 8.0} {:x -7.539999999999999, :y 6.0} {:x -7.309999999999999, :y 11.0} {:x -7.079999999999998, :y 11.0} {:x -6.849999999999998, :y 16.0} {:x -6.619999999999997, :y 24.0} {:x -6.389999999999997, :y 22.0} {:x -6.159999999999997, :y 33.0} {:x -5.929999999999996, :y 30.0} {:x -5.699999999999996, :y 48.0} {:x -5.469999999999995, :y 48.0} {:x -5.239999999999995, :y 38.0} {:x -5.0099999999999945, :y 38.0} {:x -4.779999999999994, :y 48.0} {:x -4.549999999999994, :y 55.0} {:x -4.319999999999993, :y 56.0} {:x -4.089999999999993, :y 37.0} {:x -3.8599999999999928, :y 38.0} {:x -3.629999999999993, :y 39.0} {:x -3.399999999999993, :y 34.0} {:x -3.169999999999993, :y 24.0} {:x -2.939999999999993, :y 29.0} {:x -2.709999999999993, :y 31.0} {:x -2.479999999999993, :y 26.0} {:x -2.249999999999993, :y 18.0} {:x -2.019999999999993, :y 13.0} {:x -1.789999999999993, :y 14.0} {:x -1.559999999999993, :y 30.0} {:x -1.329999999999993, :y 116.0} {:x -1.099999999999993, :y 334.0} {:x -0.869999999999993, :y 641.0} {:x -0.639999999999993, :y 580.0} {:x -0.40999999999999304, :y 279.0} {:x -0.17999999999999303, :y 88.0} {:x 0.05000000000000698, :y 33.0} {:x 0.280000000000007, :y 31.0} {:x 0.510000000000007, :y 35.0} {:x 0.740000000000007, :y 43.0} {:x 0.970000000000007, :y 52.0} {:x 1.200000000000007, :y 64.0} {:x 1.430000000000007, :y 66.0} {:x 1.660000000000007, :y 93.0} {:x 1.890000000000007, :y 117.0} {:x 2.120000000000007, :y 154.0} {:x 2.350000000000007, :y 158.0} {:x 2.580000000000007, :y 291.0} {:x 2.810000000000007, :y 734.0} {:x 3.040000000000007, :y 1258.0} {:x 3.270000000000007, :y 747.0} {:x 3.500000000000007, :y 234.0} {:x 3.730000000000007, :y 76.0} {:x 3.960000000000007, :y 37.0} {:x 4.1900000000000075, :y 33.0} {:x 4.420000000000008, :y 23.0} {:x 4.650000000000008, :y 46.0} {:x 4.880000000000009, :y 64.0} {:x 5.110000000000009, :y 81.0} {:x 5.34000000000001, :y 112.0} {:x 5.57000000000001, :y 128.0} {:x 5.8000000000000105, :y 147.0} {:x 6.030000000000011, :y 179.0} {:x 6.260000000000011, :y 208.0} {:x 6.490000000000012, :y 219.0} {:x 6.720000000000012, :y 215.0} {:x 6.950000000000013, :y 240.0} {:x 7.180000000000013, :y 228.0} {:x 7.4100000000000135, :y 215.0} {:x 7.640000000000014, :y 191.0} {:x 7.870000000000014, :y 152.0} {:x 8.100000000000014, :y 120.0} {:x 8.330000000000014, :y 78.0} {:x 8.560000000000015, :y 61.0} {:x 8.790000000000015, :y 53.0} {:x 9.020000000000016, :y 40.0} {:x 9.250000000000016, :y 17.0} {:x 9.480000000000016, :y 10.0} {:x 9.710000000000017, :y 6.0} {:x 9.940000000000017, :y 3.0} {:x 10.170000000000018, :y 3.0} {:x 10.400000000000018, :y 1.0} {:x 10.630000000000019, :y 0.0} {:x 10.860000000000019, :y 0.0} {:x 11.09000000000002, :y 0.0} {:x 11.32000000000002, :y 0.0} {:x 11.55000000000002, :y 1.0} {:x 11.78000000000002, :y 1.0} {:x 12.010000000000021, :y 2.0} {:x 12.240000000000022, :y 1.0} {:x 12.470000000000022, :y 0.0} {:x 12.700000000000022, :y 0.0} {:x 12.930000000000023, :y 0.0} {:x 13.160000000000023, :y 0.0} {:x 13.390000000000024, :y 0.0} {:x 13.620000000000024, :y 0.0} {:x 13.850000000000025, :y 0.0} {:x 14.080000000000025, :y 0.0} {:x 14.310000000000025, :y 0.0} {:x 14.540000000000026, :y 1.0} {:x 14.770000000000026, :y 0.0} {:x 15.000000000000027, :y 0.0} {:x 15.230000000000027, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; ## Twist
;;; 
;;; It should be apparent that modifying these DP mixture examples is straightforward.  For instance using the `DPmem` construct is is easy to build nested and hierarchical DPs.  We have not exploited conjugacy in any way suggesting that novel DP mixture models can be written simply by changing the prior (@@H@@) and the likelihood (@@F@@) in any of these examples. 
;; **

;; @@

;; @@
