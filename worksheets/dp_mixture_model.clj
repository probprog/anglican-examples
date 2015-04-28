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
(defm create-sethuraman-stick-breaking-process
  [concentration]
  (let
    [
      ; sethuraman-stick-breaking-rule returns 
      ; a stick length via the one-parameter 
      ; stick breaking rule which closes
      ; over concentration
      sethuraman-stick-breaking-rule 
        (mem (fn [k] (sample (beta 1 concentration))))

      ; sample-stick-index is a procedure that 
      ; samples an index from a potentially 
      ; infinite dimensional discrete distribution 
      ; lazily constructed by a stick breaking rule
      sample-stick-index 
        (fn sample-stick-index [k] 
          (if (sample (flip (sethuraman-stick-breaking-rule k))) 
             k 
             (sample-stick-index (+ k 1))))
      
      ; wrapping a stick breaking process to return it
      stick-breaking-process 
        (fn [] (sample-stick-index 1))]
      stick-breaking-process))

(defquery polya-urn-via-stick-breaking
  []
  (let
    [
      ; create a stick-breaking process 
      ; given concentration parameter and discount
      pick-a-stick 
        (create-sethuraman-stick-breaking-process 10.0)

      ; memoize stick index given draw num.
      get-stick-index (mem (fn [i] (pick-a-stick)))]

    ; predict the number of unique indices 
    ; in a sample of stick indices of size ten
    (predict :count 
       (count 
         (distinct 
           (map get-stick-index (range 1 10)))))))

(def samples
  (take 1000
        (map get-predicts
             (doquery :pgibbs
                      polya-urn-via-stick-breaking
                      []
                      :number-of-particles 100))))

(def counts (map :count samples))

(print (take 50 counts))

(plot/histogram counts)
;; @@
;; ->
;;; (8 8 5 6 8 6 7 7 6 7 6 7 8 6 7 4 8 5 6 6 8 8 8 6 7 5 8 8 5 8 7 8 7 8 7 6 9 5 7 7 6 7 8 4 6 6 6 9 8 8)
;; <-
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"8fbf5f6c-5d23-4040-889e-f6cfeb2309fc","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"8fbf5f6c-5d23-4040-889e-f6cfeb2309fc","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"8fbf5f6c-5d23-4040-889e-f6cfeb2309fc"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"8fbf5f6c-5d23-4040-889e-f6cfeb2309fc","values":[{"x":3.0,"y":0},{"x":3.545454545454546,"y":9.0},{"x":4.090909090909092,"y":39.0},{"x":4.636363636363638,"y":0.0},{"x":5.181818181818183,"y":138.0},{"x":5.727272727272729,"y":0.0},{"x":6.272727272727275,"y":232.0},{"x":6.818181818181821,"y":0.0},{"x":7.363636363636367,"y":311.0},{"x":7.909090909090913,"y":0.0},{"x":8.454545454545459,"y":219.0},{"x":9.000000000000004,"y":52.0},{"x":9.545454545454549,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"8fbf5f6c-5d23-4040-889e-f6cfeb2309fc\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"8fbf5f6c-5d23-4040-889e-f6cfeb2309fc\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"8fbf5f6c-5d23-4040-889e-f6cfeb2309fc\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"8fbf5f6c-5d23-4040-889e-f6cfeb2309fc\", :values ({:x 3.0, :y 0} {:x 3.545454545454546, :y 9.0} {:x 4.090909090909092, :y 39.0} {:x 4.636363636363638, :y 0.0} {:x 5.181818181818183, :y 138.0} {:x 5.727272727272729, :y 0.0} {:x 6.272727272727275, :y 232.0} {:x 6.818181818181821, :y 0.0} {:x 7.363636363636367, :y 311.0} {:x 7.909090909090913, :y 0.0} {:x 8.454545454545459, :y 219.0} {:x 9.000000000000004, :y 52.0} {:x 9.545454545454549, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
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
      ; DPmem is a procedure that takes two arguments 
      ; -- the concentration to a Dirichlet 
      ; process and a base sampling procedure.
      ; DPmem returns a procedure 
      DPmem (fn [concentration base]
              (let [get-value-from-cache-or-sample 
                     (mem 
                       (fn [args stick-index] 
                         (apply base args)))
                    get-stick-picking-procedure-from-cache 
                      (mem 
                        (fn [args] 
                           (create-sethuraman-stick-breaking-process 
                             concentration)))]
                (fn [& varargs]
                  ; when the returned function is called, 
                  ; the first thing it does is get
                  ; the cached stick breaking procedure 
                  ; for the passed in arguments
                  ; and _calls_ it to get an index
                  (let [index ((get-stick-picking-procedure-from-cache 
                                 varargs))]
                    ; if, for the given set of arguments 
                    ; and just sampled index a return value has already 
                    ; been computed, get it from the cache
                    ; and return it, otherwise sample a new value
                    (get-value-from-cache-or-sample varargs index)))))

      H (fn []
          (let [v (/ 1.0 (sample (gamma 1 0.1)))]
            (list (sample (normal 0 (sqrt (* v 30)))) 
                  (sqrt v))))

      obs-param (DPmem 10.0 H)]

    ; observe draws from the DP mixture
    (map
      (fn [value]
        (observe (apply normal (obs-param)) value))
      input-data)

    ; predict a data point from the DP mixture
    (predict :prediction (sample (apply normal (obs-param))))))

(def samples
  (take 10000
        (map get-predicts
             (doquery :pgibbs
                      dp-example-1
                      [input-data]
                      :number-of-particles 100))))

(def predictions (map #(get % :prediction) samples))

(plot/histogram predictions :bins 100 :plot-range [[-8 15] :all])
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-8,15]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"f19fea6d-85ab-4b05-a1a0-ed5c4cfa0589","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"f19fea6d-85ab-4b05-a1a0-ed5c4cfa0589"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"f19fea6d-85ab-4b05-a1a0-ed5c4cfa0589","values":[{"x":-8.0,"y":0},{"x":-7.77,"y":1.0},{"x":-7.539999999999999,"y":2.0},{"x":-7.309999999999999,"y":2.0},{"x":-7.079999999999998,"y":5.0},{"x":-6.849999999999998,"y":3.0},{"x":-6.619999999999997,"y":0.0},{"x":-6.389999999999997,"y":3.0},{"x":-6.159999999999997,"y":3.0},{"x":-5.929999999999996,"y":3.0},{"x":-5.699999999999996,"y":2.0},{"x":-5.469999999999995,"y":1.0},{"x":-5.239999999999995,"y":5.0},{"x":-5.0099999999999945,"y":6.0},{"x":-4.779999999999994,"y":6.0},{"x":-4.549999999999994,"y":9.0},{"x":-4.319999999999993,"y":6.0},{"x":-4.089999999999993,"y":4.0},{"x":-3.8599999999999928,"y":7.0},{"x":-3.629999999999993,"y":11.0},{"x":-3.399999999999993,"y":16.0},{"x":-3.169999999999993,"y":9.0},{"x":-2.939999999999993,"y":7.0},{"x":-2.709999999999993,"y":23.0},{"x":-2.479999999999993,"y":26.0},{"x":-2.249999999999993,"y":30.0},{"x":-2.019999999999993,"y":43.0},{"x":-1.789999999999993,"y":97.0},{"x":-1.559999999999993,"y":142.0},{"x":-1.329999999999993,"y":222.0},{"x":-1.099999999999993,"y":611.0},{"x":-0.869999999999993,"y":1873.0},{"x":-0.639999999999993,"y":2152.0},{"x":-0.40999999999999304,"y":747.0},{"x":-0.17999999999999303,"y":158.0},{"x":0.05000000000000698,"y":93.0},{"x":0.280000000000007,"y":70.0},{"x":0.510000000000007,"y":49.0},{"x":0.740000000000007,"y":54.0},{"x":0.970000000000007,"y":54.0},{"x":1.200000000000007,"y":43.0},{"x":1.430000000000007,"y":41.0},{"x":1.660000000000007,"y":46.0},{"x":1.890000000000007,"y":34.0},{"x":2.120000000000007,"y":46.0},{"x":2.350000000000007,"y":61.0},{"x":2.580000000000007,"y":136.0},{"x":2.810000000000007,"y":279.0},{"x":3.040000000000007,"y":495.0},{"x":3.270000000000007,"y":488.0},{"x":3.500000000000007,"y":365.0},{"x":3.730000000000007,"y":169.0},{"x":3.960000000000007,"y":83.0},{"x":4.1900000000000075,"y":37.0},{"x":4.420000000000008,"y":27.0},{"x":4.650000000000008,"y":17.0},{"x":4.880000000000009,"y":14.0},{"x":5.110000000000009,"y":20.0},{"x":5.34000000000001,"y":23.0},{"x":5.57000000000001,"y":29.0},{"x":5.8000000000000105,"y":36.0},{"x":6.030000000000011,"y":28.0},{"x":6.260000000000011,"y":33.0},{"x":6.490000000000012,"y":60.0},{"x":6.720000000000012,"y":74.0},{"x":6.950000000000013,"y":91.0},{"x":7.180000000000013,"y":107.0},{"x":7.4100000000000135,"y":127.0},{"x":7.640000000000014,"y":96.0},{"x":7.870000000000014,"y":76.0},{"x":8.100000000000014,"y":70.0},{"x":8.330000000000014,"y":43.0},{"x":8.560000000000015,"y":27.0},{"x":8.790000000000015,"y":11.0},{"x":9.020000000000016,"y":10.0},{"x":9.250000000000016,"y":6.0},{"x":9.480000000000016,"y":9.0},{"x":9.710000000000017,"y":4.0},{"x":9.940000000000017,"y":4.0},{"x":10.170000000000018,"y":6.0},{"x":10.400000000000018,"y":1.0},{"x":10.630000000000019,"y":4.0},{"x":10.860000000000019,"y":4.0},{"x":11.09000000000002,"y":3.0},{"x":11.32000000000002,"y":5.0},{"x":11.55000000000002,"y":5.0},{"x":11.78000000000002,"y":2.0},{"x":12.010000000000021,"y":2.0},{"x":12.240000000000022,"y":0.0},{"x":12.470000000000022,"y":2.0},{"x":12.700000000000022,"y":0.0},{"x":12.930000000000023,"y":1.0},{"x":13.160000000000023,"y":0.0},{"x":13.390000000000024,"y":2.0},{"x":13.620000000000024,"y":2.0},{"x":13.850000000000025,"y":1.0},{"x":14.080000000000025,"y":1.0},{"x":14.310000000000025,"y":2.0},{"x":14.540000000000026,"y":0.0},{"x":14.770000000000026,"y":0.0},{"x":15.000000000000027,"y":1.0},{"x":15.230000000000027,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-8 15]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"f19fea6d-85ab-4b05-a1a0-ed5c4cfa0589\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"f19fea6d-85ab-4b05-a1a0-ed5c4cfa0589\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"f19fea6d-85ab-4b05-a1a0-ed5c4cfa0589\", :values ({:x -8.0, :y 0} {:x -7.77, :y 1.0} {:x -7.539999999999999, :y 2.0} {:x -7.309999999999999, :y 2.0} {:x -7.079999999999998, :y 5.0} {:x -6.849999999999998, :y 3.0} {:x -6.619999999999997, :y 0.0} {:x -6.389999999999997, :y 3.0} {:x -6.159999999999997, :y 3.0} {:x -5.929999999999996, :y 3.0} {:x -5.699999999999996, :y 2.0} {:x -5.469999999999995, :y 1.0} {:x -5.239999999999995, :y 5.0} {:x -5.0099999999999945, :y 6.0} {:x -4.779999999999994, :y 6.0} {:x -4.549999999999994, :y 9.0} {:x -4.319999999999993, :y 6.0} {:x -4.089999999999993, :y 4.0} {:x -3.8599999999999928, :y 7.0} {:x -3.629999999999993, :y 11.0} {:x -3.399999999999993, :y 16.0} {:x -3.169999999999993, :y 9.0} {:x -2.939999999999993, :y 7.0} {:x -2.709999999999993, :y 23.0} {:x -2.479999999999993, :y 26.0} {:x -2.249999999999993, :y 30.0} {:x -2.019999999999993, :y 43.0} {:x -1.789999999999993, :y 97.0} {:x -1.559999999999993, :y 142.0} {:x -1.329999999999993, :y 222.0} {:x -1.099999999999993, :y 611.0} {:x -0.869999999999993, :y 1873.0} {:x -0.639999999999993, :y 2152.0} {:x -0.40999999999999304, :y 747.0} {:x -0.17999999999999303, :y 158.0} {:x 0.05000000000000698, :y 93.0} {:x 0.280000000000007, :y 70.0} {:x 0.510000000000007, :y 49.0} {:x 0.740000000000007, :y 54.0} {:x 0.970000000000007, :y 54.0} {:x 1.200000000000007, :y 43.0} {:x 1.430000000000007, :y 41.0} {:x 1.660000000000007, :y 46.0} {:x 1.890000000000007, :y 34.0} {:x 2.120000000000007, :y 46.0} {:x 2.350000000000007, :y 61.0} {:x 2.580000000000007, :y 136.0} {:x 2.810000000000007, :y 279.0} {:x 3.040000000000007, :y 495.0} {:x 3.270000000000007, :y 488.0} {:x 3.500000000000007, :y 365.0} {:x 3.730000000000007, :y 169.0} {:x 3.960000000000007, :y 83.0} {:x 4.1900000000000075, :y 37.0} {:x 4.420000000000008, :y 27.0} {:x 4.650000000000008, :y 17.0} {:x 4.880000000000009, :y 14.0} {:x 5.110000000000009, :y 20.0} {:x 5.34000000000001, :y 23.0} {:x 5.57000000000001, :y 29.0} {:x 5.8000000000000105, :y 36.0} {:x 6.030000000000011, :y 28.0} {:x 6.260000000000011, :y 33.0} {:x 6.490000000000012, :y 60.0} {:x 6.720000000000012, :y 74.0} {:x 6.950000000000013, :y 91.0} {:x 7.180000000000013, :y 107.0} {:x 7.4100000000000135, :y 127.0} {:x 7.640000000000014, :y 96.0} {:x 7.870000000000014, :y 76.0} {:x 8.100000000000014, :y 70.0} {:x 8.330000000000014, :y 43.0} {:x 8.560000000000015, :y 27.0} {:x 8.790000000000015, :y 11.0} {:x 9.020000000000016, :y 10.0} {:x 9.250000000000016, :y 6.0} {:x 9.480000000000016, :y 9.0} {:x 9.710000000000017, :y 4.0} {:x 9.940000000000017, :y 4.0} {:x 10.170000000000018, :y 6.0} {:x 10.400000000000018, :y 1.0} {:x 10.630000000000019, :y 4.0} {:x 10.860000000000019, :y 4.0} {:x 11.09000000000002, :y 3.0} {:x 11.32000000000002, :y 5.0} {:x 11.55000000000002, :y 5.0} {:x 11.78000000000002, :y 2.0} {:x 12.010000000000021, :y 2.0} {:x 12.240000000000022, :y 0.0} {:x 12.470000000000022, :y 2.0} {:x 12.700000000000022, :y 0.0} {:x 12.930000000000023, :y 1.0} {:x 13.160000000000023, :y 0.0} {:x 13.390000000000024, :y 2.0} {:x 13.620000000000024, :y 2.0} {:x 13.850000000000025, :y 1.0} {:x 14.080000000000025, :y 1.0} {:x 14.310000000000025, :y 2.0} {:x 14.540000000000026, :y 0.0} {:x 14.770000000000026, :y 0.0} {:x 15.000000000000027, :y 1.0} {:x 15.230000000000027, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
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
    [ ; class generator samples a sequence of 
      ; integers according to the Chinese rest. process
      class-generator (crp 10.0)
      ; lazily sample class identifiers
      get-class (mem (fn [n] (class-generator)))
      ; iid sample observation distribution parameters
      get-var (mem (fn [c] (/ 1.0 (sample (gamma 1 0.1)))))
      get-std (fn [c] (sqrt (get-var c)))
      get-mean (mem 
                 (fn [c] 
                   (sample (normal 0 (sqrt (* (get-var c) 30))))))]

    ; observe draws from the DP mixture
    (map
      (fn [value n]
        (observe 
          (normal (get-mean (get-class n)) 
                  (get-std  (get-class n))) value))
      input-data
      (range (count input-data)))

    ; predict a data point from the DP mixture
    (predict :prediction 
             (sample (normal (get-mean (get-class 'new)) 
                             (get-std  (get-class 'new)))))))
    

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;dp-mixture-model/dp-example-2</span>","value":"#'dp-mixture-model/dp-example-2"}
;; <=

;; @@
(def samples
  (take 10000
        (map get-predicts
             (doquery :pgibbs
                      dp-example-2
                      [input-data]
                      :number-of-particles 100))))

(def predictions (map #(get % :prediction) samples))

(plot/histogram predictions :bins 100 :plot-range [[-8 15] :all])
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-8,15]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"2049deb4-2a45-4fbf-88a0-e8c4efd1bc85","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"2049deb4-2a45-4fbf-88a0-e8c4efd1bc85"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"2049deb4-2a45-4fbf-88a0-e8c4efd1bc85","values":[{"x":-8.0,"y":0},{"x":-7.77,"y":1.0},{"x":-7.539999999999999,"y":4.0},{"x":-7.309999999999999,"y":2.0},{"x":-7.079999999999998,"y":3.0},{"x":-6.849999999999998,"y":0.0},{"x":-6.619999999999997,"y":1.0},{"x":-6.389999999999997,"y":3.0},{"x":-6.159999999999997,"y":7.0},{"x":-5.929999999999996,"y":2.0},{"x":-5.699999999999996,"y":6.0},{"x":-5.469999999999995,"y":5.0},{"x":-5.239999999999995,"y":5.0},{"x":-5.0099999999999945,"y":10.0},{"x":-4.779999999999994,"y":6.0},{"x":-4.549999999999994,"y":6.0},{"x":-4.319999999999993,"y":17.0},{"x":-4.089999999999993,"y":12.0},{"x":-3.8599999999999928,"y":12.0},{"x":-3.629999999999993,"y":17.0},{"x":-3.399999999999993,"y":15.0},{"x":-3.169999999999993,"y":9.0},{"x":-2.939999999999993,"y":13.0},{"x":-2.709999999999993,"y":22.0},{"x":-2.479999999999993,"y":17.0},{"x":-2.249999999999993,"y":27.0},{"x":-2.019999999999993,"y":18.0},{"x":-1.789999999999993,"y":29.0},{"x":-1.559999999999993,"y":46.0},{"x":-1.329999999999993,"y":109.0},{"x":-1.099999999999993,"y":532.0},{"x":-0.869999999999993,"y":861.0},{"x":-0.639999999999993,"y":534.0},{"x":-0.40999999999999304,"y":142.0},{"x":-0.17999999999999303,"y":58.0},{"x":0.05000000000000698,"y":68.0},{"x":0.280000000000007,"y":53.0},{"x":0.510000000000007,"y":54.0},{"x":0.740000000000007,"y":54.0},{"x":0.970000000000007,"y":73.0},{"x":1.200000000000007,"y":64.0},{"x":1.430000000000007,"y":67.0},{"x":1.660000000000007,"y":86.0},{"x":1.890000000000007,"y":52.0},{"x":2.120000000000007,"y":80.0},{"x":2.350000000000007,"y":170.0},{"x":2.580000000000007,"y":390.0},{"x":2.810000000000007,"y":721.0},{"x":3.040000000000007,"y":962.0},{"x":3.270000000000007,"y":844.0},{"x":3.500000000000007,"y":453.0},{"x":3.730000000000007,"y":179.0},{"x":3.960000000000007,"y":81.0},{"x":4.1900000000000075,"y":51.0},{"x":4.420000000000008,"y":36.0},{"x":4.650000000000008,"y":30.0},{"x":4.880000000000009,"y":39.0},{"x":5.110000000000009,"y":56.0},{"x":5.34000000000001,"y":82.0},{"x":5.57000000000001,"y":104.0},{"x":5.8000000000000105,"y":147.0},{"x":6.030000000000011,"y":202.0},{"x":6.260000000000011,"y":239.0},{"x":6.490000000000012,"y":324.0},{"x":6.720000000000012,"y":336.0},{"x":6.950000000000013,"y":323.0},{"x":7.180000000000013,"y":310.0},{"x":7.4100000000000135,"y":245.0},{"x":7.640000000000014,"y":184.0},{"x":7.870000000000014,"y":121.0},{"x":8.100000000000014,"y":68.0},{"x":8.330000000000014,"y":29.0},{"x":8.560000000000015,"y":19.0},{"x":8.790000000000015,"y":10.0},{"x":9.020000000000016,"y":4.0},{"x":9.250000000000016,"y":1.0},{"x":9.480000000000016,"y":2.0},{"x":9.710000000000017,"y":0.0},{"x":9.940000000000017,"y":1.0},{"x":10.170000000000018,"y":0.0},{"x":10.400000000000018,"y":0.0},{"x":10.630000000000019,"y":1.0},{"x":10.860000000000019,"y":1.0},{"x":11.09000000000002,"y":1.0},{"x":11.32000000000002,"y":0.0},{"x":11.55000000000002,"y":0.0},{"x":11.78000000000002,"y":0.0},{"x":12.010000000000021,"y":0.0},{"x":12.240000000000022,"y":2.0},{"x":12.470000000000022,"y":0.0},{"x":12.700000000000022,"y":1.0},{"x":12.930000000000023,"y":1.0},{"x":13.160000000000023,"y":0.0},{"x":13.390000000000024,"y":0.0},{"x":13.620000000000024,"y":0.0},{"x":13.850000000000025,"y":2.0},{"x":14.080000000000025,"y":0.0},{"x":14.310000000000025,"y":0.0},{"x":14.540000000000026,"y":0.0},{"x":14.770000000000026,"y":1.0},{"x":15.000000000000027,"y":0.0},{"x":15.230000000000027,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-8 15]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"2049deb4-2a45-4fbf-88a0-e8c4efd1bc85\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"2049deb4-2a45-4fbf-88a0-e8c4efd1bc85\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"2049deb4-2a45-4fbf-88a0-e8c4efd1bc85\", :values ({:x -8.0, :y 0} {:x -7.77, :y 1.0} {:x -7.539999999999999, :y 4.0} {:x -7.309999999999999, :y 2.0} {:x -7.079999999999998, :y 3.0} {:x -6.849999999999998, :y 0.0} {:x -6.619999999999997, :y 1.0} {:x -6.389999999999997, :y 3.0} {:x -6.159999999999997, :y 7.0} {:x -5.929999999999996, :y 2.0} {:x -5.699999999999996, :y 6.0} {:x -5.469999999999995, :y 5.0} {:x -5.239999999999995, :y 5.0} {:x -5.0099999999999945, :y 10.0} {:x -4.779999999999994, :y 6.0} {:x -4.549999999999994, :y 6.0} {:x -4.319999999999993, :y 17.0} {:x -4.089999999999993, :y 12.0} {:x -3.8599999999999928, :y 12.0} {:x -3.629999999999993, :y 17.0} {:x -3.399999999999993, :y 15.0} {:x -3.169999999999993, :y 9.0} {:x -2.939999999999993, :y 13.0} {:x -2.709999999999993, :y 22.0} {:x -2.479999999999993, :y 17.0} {:x -2.249999999999993, :y 27.0} {:x -2.019999999999993, :y 18.0} {:x -1.789999999999993, :y 29.0} {:x -1.559999999999993, :y 46.0} {:x -1.329999999999993, :y 109.0} {:x -1.099999999999993, :y 532.0} {:x -0.869999999999993, :y 861.0} {:x -0.639999999999993, :y 534.0} {:x -0.40999999999999304, :y 142.0} {:x -0.17999999999999303, :y 58.0} {:x 0.05000000000000698, :y 68.0} {:x 0.280000000000007, :y 53.0} {:x 0.510000000000007, :y 54.0} {:x 0.740000000000007, :y 54.0} {:x 0.970000000000007, :y 73.0} {:x 1.200000000000007, :y 64.0} {:x 1.430000000000007, :y 67.0} {:x 1.660000000000007, :y 86.0} {:x 1.890000000000007, :y 52.0} {:x 2.120000000000007, :y 80.0} {:x 2.350000000000007, :y 170.0} {:x 2.580000000000007, :y 390.0} {:x 2.810000000000007, :y 721.0} {:x 3.040000000000007, :y 962.0} {:x 3.270000000000007, :y 844.0} {:x 3.500000000000007, :y 453.0} {:x 3.730000000000007, :y 179.0} {:x 3.960000000000007, :y 81.0} {:x 4.1900000000000075, :y 51.0} {:x 4.420000000000008, :y 36.0} {:x 4.650000000000008, :y 30.0} {:x 4.880000000000009, :y 39.0} {:x 5.110000000000009, :y 56.0} {:x 5.34000000000001, :y 82.0} {:x 5.57000000000001, :y 104.0} {:x 5.8000000000000105, :y 147.0} {:x 6.030000000000011, :y 202.0} {:x 6.260000000000011, :y 239.0} {:x 6.490000000000012, :y 324.0} {:x 6.720000000000012, :y 336.0} {:x 6.950000000000013, :y 323.0} {:x 7.180000000000013, :y 310.0} {:x 7.4100000000000135, :y 245.0} {:x 7.640000000000014, :y 184.0} {:x 7.870000000000014, :y 121.0} {:x 8.100000000000014, :y 68.0} {:x 8.330000000000014, :y 29.0} {:x 8.560000000000015, :y 19.0} {:x 8.790000000000015, :y 10.0} {:x 9.020000000000016, :y 4.0} {:x 9.250000000000016, :y 1.0} {:x 9.480000000000016, :y 2.0} {:x 9.710000000000017, :y 0.0} {:x 9.940000000000017, :y 1.0} {:x 10.170000000000018, :y 0.0} {:x 10.400000000000018, :y 0.0} {:x 10.630000000000019, :y 1.0} {:x 10.860000000000019, :y 1.0} {:x 11.09000000000002, :y 1.0} {:x 11.32000000000002, :y 0.0} {:x 11.55000000000002, :y 0.0} {:x 11.78000000000002, :y 0.0} {:x 12.010000000000021, :y 0.0} {:x 12.240000000000022, :y 2.0} {:x 12.470000000000022, :y 0.0} {:x 12.700000000000022, :y 1.0} {:x 12.930000000000023, :y 1.0} {:x 13.160000000000023, :y 0.0} {:x 13.390000000000024, :y 0.0} {:x 13.620000000000024, :y 0.0} {:x 13.850000000000025, :y 2.0} {:x 14.080000000000025, :y 0.0} {:x 14.310000000000025, :y 0.0} {:x 14.540000000000026, :y 0.0} {:x 14.770000000000026, :y 1.0} {:x 15.000000000000027, :y 0.0} {:x 15.230000000000027, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; We can easily advance the model so it follows a Pitman–Yor process:
;; **

;; @@
(defm create-pitman-yor-stick-breaking-process
  [concentration discount]
  (let
    [
      ; pitman-yor-stick-breaking-rule 
      ; returns a stick length
      ; via the two-parameter stick breaking 
      ; rule which closes over
      ; concentration and discount
      pitman-yor-stick-breaking-rule
      (mem 
        (fn [k] 
          (sample (beta (- 1 discount) 
                        (+ concentration 
                           (* k discount))))))

      ; sample-stick-index is a procedure
      ; that samples an index from
      ; a potentially infinite dimensional 
      ; discrete distribution lazily constructed 
      ; by a stick breaking rule
      sample-stick-index 
       (fn sample-stick-index [k] 
          (if (sample (flip (pitman-yor-stick-breaking-rule k))) 
                             k 
                             (sample-stick-index (+ k 1))))
      
      ; wrapping a stick breaking process to return it
      stick-breaking-process (fn [] (sample-stick-index 1))]
    stick-breaking-process))

(defquery py-example-1
  [input-data]
  (let
    [ 
      ; PYmem is a procedure that takes three arguments 
      ; -- the concentration
      ; and discount to a Pitman--Yor process 
      ; and a base sampling procedure.
      ; PYmem returns a procedure 
      PYmem (fn [concentration discount base]
              (let [get-value-from-cache-or-sample 
                      (mem 
                        (fn [args stick-index] 
                          (apply base args)))
                    
                    get-stick-picking-procedure-from-cache 
                      (mem 
                        (fn [args] 
                          (create-pitman-yor-stick-breaking-process 								  concentration discount)))]
                
                (fn [& varargs]
                  ; when the returned function is called, 
                  ; the first thing it does is get
                  ; the cached stick breaking procedure 
                  ; for the passed in arguments
                  ; and _calls_ it to get an index
                  (let [index 
                        ((get-stick-picking-procedure-from-cache
                           varargs))]
                    ; if, for the given set of arguments 
                    ; and just sampled index
                    ; a return value has already been
                    ; computed, get it from the cache
                    ; and return it, otherwise sample a new value
                    (get-value-from-cache-or-sample varargs index)))))

      H
      (fn []
        (let
          [v (/ 1.0 (sample (gamma 1 0.1)))]
          (list (sample (normal 0 (sqrt (* v 30)))) (sqrt v))))

      obs-param (PYmem 10.0 0.01 H)]

    ; observe draws from the DP mixture
    (map
      (fn [value]
        (observe (apply normal (obs-param)) value))
      input-data)

    ; predict a data point from the DP mixture
    (predict :prediction 
       (sample (apply normal (obs-param))))))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;dp-mixture-model/py-example-1</span>","value":"#'dp-mixture-model/py-example-1"}
;; <=

;; @@
(def samples
  (take 10000
        (map get-predicts
             (doquery :pgibbs
                      py-example-1
                      [input-data]
                      :number-of-particles 100))))

(def predictions (map #(get % :prediction) samples))

(plot/histogram predictions :bins 100 :plot-range [[-8 15] :all])
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-8,15]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"f85c3be6-0df4-4fe3-b035-ff4960f78f03","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"f85c3be6-0df4-4fe3-b035-ff4960f78f03"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"f85c3be6-0df4-4fe3-b035-ff4960f78f03","values":[{"x":-8.0,"y":0},{"x":-7.77,"y":0.0},{"x":-7.539999999999999,"y":4.0},{"x":-7.309999999999999,"y":2.0},{"x":-7.079999999999998,"y":0.0},{"x":-6.849999999999998,"y":3.0},{"x":-6.619999999999997,"y":2.0},{"x":-6.389999999999997,"y":1.0},{"x":-6.159999999999997,"y":4.0},{"x":-5.929999999999996,"y":0.0},{"x":-5.699999999999996,"y":4.0},{"x":-5.469999999999995,"y":6.0},{"x":-5.239999999999995,"y":5.0},{"x":-5.0099999999999945,"y":3.0},{"x":-4.779999999999994,"y":11.0},{"x":-4.549999999999994,"y":5.0},{"x":-4.319999999999993,"y":4.0},{"x":-4.089999999999993,"y":7.0},{"x":-3.8599999999999928,"y":6.0},{"x":-3.629999999999993,"y":8.0},{"x":-3.399999999999993,"y":12.0},{"x":-3.169999999999993,"y":10.0},{"x":-2.939999999999993,"y":11.0},{"x":-2.709999999999993,"y":12.0},{"x":-2.479999999999993,"y":18.0},{"x":-2.249999999999993,"y":24.0},{"x":-2.019999999999993,"y":47.0},{"x":-1.789999999999993,"y":56.0},{"x":-1.559999999999993,"y":103.0},{"x":-1.329999999999993,"y":240.0},{"x":-1.099999999999993,"y":1065.0},{"x":-0.869999999999993,"y":1851.0},{"x":-0.639999999999993,"y":1539.0},{"x":-0.40999999999999304,"y":799.0},{"x":-0.17999999999999303,"y":315.0},{"x":0.05000000000000698,"y":109.0},{"x":0.280000000000007,"y":70.0},{"x":0.510000000000007,"y":60.0},{"x":0.740000000000007,"y":44.0},{"x":0.970000000000007,"y":64.0},{"x":1.200000000000007,"y":51.0},{"x":1.430000000000007,"y":40.0},{"x":1.660000000000007,"y":41.0},{"x":1.890000000000007,"y":54.0},{"x":2.120000000000007,"y":56.0},{"x":2.350000000000007,"y":86.0},{"x":2.580000000000007,"y":130.0},{"x":2.810000000000007,"y":204.0},{"x":3.040000000000007,"y":219.0},{"x":3.270000000000007,"y":194.0},{"x":3.500000000000007,"y":135.0},{"x":3.730000000000007,"y":88.0},{"x":3.960000000000007,"y":49.0},{"x":4.1900000000000075,"y":38.0},{"x":4.420000000000008,"y":34.0},{"x":4.650000000000008,"y":26.0},{"x":4.880000000000009,"y":24.0},{"x":5.110000000000009,"y":22.0},{"x":5.34000000000001,"y":30.0},{"x":5.57000000000001,"y":31.0},{"x":5.8000000000000105,"y":54.0},{"x":6.030000000000011,"y":62.0},{"x":6.260000000000011,"y":57.0},{"x":6.490000000000012,"y":86.0},{"x":6.720000000000012,"y":120.0},{"x":6.950000000000013,"y":167.0},{"x":7.180000000000013,"y":185.0},{"x":7.4100000000000135,"y":210.0},{"x":7.640000000000014,"y":200.0},{"x":7.870000000000014,"y":189.0},{"x":8.100000000000014,"y":194.0},{"x":8.330000000000014,"y":109.0},{"x":8.560000000000015,"y":91.0},{"x":8.790000000000015,"y":78.0},{"x":9.020000000000016,"y":36.0},{"x":9.250000000000016,"y":12.0},{"x":9.480000000000016,"y":12.0},{"x":9.710000000000017,"y":10.0},{"x":9.940000000000017,"y":2.0},{"x":10.170000000000018,"y":2.0},{"x":10.400000000000018,"y":1.0},{"x":10.630000000000019,"y":0.0},{"x":10.860000000000019,"y":1.0},{"x":11.09000000000002,"y":0.0},{"x":11.32000000000002,"y":2.0},{"x":11.55000000000002,"y":4.0},{"x":11.78000000000002,"y":0.0},{"x":12.010000000000021,"y":0.0},{"x":12.240000000000022,"y":3.0},{"x":12.470000000000022,"y":2.0},{"x":12.700000000000022,"y":0.0},{"x":12.930000000000023,"y":0.0},{"x":13.160000000000023,"y":0.0},{"x":13.390000000000024,"y":1.0},{"x":13.620000000000024,"y":0.0},{"x":13.850000000000025,"y":0.0},{"x":14.080000000000025,"y":1.0},{"x":14.310000000000025,"y":1.0},{"x":14.540000000000026,"y":0.0},{"x":14.770000000000026,"y":1.0},{"x":15.000000000000027,"y":2.0},{"x":15.230000000000027,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-8 15]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"f85c3be6-0df4-4fe3-b035-ff4960f78f03\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"f85c3be6-0df4-4fe3-b035-ff4960f78f03\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"f85c3be6-0df4-4fe3-b035-ff4960f78f03\", :values ({:x -8.0, :y 0} {:x -7.77, :y 0.0} {:x -7.539999999999999, :y 4.0} {:x -7.309999999999999, :y 2.0} {:x -7.079999999999998, :y 0.0} {:x -6.849999999999998, :y 3.0} {:x -6.619999999999997, :y 2.0} {:x -6.389999999999997, :y 1.0} {:x -6.159999999999997, :y 4.0} {:x -5.929999999999996, :y 0.0} {:x -5.699999999999996, :y 4.0} {:x -5.469999999999995, :y 6.0} {:x -5.239999999999995, :y 5.0} {:x -5.0099999999999945, :y 3.0} {:x -4.779999999999994, :y 11.0} {:x -4.549999999999994, :y 5.0} {:x -4.319999999999993, :y 4.0} {:x -4.089999999999993, :y 7.0} {:x -3.8599999999999928, :y 6.0} {:x -3.629999999999993, :y 8.0} {:x -3.399999999999993, :y 12.0} {:x -3.169999999999993, :y 10.0} {:x -2.939999999999993, :y 11.0} {:x -2.709999999999993, :y 12.0} {:x -2.479999999999993, :y 18.0} {:x -2.249999999999993, :y 24.0} {:x -2.019999999999993, :y 47.0} {:x -1.789999999999993, :y 56.0} {:x -1.559999999999993, :y 103.0} {:x -1.329999999999993, :y 240.0} {:x -1.099999999999993, :y 1065.0} {:x -0.869999999999993, :y 1851.0} {:x -0.639999999999993, :y 1539.0} {:x -0.40999999999999304, :y 799.0} {:x -0.17999999999999303, :y 315.0} {:x 0.05000000000000698, :y 109.0} {:x 0.280000000000007, :y 70.0} {:x 0.510000000000007, :y 60.0} {:x 0.740000000000007, :y 44.0} {:x 0.970000000000007, :y 64.0} {:x 1.200000000000007, :y 51.0} {:x 1.430000000000007, :y 40.0} {:x 1.660000000000007, :y 41.0} {:x 1.890000000000007, :y 54.0} {:x 2.120000000000007, :y 56.0} {:x 2.350000000000007, :y 86.0} {:x 2.580000000000007, :y 130.0} {:x 2.810000000000007, :y 204.0} {:x 3.040000000000007, :y 219.0} {:x 3.270000000000007, :y 194.0} {:x 3.500000000000007, :y 135.0} {:x 3.730000000000007, :y 88.0} {:x 3.960000000000007, :y 49.0} {:x 4.1900000000000075, :y 38.0} {:x 4.420000000000008, :y 34.0} {:x 4.650000000000008, :y 26.0} {:x 4.880000000000009, :y 24.0} {:x 5.110000000000009, :y 22.0} {:x 5.34000000000001, :y 30.0} {:x 5.57000000000001, :y 31.0} {:x 5.8000000000000105, :y 54.0} {:x 6.030000000000011, :y 62.0} {:x 6.260000000000011, :y 57.0} {:x 6.490000000000012, :y 86.0} {:x 6.720000000000012, :y 120.0} {:x 6.950000000000013, :y 167.0} {:x 7.180000000000013, :y 185.0} {:x 7.4100000000000135, :y 210.0} {:x 7.640000000000014, :y 200.0} {:x 7.870000000000014, :y 189.0} {:x 8.100000000000014, :y 194.0} {:x 8.330000000000014, :y 109.0} {:x 8.560000000000015, :y 91.0} {:x 8.790000000000015, :y 78.0} {:x 9.020000000000016, :y 36.0} {:x 9.250000000000016, :y 12.0} {:x 9.480000000000016, :y 12.0} {:x 9.710000000000017, :y 10.0} {:x 9.940000000000017, :y 2.0} {:x 10.170000000000018, :y 2.0} {:x 10.400000000000018, :y 1.0} {:x 10.630000000000019, :y 0.0} {:x 10.860000000000019, :y 1.0} {:x 11.09000000000002, :y 0.0} {:x 11.32000000000002, :y 2.0} {:x 11.55000000000002, :y 4.0} {:x 11.78000000000002, :y 0.0} {:x 12.010000000000021, :y 0.0} {:x 12.240000000000022, :y 3.0} {:x 12.470000000000022, :y 2.0} {:x 12.700000000000022, :y 0.0} {:x 12.930000000000023, :y 0.0} {:x 13.160000000000023, :y 0.0} {:x 13.390000000000024, :y 1.0} {:x 13.620000000000024, :y 0.0} {:x 13.850000000000025, :y 0.0} {:x 14.080000000000025, :y 1.0} {:x 14.310000000000025, :y 1.0} {:x 14.540000000000026, :y 0.0} {:x 14.770000000000026, :y 1.0} {:x 15.000000000000027, :y 2.0} {:x 15.230000000000027, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; ## Twist
;;; 
;;; It should be apparent that modifying these DP mixture examples is straightforward.  For instance using the `DPmem` construct is is easy to build nested and hierarchical DPs.  We have not exploited conjugacy in any way suggesting that novel DP mixture models can be written simply by changing the prior (@@H@@) and the likelihood (@@F@@) in any of these examples. 
;;; 
;;; Furthermore, as CRP, PYMem, and DPmem are constructs in the language, writing models of this form is rather straightforward, requiring just a few lines if these are taken as given.
;;; 
;;; Also, conjugacy is not exploited in any way here.  Using stateful stochastic procedures in place of lazy parameter instantiation will improve inference performance dramatically.
;; **

;; @@

;; @@
