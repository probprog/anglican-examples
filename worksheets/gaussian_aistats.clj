;; gorilla-repl.fileformat = 1

;; **
;;; # Gaussian with Unknown Mean (AISTATS)
;;; 
;;; This is the Gaussian with unknown mean  benchmark from the 2014 AISTATS paper, with analytical posterior.
;; **

;; @@
(ns aistats-examples
  (:require [gorilla-plot.core :as plot]
            [clojure.core.matrix :as m]
            [anglican.stat :as s])
  (:use clojure.repl
        [anglican 
          core runtime emit 
          [state :only [get-predicts get-log-weight]]
          [inference :only [collect-by]]]))

(defn- square [x] (m/mul x x))

(defn kl-normal 
  "calculates the kl divergence beween two 
  normal distributions from parameters"
  [p-mean p-sigma q-mean q-sigma]
  (+ (- (log q-sigma)
     	(log p-sigma))
     (/ (+ (square p-sigma)
           (square (- p-mean
                      q-mean)))
        (* 2 (square q-sigma)))
     (/ -1 2)))           
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/kl-normal</span>","value":"#'aistats-examples/kl-normal"}
;; <=

;; **
;;; Define model
;; **

;; @@
(defquery gaussian
    "Returns the posterior distribution on the mean of a Gaussian, 
    conditioned on observations"
    [observations sigma mu0 sigma0]
    (let [mu (sample (normal mu0 sigma0))
          likelihood (normal mu sigma)]
      (reduce (fn [_ obs]
                (observe likelihood obs))
              nil
              observations)
      (predict :mu mu)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/gaussian</span>","value":"#'aistats-examples/gaussian"}
;; <=

;; **
;;; Posterior
;; **

;; @@
(def posterior (normal 7.25 (sqrt (/ 1.0 1.2))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/posterior</span>","value":"#'aistats-examples/posterior"}
;; <=

;; **
;;; Run inference
;; **

;; @@
(def number-of-samples 100000)

(def samples
  (->> (doquery :lmh
                gaussian 
                [[9.0 8.0] (sqrt 2.0) 1.0 (sqrt 5.0)])
       (take number-of-samples)
       doall
       time))
;; @@
;; ->
;;; &quot;Elapsed time: 5011.655 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/samples</span>","value":"#'aistats-examples/samples"}
;; <=

;; **
;;; Calculate KL error relative to true posterior as a function of number of samples
;; **

;; @@
(def num-sample-range (mapv (comp int (partial * number-of-samples))
                            [1e-3 2e-3 5e-3 1e-2 2e-2 5e-2 1e-1 2e-1 5e-1 1]))
  
(def KL-errors
  (map (fn [n]
         (let [mus (collect-by :mu (take n samples))
			   mean (s/empirical-mean mus)
               sd (s/empirical-std mus)]
			(kl-normal mean sd (:mean posterior) (:sd posterior))))
       num-sample-range))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/KL-errors</span>","value":"#'aistats-examples/KL-errors"}
;; <=

;; @@
(plot/list-plot (map vector 
                     (map #(/ (log %) 
                              (log 10)) 
                          num-sample-range)
                     (map #(/ (log %) 
                              (log 10)) 
                          KL-errors))
                :joined true
                :color "#05A"
                :x-title "log number of samples"
                :y-title "log KL divergence")
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":50,"bottom":20,"right":10},"data":[{"name":"75bf816a-5aaa-4750-a2d9-7463e74db235","values":[{"x":2.0,"y":-0.37298302246990905},{"x":2.301029995663981,"y":-0.8949494007839545},{"x":2.6989700043360183,"y":-0.6964922791436191},{"x":2.9999999999999996,"y":-1.4221033657305007},{"x":3.301029995663981,"y":-0.9883462468102788},{"x":3.6989700043360187,"y":-1.5681344385457023},{"x":4.0,"y":-1.1364573893222167},{"x":4.30102999566398,"y":-1.2257725471021892},{"x":4.698970004336019,"y":-2.202983704777243},{"x":5.0,"y":-2.3792338200716108}]}],"marks":[{"type":"line","from":{"data":"75bf816a-5aaa-4750-a2d9-7463e74db235"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#05A"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"75bf816a-5aaa-4750-a2d9-7463e74db235","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"75bf816a-5aaa-4750-a2d9-7463e74db235","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 50, :bottom 20, :right 10}, :data [{:name \"75bf816a-5aaa-4750-a2d9-7463e74db235\", :values ({:x 2.0, :y -0.37298302246990905} {:x 2.301029995663981, :y -0.8949494007839545} {:x 2.6989700043360183, :y -0.6964922791436191} {:x 2.9999999999999996, :y -1.4221033657305007} {:x 3.301029995663981, :y -0.9883462468102788} {:x 3.6989700043360187, :y -1.5681344385457023} {:x 4.0, :y -1.1364573893222167} {:x 4.30102999566398, :y -1.2257725471021892} {:x 4.698970004336019, :y -2.202983704777243} {:x 5.0, :y -2.3792338200716108})}], :marks [{:type \"line\", :from {:data \"75bf816a-5aaa-4750-a2d9-7463e74db235\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#05A\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"75bf816a-5aaa-4750-a2d9-7463e74db235\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"75bf816a-5aaa-4750-a2d9-7463e74db235\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=
