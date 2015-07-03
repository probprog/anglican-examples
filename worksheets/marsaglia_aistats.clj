;; gorilla-repl.fileformat = 1

;; **
;;; # Marsaglia Normal (AISTATS)
;;; 
;;; This is the Gaussian with unknown mean benchmark from the 2014 AISTATS paper, where the Marsaglia rejection sampling algorithm is used to sample the mean.
;; **

;; @@
(ns aistats-examples
  (:require [gorilla-plot.core :as plot]
            [clojure.core.matrix :as m])
  (:use clojure.repl
        [anglican 
          core runtime emit 
          [state :only [get-predicts get-log-weight]]]))

(defn expected-value
  "applies f to each sample and computes weighted expectation"
  [f samples]
  (let [vs (map f samples)
        lws (map get-log-weight samples)
        vlws (map vector vs lws)
        max-lw (reduce max lws)]
    (loop [vlws vlws
           sum-wv 0.0
           sum-w 0.0]
      (if-let [[v lw] (first vlws)]
        (let [w (exp (- lw max-lw))]
          (recur (rest vlws)
                 (m/add sum-wv (m/mul w v))
                 (m/add sum-w w)))
        (m/div sum-wv sum-w)))))

(defn- square [x] (m/mul x x))

(defn empirical-moments 
  "returns the empirical mean and variance
  of predicts with key k"
  [k samples]
  (let [mean (expected-value 
               (comp k get-predicts) 
               samples)
        var (m/sub (expected-value 
                     (comp square k get-predicts) 
                     samples)
                   (m/mul mean mean))]
    [mean var]))

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
(defm marsaglia 
  "Samples from a normal distribution using the Marsaglia rejection sampling"
  [mu std]
  (let [u (uniform-continuous -1.0 1.0)]
    (loop [x (sample u)
           y (sample u)]
      (let [s (+ (* x x) (* y y))]
        (if (< s 1.0)
          (+ mu (* std (* x (sqrt (* -2.0 (/ (log s) s))))))
          (recur (sample u) (sample u)))))))

(defquery gaussian-marsaglia
    "Returns the posterior distribution on the mean of a Gaussian, 
    conditioned on observations. The mean is sampled using the
    Marsaglia algorithm."
    [observations sigma mu0 sigma0]
    (let [mu (marsaglia mu0 sigma0)
          likelihood (normal mu sigma)]
      (reduce (fn [_ obs]
                (observe likelihood obs))
              nil
              observations)
      (predict :mu mu)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/gaussian-marsaglia</span>","value":"#'aistats-examples/gaussian-marsaglia"}
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
  (->> (doquery :importance 
                gaussian-marsaglia
                [[9.0 8.0] (sqrt 2.0) 1.0 (sqrt 5.0)])
       (take number-of-samples)
       doall
       time))
;; @@
;; ->
;;; &quot;Elapsed time: 2094.812 msecs&quot;
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
         (let [[m v] (empirical-moments 
                       :mu 
                       (take n samples))]
			(kl-normal m 
                       (sqrt v) 
                       (:mean posterior) 
                       (:sd posterior))))

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
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"5358584c-abe2-4a2a-be41-390076e8eb9b","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"5358584c-abe2-4a2a-be41-390076e8eb9b","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"5358584c-abe2-4a2a-be41-390076e8eb9b"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#05A"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"5358584c-abe2-4a2a-be41-390076e8eb9b","values":[{"x":2.0,"y":0.34791447751882476},{"x":2.301029995663981,"y":0.3305568290583956},{"x":2.6989700043360183,"y":-1.4143660332055876},{"x":2.9999999999999996,"y":-0.8875015443830396},{"x":3.301029995663981,"y":-1.0750300845265404},{"x":3.6989700043360187,"y":-1.0215114417366515},{"x":4.0,"y":-1.9722695641314043},{"x":4.30102999566398,"y":-2.7241529768480746},{"x":4.698970004336019,"y":-2.944956045261223},{"x":5.0,"y":-2.8810510920306585}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"5358584c-abe2-4a2a-be41-390076e8eb9b\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"5358584c-abe2-4a2a-be41-390076e8eb9b\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"5358584c-abe2-4a2a-be41-390076e8eb9b\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#05A\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"5358584c-abe2-4a2a-be41-390076e8eb9b\", :values ({:x 2.0, :y 0.34791447751882476} {:x 2.301029995663981, :y 0.3305568290583956} {:x 2.6989700043360183, :y -1.4143660332055876} {:x 2.9999999999999996, :y -0.8875015443830396} {:x 3.301029995663981, :y -1.0750300845265404} {:x 3.6989700043360187, :y -1.0215114417366515} {:x 4.0, :y -1.9722695641314043} {:x 4.30102999566398, :y -2.7241529768480746} {:x 4.698970004336019, :y -2.944956045261223} {:x 5.0, :y -2.8810510920306585})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=
