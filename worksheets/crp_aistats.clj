;; gorilla-repl.fileformat = 1

;; **
;;; # CRP Gaussian Mixuture (AISTATS)
;;; 
;;; This is the CRP Gaussian mixture benchmark from the 2014 AISTATS paper, with 10 observations and a fully enumerated ground truth posterior.
;; **

;; @@
 (ns aistats-examples
  (:require [gorilla-plot.core :as plot]
            [clojure.core.matrix :as m])
  (:use clojure.repl
        [anglican 
          core runtime emit 
          [state :only [get-predicts get-log-weight]]]))

(defn empirical-frequencies
  "applies f to each sample and returns a map of weighted 
  counts for each unique value"
  [f samples]
  (let [log-Z (- (reduce 
                   log-sum-exp
                   (map get-log-weight samples))
                 (log (count samples)))]
    (reduce (fn [freqs s]
              (let [v (f s)]
                (assoc freqs
                  v (+ (get freqs v 0.0)
                       (exp (- (get-log-weight s)
                               log-Z))))))
            {}
            samples)))
 
(defn normalize
  "divides each element in a collection by the sum of the elements"
  [coll]
  (let [norm (reduce + coll)]
    (map #(/ % norm) coll)))

(defn kl-div 
  "KL divergence between two vectors of normalized probabilities"
  [p-probs q-probs]
  (reduce + 
          (map (fn [p q]
                 (if (> p 0.0)
                   (* p (log (/ p q)))
                   0.0))
               p-probs
               q-probs)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/kl-div</span>","value":"#'aistats-examples/kl-div"}
;; <=

;; **
;;; Define model
;; **

;; @@
(defquery crp-mixture
  "CRP gaussian mixture model"
  [observations alpha mu beta a b]
  (let [precision-prior (gamma a b)]
    (loop [observations observations
           state-proc (CRP alpha)
           obs-dists []
           states []]
      (if (empty? observations)
        (do 
          (predict :states states)
          (predict :num-clusters (count obs-dists)))
        (let [state (sample (produce state-proc))
              obs-dist (get obs-dists
                            state
                            (let [l (sample precision-prior)
                                  s (sqrt (/ (* beta l)))
                                  m (sample (normal mu s))]
                              (normal m (sqrt (/ l)))))]
          (observe obs-dist (first observations))
          (recur (rest observations)
                 (absorb state-proc state)
                 (assoc obs-dists state obs-dist)
                 (conj states state)))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/crp-mixture</span>","value":"#'aistats-examples/crp-mixture"}
;; <=

;; **
;;; Define data and posterior
;; **

;; @@
(def data 
  "observation sequence length 10"
  [10 11 12 -100 -150 -200 0.001 0.01 0.005 0.0])
 
(def posterior 
  "posterior on number of states, calculated by enumeration"
  (mapv exp 
        [-11.4681 -1.0437 -0.9126 -1.6553 -3.0348 
         -4.9985 -7.5829 -10.9459 -15.6461 -21.6521]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/posterior</span>","value":"#'aistats-examples/posterior"}
;; <=

;; **
;;; Run inference
;; **

;; @@
(def number-of-particles 10000)
(def number-of-samples 100000)

(def samples
  (->> (doquery :smc crp-mixture
                [data 1.72 0.0 100.0 1.0 10.0]
                :number-of-particles number-of-particles)
       (take number-of-samples)
       doall
       time))
;; @@
;; ->
;;; &quot;Elapsed time: 25839.253 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/samples</span>","value":"#'aistats-examples/samples"}
;; <=

;; **
;;; Calculate KL error relative to true posterior as a function of number of samples
;; **

;; @@
(def num-sample-range [1e1 2e1 5e1 1e2 2e2 5e2 1e3 2e3 5e3 1e4])
  
(def KL-errors
  (map (fn [n]
         (->> (take n samples)
              (empirical-frequencies 
                (comp :num-clusters get-predicts))
     		  (into (sorted-map))
			  vals
		      normalize
              (#(kl-div % posterior))))
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
                :y-title "log KL divergne")
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"2f1dc1e4-7113-4d41-8a39-4e2c945cf8e8","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"2f1dc1e4-7113-4d41-8a39-4e2c945cf8e8","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"2f1dc1e4-7113-4d41-8a39-4e2c945cf8e8"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#05A"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"2f1dc1e4-7113-4d41-8a39-4e2c945cf8e8","values":[{"x":1.0,"y":0.8212074055783224},{"x":1.301029995663981,"y":0.6161939275386032},{"x":1.6989700043360185,"y":-0.057251425387462904},{"x":2.0,"y":-0.13133387040578912},{"x":2.301029995663981,"y":-0.2013740762230533},{"x":2.6989700043360183,"y":-0.6794233231386524},{"x":2.9999999999999996,"y":-0.6648134274673143},{"x":3.301029995663981,"y":-0.3594195312103305},{"x":3.6989700043360187,"y":-0.33115899405938587},{"x":4.0,"y":-0.6820407482543657}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"2f1dc1e4-7113-4d41-8a39-4e2c945cf8e8\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"2f1dc1e4-7113-4d41-8a39-4e2c945cf8e8\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"2f1dc1e4-7113-4d41-8a39-4e2c945cf8e8\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#05A\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"2f1dc1e4-7113-4d41-8a39-4e2c945cf8e8\", :values ({:x 1.0, :y 0.8212074055783224} {:x 1.301029995663981, :y 0.6161939275386032} {:x 1.6989700043360185, :y -0.057251425387462904} {:x 2.0, :y -0.13133387040578912} {:x 2.301029995663981, :y -0.2013740762230533} {:x 2.6989700043360183, :y -0.6794233231386524} {:x 2.9999999999999996, :y -0.6648134274673143} {:x 3.301029995663981, :y -0.3594195312103305} {:x 3.6989700043360187, :y -0.33115899405938587} {:x 4.0, :y -0.6820407482543657})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=
