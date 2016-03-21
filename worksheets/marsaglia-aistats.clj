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
;;; 
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
  (->> (doquery :lmh 
                gaussian-marsaglia
                [[9.0 8.0] (sqrt 2.0) 1.0 (sqrt 5.0)])
       (take number-of-samples)
       doall
       time))
;; @@
;; ->
;;; &quot;Elapsed time: 6781.181 msecs&quot;
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":50,"bottom":20,"right":10},"data":[{"name":"7e59f912-3206-48bc-9000-12f6193f62e2","values":[{"x":2.0,"y":-0.7569707442852656},{"x":2.301029995663981,"y":-0.5819256292543926},{"x":2.6989700043360183,"y":-0.571336354459199},{"x":2.9999999999999996,"y":-2.226481369537634},{"x":3.301029995663981,"y":-1.8840962788552595},{"x":3.6989700043360187,"y":-2.1475794366323413},{"x":4.0,"y":-2.224156569876913},{"x":4.30102999566398,"y":-3.308171162914727},{"x":4.698970004336019,"y":-3.7478475463479266},{"x":5.0,"y":-2.877651549783495}]}],"marks":[{"type":"line","from":{"data":"7e59f912-3206-48bc-9000-12f6193f62e2"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#05A"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"7e59f912-3206-48bc-9000-12f6193f62e2","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"7e59f912-3206-48bc-9000-12f6193f62e2","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 50, :bottom 20, :right 10}, :data [{:name \"7e59f912-3206-48bc-9000-12f6193f62e2\", :values ({:x 2.0, :y -0.7569707442852656} {:x 2.301029995663981, :y -0.5819256292543926} {:x 2.6989700043360183, :y -0.571336354459199} {:x 2.9999999999999996, :y -2.226481369537634} {:x 3.301029995663981, :y -1.8840962788552595} {:x 3.6989700043360187, :y -2.1475794366323413} {:x 4.0, :y -2.224156569876913} {:x 4.30102999566398, :y -3.308171162914727} {:x 4.698970004336019, :y -3.7478475463479266} {:x 5.0, :y -2.877651549783495})}], :marks [{:type \"line\", :from {:data \"7e59f912-3206-48bc-9000-12f6193f62e2\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#05A\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"7e59f912-3206-48bc-9000-12f6193f62e2\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"7e59f912-3206-48bc-9000-12f6193f62e2\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=
