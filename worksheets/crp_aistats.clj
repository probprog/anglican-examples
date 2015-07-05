;; gorilla-repl.fileformat = 1

;; **
;;; # CRP Gaussian Mixture (AISTATS)
;;; 
;;; This is the CRP Gaussian mixture benchmark from the 2014 AISTATS paper, with 10 observations and a fully enumerated ground truth posterior.
;; **

;; @@
 (ns aistats-examples
  (:require [gorilla-plot.core :as plot]
            [clojure.core.matrix :as m]
            [anglican.stat :as s]
            :reload)
  (:use clojure.repl
        [anglican 
          core runtime emit 
          [state :only [get-predicts get-log-weight]]
          [inference :only [collect-by]]]))
 
(defn kl-categorical
  "KL divergence between two categorical distributions"
  [p-categories q-categories]
  (let [p-norm (reduce + (map second p-categories))
        q-norm (reduce + (map second q-categories))
        q (into {} (for [[c w] q-categories] 
                     [c (/ w q-norm)]))]
    (reduce + 
            (for [[c w] p-categories]
                   (if (> w 0.0)
                     (* (/ w p-norm)
                        (log (/ (double (/ w p-norm))
                                (double (get q c 0.0)))))
                     0.0)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/kl-categorical</span>","value":"#'aistats-examples/kl-categorical"}
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
  (zipmap 
    (range 1 11)
    (mapv exp 
        [-11.4681 -1.0437 -0.9126 -1.6553 -3.0348 
         -4.9985 -7.5829 -10.9459 -15.6461 -21.6521])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/posterior</span>","value":"#'aistats-examples/posterior"}
;; <=

;; **
;;; Run inference
;; **

;; @@
(def number-of-particles 1000)
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
;;; &quot;Elapsed time: 46874.117 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/samples</span>","value":"#'aistats-examples/samples"}
;; <=

;; **
;;; Calculate L2 error relative to true posterior as a function of number of samples
;; **

;; @@
(def num-sample-range (mapv (partial * number-of-samples)
                            [1e-2 2e-2 5e-2 1e-1 2e-1 5e-1 1]))
  
(def KL-errors
  (map (fn [n]
         (->> (take n samples)
              (collect-by :num-clusters)
              s/empirical-distribution
              (#(kl-categorical % posterior))))
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
                :y-title "log L2 error")
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":50,"bottom":20,"right":10},"data":[{"name":"1de2f4d2-2cab-448c-8568-bb491e3d257a","values":[{"x":2.9999999999999996,"y":-0.10090380180482968},{"x":3.301029995663981,"y":-0.6011536244852838},{"x":3.6989700043360187,"y":-1.623207648409456},{"x":4.0,"y":-0.5631326608503568},{"x":4.30102999566398,"y":-0.656868280921334},{"x":4.698970004336019,"y":-0.6897469422680568},{"x":5.0,"y":-0.7460849466018051}]}],"marks":[{"type":"line","from":{"data":"1de2f4d2-2cab-448c-8568-bb491e3d257a"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#05A"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"1de2f4d2-2cab-448c-8568-bb491e3d257a","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"1de2f4d2-2cab-448c-8568-bb491e3d257a","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 50, :bottom 20, :right 10}, :data [{:name \"1de2f4d2-2cab-448c-8568-bb491e3d257a\", :values ({:x 2.9999999999999996, :y -0.10090380180482968} {:x 3.301029995663981, :y -0.6011536244852838} {:x 3.6989700043360187, :y -1.623207648409456} {:x 4.0, :y -0.5631326608503568} {:x 4.30102999566398, :y -0.656868280921334} {:x 4.698970004336019, :y -0.6897469422680568} {:x 5.0, :y -0.7460849466018051})}], :marks [{:type \"line\", :from {:data \"1de2f4d2-2cab-448c-8568-bb491e3d257a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#05A\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"1de2f4d2-2cab-448c-8568-bb491e3d257a\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"1de2f4d2-2cab-448c-8568-bb491e3d257a\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=
