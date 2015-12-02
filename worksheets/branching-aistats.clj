;; gorilla-repl.fileformat = 1

;; **
;;; # Simple Branching (AISTATS)
;;; 
;;; This is the simple branching benchmark from the 2014 AISTATS paper, with enumerated posterior.
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
(defn fib [n]
  "returns the n-th number in the Fibonacci sequence"
  (loop [a 0 b 1 m 0]
    (if (= m n)
      a
      (recur b (+ a b) (inc m)))))

(with-primitive-procedures [fib]
  (defquery branching
      "A simple example illustrating flow control with
      dependence on random choices"
      []
      (let [count-prior (poisson 4)
            r (sample count-prior)
            l (if (< 4 r)
                6
                (+ (fib (* 3 r))
                   (sample count-prior)))]
        (observe (poisson l) 6)
        (predict :r r)
        (predict :l l))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/branching</span>","value":"#'aistats-examples/branching"}
;; <=

;; **
;;; Posterior
;; **

;; @@
(def -inf (/ -1.0 0.0))

(def posterior 
  "posterior on l (ranged 0 ... 15), calculated by enumeration"
  (zipmap (range 15)
          (mapv exp 
                [-3.9095 -2.1104 -2.6806 -inf -inf -1.1045 
                 -1.5051 -2.0530 -2.7665 -3.5635 -4.4786 
                 -5.5249 -6.5592 -7.8998 -8.7471])))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/posterior</span>","value":"#'aistats-examples/posterior"}
;; <=

;; **
;;; Run inference
;; **

;; @@
(def number-of-samples 1000000)

(def samples
  (->> (doquery :importance branching [])
       (take number-of-samples)
       doall
       time))
;; @@
;; ->
;;; &quot;Elapsed time: 26774.637 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/samples</span>","value":"#'aistats-examples/samples"}
;; <=

;; **
;;; Calculate KL error relative to true posterior as a function of number of samples
;; **

;; @@
(def num-sample-range (mapv (partial * number-of-samples)
                            [1e-2 2e-2 5e-2 1e-1 2e-1 5e-1 1]))
  
(def KL-errors
  (map (fn [n]
         (->> (take n samples)
              (collect-by :r)
              s/empirical-distribution
              (#(kl-categorical posterior %))))
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":50,"bottom":20,"right":10},"data":[{"name":"0c709d4a-57f1-4dd0-a80a-d57b5f598bba","values":[{"x":4.0,"y":-2.753500366683675},{"x":4.30102999566398,"y":-3.052640518294788},{"x":4.698970004336019,"y":-3.6997184291987253},{"x":5.0,"y":-3.940469401303053},{"x":5.301029995663981,"y":-4.027550350866245},{"x":5.698970004336018,"y":-3.896951844297979},{"x":5.999999999999999,"y":-3.903775937956721}]}],"marks":[{"type":"line","from":{"data":"0c709d4a-57f1-4dd0-a80a-d57b5f598bba"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#05A"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"0c709d4a-57f1-4dd0-a80a-d57b5f598bba","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"0c709d4a-57f1-4dd0-a80a-d57b5f598bba","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 50, :bottom 20, :right 10}, :data [{:name \"0c709d4a-57f1-4dd0-a80a-d57b5f598bba\", :values ({:x 4.0, :y -2.753500366683675} {:x 4.30102999566398, :y -3.052640518294788} {:x 4.698970004336019, :y -3.6997184291987253} {:x 5.0, :y -3.940469401303053} {:x 5.301029995663981, :y -4.027550350866245} {:x 5.698970004336018, :y -3.896951844297979} {:x 5.999999999999999, :y -3.903775937956721})}], :marks [{:type \"line\", :from {:data \"0c709d4a-57f1-4dd0-a80a-d57b5f598bba\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#05A\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"0c709d4a-57f1-4dd0-a80a-d57b5f598bba\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"0c709d4a-57f1-4dd0-a80a-d57b5f598bba\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=
