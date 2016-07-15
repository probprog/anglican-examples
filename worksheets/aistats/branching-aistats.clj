;; gorilla-repl.fileformat = 1

;; **
;;; # Simple Branching (AISTATS)
;;; 
;;; This is the simple branching benchmark from the 2014 AISTATS paper, with enumerated posterior.
;; **

;; @@
(ns branching-aistats
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/kl-categorical</span>","value":"#'branching-aistats/kl-categorical"}
;; <=

;; **
;;; ## Define model
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
      r)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/branching</span>","value":"#'branching-aistats/branching"}
;; <=

;; **
;;; ## Ground Truth Posterior
;; **

;; @@
(def -inf (/ -1.0 0.0))

(def posterior 
  "posterior on l (range 0 ... 15), 
  calculated by externally-coded enumeration"
  (zipmap (range 15)
          (mapv exp 
                [-3.9095 -2.1104 -2.6806 -inf -inf -1.1045 
                 -1.5051 -2.0530 -2.7665 -3.5635 -4.4786 
                 -5.5249 -6.5592 -7.8998 -8.7471])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/posterior</span>","value":"#'branching-aistats/posterior"}
;; <=

;; **
;;; ## Run inference
;; **

;; @@
(def number-of-samples 100000)

(def samples
  (->> (doquery :importance branching [])
       (take number-of-samples)
       doall
       time))
;; @@
;; ->
;;; &quot;Elapsed time: 2556.934732 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/samples</span>","value":"#'branching-aistats/samples"}
;; <=

;; **
;;; ## Plot Empirical Posterior and True Posterior on r
;; **

;; @@
(def empirical-posterior 
  (->> samples
       (collect-by :result)
       s/empirical-distribution
  	   (into (sorted-map))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/empirical-posterior</span>","value":"#'branching-aistats/empirical-posterior"}
;; <=

;; **
;;; **Empirical Posterior**
;; **

;; @@
(plot/bar-chart (keys empirical-posterior)
                (vals empirical-posterior))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"23a4bb24-9954-48f0-9ce1-ace96fe0e740","values":[{"x":0,"y":0.02119386922634044},{"x":1,"y":0.12042088302632481},{"x":2,"y":0.06811629818543882},{"x":3,"y":9.94810605967839E-10},{"x":4,"y":7.970601507979533E-54},{"x":5,"y":0.3312259172715936},{"x":6,"y":0.22610991230213678},{"x":7,"y":0.1268730225320287},{"x":8,"y":0.06033602898022448},{"x":9,"y":0.028773333877126973},{"x":10,"y":0.011243271403142685},{"x":11,"y":0.003926562341173893},{"x":12,"y":0.0013303107385397872},{"x":13,"y":2.3602287296673667E-4},{"x":14,"y":1.5019637370610516E-4},{"x":15,"y":6.436987444547348E-5}]}],"marks":[{"type":"rect","from":{"data":"23a4bb24-9954-48f0-9ce1-ace96fe0e740"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"23a4bb24-9954-48f0-9ce1-ace96fe0e740","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"23a4bb24-9954-48f0-9ce1-ace96fe0e740","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"23a4bb24-9954-48f0-9ce1-ace96fe0e740\", :values ({:x 0, :y 0.02119386922634044} {:x 1, :y 0.12042088302632481} {:x 2, :y 0.06811629818543882} {:x 3, :y 9.94810605967839E-10} {:x 4, :y 7.970601507979533E-54} {:x 5, :y 0.3312259172715936} {:x 6, :y 0.22610991230213678} {:x 7, :y 0.1268730225320287} {:x 8, :y 0.06033602898022448} {:x 9, :y 0.028773333877126973} {:x 10, :y 0.011243271403142685} {:x 11, :y 0.003926562341173893} {:x 12, :y 0.0013303107385397872} {:x 13, :y 2.3602287296673667E-4} {:x 14, :y 1.5019637370610516E-4} {:x 15, :y 6.436987444547348E-5})}], :marks [{:type \"rect\", :from {:data \"23a4bb24-9954-48f0-9ce1-ace96fe0e740\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"23a4bb24-9954-48f0-9ce1-ace96fe0e740\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"23a4bb24-9954-48f0-9ce1-ace96fe0e740\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; **True Posterior**
;; **

;; @@
(let [p (into (sorted-map) posterior)]
	(plot/bar-chart (keys p)
    	            (vals p)))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"c6555738-20ad-4249-8d4c-8a8daa4cd939","values":[{"x":0,"y":0.020050523817695055},{"x":1,"y":0.12118948094455258},{"x":2,"y":0.0685220286006852},{"x":3,"y":0.0},{"x":4,"y":0.0},{"x":5,"y":0.33137652909136334},{"x":6,"y":0.22199509321261565},{"x":7,"y":0.12834927760523412},{"x":8,"y":0.06288170601169404},{"x":9,"y":0.02833946281244483},{"x":10,"y":0.011349291045013778},{"x":11,"y":0.003986267298927514},{"x":12,"y":0.0014170188721193543},{"x":13,"y":3.708176965825454E-4},{"x":14,"y":1.5892152993304522E-4}]}],"marks":[{"type":"rect","from":{"data":"c6555738-20ad-4249-8d4c-8a8daa4cd939"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"c6555738-20ad-4249-8d4c-8a8daa4cd939","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"c6555738-20ad-4249-8d4c-8a8daa4cd939","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"c6555738-20ad-4249-8d4c-8a8daa4cd939\", :values ({:x 0, :y 0.020050523817695055} {:x 1, :y 0.12118948094455258} {:x 2, :y 0.0685220286006852} {:x 3, :y 0.0} {:x 4, :y 0.0} {:x 5, :y 0.33137652909136334} {:x 6, :y 0.22199509321261565} {:x 7, :y 0.12834927760523412} {:x 8, :y 0.06288170601169404} {:x 9, :y 0.02833946281244483} {:x 10, :y 0.011349291045013778} {:x 11, :y 0.003986267298927514} {:x 12, :y 0.0014170188721193543} {:x 13, :y 3.708176965825454E-4} {:x 14, :y 1.5892152993304522E-4})}], :marks [{:type \"rect\", :from {:data \"c6555738-20ad-4249-8d4c-8a8daa4cd939\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c6555738-20ad-4249-8d4c-8a8daa4cd939\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c6555738-20ad-4249-8d4c-8a8daa4cd939\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; ## Plot KL error relative to true posterior as a function of number of samples
;; **

;; @@
(def num-sample-range 
  (mapv (partial / number-of-samples)
        (reverse [1 2 3 4 5 6 7 8 9])))
  
(def KL-errors
  (map (fn [n]
         (->> (take n samples)
              (collect-by :result)
              s/empirical-distribution
              (#(kl-categorical posterior %))))
       num-sample-range))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/KL-errors</span>","value":"#'branching-aistats/KL-errors"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"8a0ece82-a389-487e-9a17-717dfa9de80e","values":[{"x":4.045757490560675,"y":-3.0732106268456802},{"x":4.096910013008056,"y":-3.183473120263494},{"x":4.154901959985743,"y":-3.024049212490327},{"x":4.221848749616356,"y":-3.1291485791825213},{"x":4.30102999566398,"y":-3.233231509250356},{"x":4.3979400086720375,"y":-3.2578113667293223},{"x":4.5228787452803365,"y":-3.218695459069157},{"x":4.698970004336019,"y":-3.3682229852739574},{"x":5.0,"y":-3.6222423404154678}]}],"marks":[{"type":"line","from":{"data":"8a0ece82-a389-487e-9a17-717dfa9de80e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#05A"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"8a0ece82-a389-487e-9a17-717dfa9de80e","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"8a0ece82-a389-487e-9a17-717dfa9de80e","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"log number of samples","titleOffset":30},{"type":"y","scale":"y","title":"log KL divergence","titleOffset":45}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"8a0ece82-a389-487e-9a17-717dfa9de80e\", :values ({:x 4.045757490560675, :y -3.0732106268456802} {:x 4.096910013008056, :y -3.183473120263494} {:x 4.154901959985743, :y -3.024049212490327} {:x 4.221848749616356, :y -3.1291485791825213} {:x 4.30102999566398, :y -3.233231509250356} {:x 4.3979400086720375, :y -3.2578113667293223} {:x 4.5228787452803365, :y -3.218695459069157} {:x 4.698970004336019, :y -3.3682229852739574} {:x 5.0, :y -3.6222423404154678})}], :marks [{:type \"line\", :from {:data \"8a0ece82-a389-487e-9a17-717dfa9de80e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#05A\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"8a0ece82-a389-487e-9a17-717dfa9de80e\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"8a0ece82-a389-487e-9a17-717dfa9de80e\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"log number of samples\", :titleOffset 30} {:type \"y\", :scale \"y\", :title \"log KL divergence\", :titleOffset 45}]}}"}
;; <=
