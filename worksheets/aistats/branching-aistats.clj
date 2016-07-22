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
  (loop [a 1 b 1 m 0]
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
;;; &quot;Elapsed time: 1624.445 msecs&quot;
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"b6d5b576-692c-4a84-bb85-1d8abfcf9ccb","values":[{"x":0,"y":0.029739901733711934},{"x":1,"y":0.12555239650450914},{"x":2,"y":0.00515082667254277},{"x":3,"y":1.2754647452031318E-17},{"x":4,"y":3.215698175495852E-91},{"x":5,"y":0.3529665154217697},{"x":6,"y":0.23367805571670952},{"x":7,"y":0.13644577033637145},{"x":8,"y":0.0685389408903422},{"x":9,"y":0.030047869013519444},{"x":10,"y":0.01137800599760603},{"x":11,"y":0.004334478475278515},{"x":12,"y":0.0014899769758769834},{"x":13,"y":3.837819483319515E-4},{"x":14,"y":2.483294959794978E-4},{"x":15,"y":4.51508174508177E-5}]}],"marks":[{"type":"rect","from":{"data":"b6d5b576-692c-4a84-bb85-1d8abfcf9ccb"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"b6d5b576-692c-4a84-bb85-1d8abfcf9ccb","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"b6d5b576-692c-4a84-bb85-1d8abfcf9ccb","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"b6d5b576-692c-4a84-bb85-1d8abfcf9ccb\", :values ({:x 0, :y 0.029739901733711934} {:x 1, :y 0.12555239650450914} {:x 2, :y 0.00515082667254277} {:x 3, :y 1.2754647452031318E-17} {:x 4, :y 3.215698175495852E-91} {:x 5, :y 0.3529665154217697} {:x 6, :y 0.23367805571670952} {:x 7, :y 0.13644577033637145} {:x 8, :y 0.0685389408903422} {:x 9, :y 0.030047869013519444} {:x 10, :y 0.01137800599760603} {:x 11, :y 0.004334478475278515} {:x 12, :y 0.0014899769758769834} {:x 13, :y 3.837819483319515E-4} {:x 14, :y 2.483294959794978E-4} {:x 15, :y 4.51508174508177E-5})}], :marks [{:type \"rect\", :from {:data \"b6d5b576-692c-4a84-bb85-1d8abfcf9ccb\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"b6d5b576-692c-4a84-bb85-1d8abfcf9ccb\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"b6d5b576-692c-4a84-bb85-1d8abfcf9ccb\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"2ab6579f-d809-4591-98ea-aac29d2accc1","values":[{"x":0,"y":0.020050523817695055},{"x":1,"y":0.12118948094455258},{"x":2,"y":0.0685220286006852},{"x":3,"y":0.0},{"x":4,"y":0.0},{"x":5,"y":0.33137652909136334},{"x":6,"y":0.22199509321261565},{"x":7,"y":0.12834927760523412},{"x":8,"y":0.06288170601169404},{"x":9,"y":0.02833946281244483},{"x":10,"y":0.011349291045013778},{"x":11,"y":0.003986267298927514},{"x":12,"y":0.0014170188721193543},{"x":13,"y":3.708176965825454E-4},{"x":14,"y":1.5892152993304522E-4}]}],"marks":[{"type":"rect","from":{"data":"2ab6579f-d809-4591-98ea-aac29d2accc1"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"2ab6579f-d809-4591-98ea-aac29d2accc1","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"2ab6579f-d809-4591-98ea-aac29d2accc1","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"2ab6579f-d809-4591-98ea-aac29d2accc1\", :values ({:x 0, :y 0.020050523817695055} {:x 1, :y 0.12118948094455258} {:x 2, :y 0.0685220286006852} {:x 3, :y 0.0} {:x 4, :y 0.0} {:x 5, :y 0.33137652909136334} {:x 6, :y 0.22199509321261565} {:x 7, :y 0.12834927760523412} {:x 8, :y 0.06288170601169404} {:x 9, :y 0.02833946281244483} {:x 10, :y 0.011349291045013778} {:x 11, :y 0.003986267298927514} {:x 12, :y 0.0014170188721193543} {:x 13, :y 3.708176965825454E-4} {:x 14, :y 1.5892152993304522E-4})}], :marks [{:type \"rect\", :from {:data \"2ab6579f-d809-4591-98ea-aac29d2accc1\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"2ab6579f-d809-4591-98ea-aac29d2accc1\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"2ab6579f-d809-4591-98ea-aac29d2accc1\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"98745fe6-8a0b-4fa1-aad9-d373d19a4c9d","values":[{"x":4.045757490560675,"y":-0.9355563204339746},{"x":4.096910013008056,"y":-0.9284991967191643},{"x":4.154901959985743,"y":-0.9305750596322869},{"x":4.221848749616356,"y":-0.9341718861748604},{"x":4.30102999566398,"y":-0.9384861550542369},{"x":4.3979400086720375,"y":-0.9399717689985287},{"x":4.5228787452803365,"y":-0.9348496557882597},{"x":4.698970004336019,"y":-0.9335751462162191},{"x":5.0,"y":-0.9302843439090204}]}],"marks":[{"type":"line","from":{"data":"98745fe6-8a0b-4fa1-aad9-d373d19a4c9d"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#05A"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"98745fe6-8a0b-4fa1-aad9-d373d19a4c9d","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"98745fe6-8a0b-4fa1-aad9-d373d19a4c9d","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"log number of samples","titleOffset":30},{"type":"y","scale":"y","title":"log KL divergence","titleOffset":45}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"98745fe6-8a0b-4fa1-aad9-d373d19a4c9d\", :values ({:x 4.045757490560675, :y -0.9355563204339746} {:x 4.096910013008056, :y -0.9284991967191643} {:x 4.154901959985743, :y -0.9305750596322869} {:x 4.221848749616356, :y -0.9341718861748604} {:x 4.30102999566398, :y -0.9384861550542369} {:x 4.3979400086720375, :y -0.9399717689985287} {:x 4.5228787452803365, :y -0.9348496557882597} {:x 4.698970004336019, :y -0.9335751462162191} {:x 5.0, :y -0.9302843439090204})}], :marks [{:type \"line\", :from {:data \"98745fe6-8a0b-4fa1-aad9-d373d19a4c9d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#05A\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"98745fe6-8a0b-4fa1-aad9-d373d19a4c9d\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"98745fe6-8a0b-4fa1-aad9-d373d19a4c9d\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"log number of samples\", :titleOffset 30} {:type \"y\", :scale \"y\", :title \"log KL divergence\", :titleOffset 45}]}}"}
;; <=
