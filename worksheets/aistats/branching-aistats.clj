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
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/kl-categorical</span>","value":"#'branching-aistats/kl-categorical"}],"value":"[nil,#'branching-aistats/kl-categorical]"}
;; <=

;; **
;;; ## Define model
;; **

;; @@
(defn fib [n]
  "returns the n-th number in the Fibonacci sequence"
  (loop [a 1
         b 1 
         m 0]
    (if (= m n)
      a
      (recur b (+ a b) (inc m)))))

(defquery branching
  "A simple example illustrating flow control with
  dependence on random choices"
  []

  (declare :primitive fib)
  (let [count-prior (poisson 4)
        r (sample count-prior)
        l (if (< 4 r)
            6
            (+ (fib (* 3 r))
               (sample count-prior)))]

    (observe (poisson l) 6)
    r))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/fib</span>","value":"#'branching-aistats/fib"},{"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/branching</span>","value":"#'branching-aistats/branching"}],"value":"[#'branching-aistats/fib,#'branching-aistats/branching]"}
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
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/-inf</span>","value":"#'branching-aistats/-inf"},{"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/posterior</span>","value":"#'branching-aistats/posterior"}],"value":"[#'branching-aistats/-inf,#'branching-aistats/posterior]"}
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
;;; &quot;Elapsed time: 691.339992 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/number-of-samples</span>","value":"#'branching-aistats/number-of-samples"},{"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/samples</span>","value":"#'branching-aistats/samples"}],"value":"[#'branching-aistats/number-of-samples,#'branching-aistats/samples]"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"513b1d9c-f1be-4006-aed9-d9ddfc827cef","values":[{"x":0,"y":0.029391068423878213},{"x":1,"y":0.128020582704366},{"x":2,"y":0.005273603374864514},{"x":3,"y":1.2909644497930088E-17},{"x":4,"y":3.2541478897778905E-91},{"x":5,"y":0.3507232723410991},{"x":6,"y":0.23727512185925673},{"x":7,"y":0.13838427860755606},{"x":8,"y":0.06451942340274397},{"x":9,"y":0.028350805130314394},{"x":10,"y":0.012153554338747942},{"x":11,"y":0.0036842567681232364},{"x":12,"y":0.0015500836402469657},{"x":13,"y":4.942295664555556E-4},{"x":14,"y":1.3478988176060598E-4},{"x":15,"y":4.49299605868686E-5}]}],"marks":[{"type":"rect","from":{"data":"513b1d9c-f1be-4006-aed9-d9ddfc827cef"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"513b1d9c-f1be-4006-aed9-d9ddfc827cef","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"513b1d9c-f1be-4006-aed9-d9ddfc827cef","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"513b1d9c-f1be-4006-aed9-d9ddfc827cef\", :values ({:x 0, :y 0.029391068423878213} {:x 1, :y 0.128020582704366} {:x 2, :y 0.005273603374864514} {:x 3, :y 1.2909644497930088E-17} {:x 4, :y 3.2541478897778905E-91} {:x 5, :y 0.3507232723410991} {:x 6, :y 0.23727512185925673} {:x 7, :y 0.13838427860755606} {:x 8, :y 0.06451942340274397} {:x 9, :y 0.028350805130314394} {:x 10, :y 0.012153554338747942} {:x 11, :y 0.0036842567681232364} {:x 12, :y 0.0015500836402469657} {:x 13, :y 4.942295664555556E-4} {:x 14, :y 1.3478988176060598E-4} {:x 15, :y 4.49299605868686E-5})}], :marks [{:type \"rect\", :from {:data \"513b1d9c-f1be-4006-aed9-d9ddfc827cef\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"513b1d9c-f1be-4006-aed9-d9ddfc827cef\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"513b1d9c-f1be-4006-aed9-d9ddfc827cef\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"76c97bd1-6637-4a23-b5c4-c35a91371d6c","values":[{"x":0,"y":0.020050523817695055},{"x":1,"y":0.12118948094455258},{"x":2,"y":0.0685220286006852},{"x":3,"y":0.0},{"x":4,"y":0.0},{"x":5,"y":0.33137652909136334},{"x":6,"y":0.22199509321261565},{"x":7,"y":0.12834927760523412},{"x":8,"y":0.06288170601169404},{"x":9,"y":0.02833946281244483},{"x":10,"y":0.011349291045013778},{"x":11,"y":0.003986267298927514},{"x":12,"y":0.0014170188721193543},{"x":13,"y":3.708176965825454E-4},{"x":14,"y":1.5892152993304522E-4}]}],"marks":[{"type":"rect","from":{"data":"76c97bd1-6637-4a23-b5c4-c35a91371d6c"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"76c97bd1-6637-4a23-b5c4-c35a91371d6c","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"76c97bd1-6637-4a23-b5c4-c35a91371d6c","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"76c97bd1-6637-4a23-b5c4-c35a91371d6c\", :values ({:x 0, :y 0.020050523817695055} {:x 1, :y 0.12118948094455258} {:x 2, :y 0.0685220286006852} {:x 3, :y 0.0} {:x 4, :y 0.0} {:x 5, :y 0.33137652909136334} {:x 6, :y 0.22199509321261565} {:x 7, :y 0.12834927760523412} {:x 8, :y 0.06288170601169404} {:x 9, :y 0.02833946281244483} {:x 10, :y 0.011349291045013778} {:x 11, :y 0.003986267298927514} {:x 12, :y 0.0014170188721193543} {:x 13, :y 3.708176965825454E-4} {:x 14, :y 1.5892152993304522E-4})}], :marks [{:type \"rect\", :from {:data \"76c97bd1-6637-4a23-b5c4-c35a91371d6c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"76c97bd1-6637-4a23-b5c4-c35a91371d6c\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"76c97bd1-6637-4a23-b5c4-c35a91371d6c\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
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
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/num-sample-range</span>","value":"#'branching-aistats/num-sample-range"},{"type":"html","content":"<span class='clj-var'>#&#x27;branching-aistats/KL-errors</span>","value":"#'branching-aistats/KL-errors"}],"value":"[#'branching-aistats/num-sample-range,#'branching-aistats/KL-errors]"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"ddbca268-db02-40db-8e7a-c0770d894d13","values":[{"x":4.045757490560675,"y":-0.9399219339650511},{"x":4.096910013008056,"y":-0.9422959240641892},{"x":4.154901959985743,"y":-0.9454144196684358},{"x":4.221848749616356,"y":-0.9429369445195382},{"x":4.30102999566398,"y":-0.9419710957562495},{"x":4.3979400086720375,"y":-0.9419865591872898},{"x":4.5228787452803365,"y":-0.9421506794924434},{"x":4.698970004336019,"y":-0.9385811050475884},{"x":5.0,"y":-0.9359857804324065}]}],"marks":[{"type":"line","from":{"data":"ddbca268-db02-40db-8e7a-c0770d894d13"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#05A"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"ddbca268-db02-40db-8e7a-c0770d894d13","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"ddbca268-db02-40db-8e7a-c0770d894d13","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"log number of samples","titleOffset":30},{"type":"y","scale":"y","title":"log KL divergence","titleOffset":45}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"ddbca268-db02-40db-8e7a-c0770d894d13\", :values ({:x 4.045757490560675, :y -0.9399219339650511} {:x 4.096910013008056, :y -0.9422959240641892} {:x 4.154901959985743, :y -0.9454144196684358} {:x 4.221848749616356, :y -0.9429369445195382} {:x 4.30102999566398, :y -0.9419710957562495} {:x 4.3979400086720375, :y -0.9419865591872898} {:x 4.5228787452803365, :y -0.9421506794924434} {:x 4.698970004336019, :y -0.9385811050475884} {:x 5.0, :y -0.9359857804324065})}], :marks [{:type \"line\", :from {:data \"ddbca268-db02-40db-8e7a-c0770d894d13\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#05A\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"ddbca268-db02-40db-8e7a-c0770d894d13\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"ddbca268-db02-40db-8e7a-c0770d894d13\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"log number of samples\", :titleOffset 30} {:type \"y\", :scale \"y\", :title \"log KL divergence\", :titleOffset 45}]}}"}
;; <=

;; @@

;; @@
