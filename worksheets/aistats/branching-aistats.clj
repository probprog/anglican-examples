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
        (if (> l 0) 
          (do
            (observe (poisson l) 6)
            r)
          nil))))
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
;;; &quot;Elapsed time: 2465.434473 msecs&quot;
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
       (remove #(nil? (key %)))
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"ee1435c7-41b9-4119-b654-40405a5e4b7c","values":[{"x":0,"y":0.021319202125261217},{"x":1,"y":0.11754771427992758},{"x":2,"y":0.06768093791154037},{"x":3,"y":9.929461725408212E-10},{"x":4,"y":7.738093610233664E-54},{"x":5,"y":0.33674310772832033},{"x":6,"y":0.22394917036119216},{"x":7,"y":0.12488204235289052},{"x":8,"y":0.06318588680056825},{"x":9,"y":0.028155920591832792},{"x":10,"y":0.011066574986963621},{"x":11,"y":0.003596636870763196},{"x":12,"y":0.0011066574986963687},{"x":13,"y":5.746106243231137E-4},{"x":14,"y":1.7025499979944113E-4},{"x":15,"y":2.12818749749301E-5}]}],"marks":[{"type":"rect","from":{"data":"ee1435c7-41b9-4119-b654-40405a5e4b7c"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"ee1435c7-41b9-4119-b654-40405a5e4b7c","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"ee1435c7-41b9-4119-b654-40405a5e4b7c","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"ee1435c7-41b9-4119-b654-40405a5e4b7c\", :values ({:x 0, :y 0.021319202125261217} {:x 1, :y 0.11754771427992758} {:x 2, :y 0.06768093791154037} {:x 3, :y 9.929461725408212E-10} {:x 4, :y 7.738093610233664E-54} {:x 5, :y 0.33674310772832033} {:x 6, :y 0.22394917036119216} {:x 7, :y 0.12488204235289052} {:x 8, :y 0.06318588680056825} {:x 9, :y 0.028155920591832792} {:x 10, :y 0.011066574986963621} {:x 11, :y 0.003596636870763196} {:x 12, :y 0.0011066574986963687} {:x 13, :y 5.746106243231137E-4} {:x 14, :y 1.7025499979944113E-4} {:x 15, :y 2.12818749749301E-5})}], :marks [{:type \"rect\", :from {:data \"ee1435c7-41b9-4119-b654-40405a5e4b7c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"ee1435c7-41b9-4119-b654-40405a5e4b7c\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"ee1435c7-41b9-4119-b654-40405a5e4b7c\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"9aead214-90d4-40ad-9ad0-875d3ba202e7","values":[{"x":0,"y":0.020050523817695055},{"x":1,"y":0.12118948094455258},{"x":2,"y":0.0685220286006852},{"x":3,"y":0.0},{"x":4,"y":0.0},{"x":5,"y":0.33137652909136334},{"x":6,"y":0.22199509321261565},{"x":7,"y":0.12834927760523412},{"x":8,"y":0.06288170601169404},{"x":9,"y":0.02833946281244483},{"x":10,"y":0.011349291045013778},{"x":11,"y":0.003986267298927514},{"x":12,"y":0.0014170188721193543},{"x":13,"y":3.708176965825454E-4},{"x":14,"y":1.5892152993304522E-4}]}],"marks":[{"type":"rect","from":{"data":"9aead214-90d4-40ad-9ad0-875d3ba202e7"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"9aead214-90d4-40ad-9ad0-875d3ba202e7","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"9aead214-90d4-40ad-9ad0-875d3ba202e7","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"9aead214-90d4-40ad-9ad0-875d3ba202e7\", :values ({:x 0, :y 0.020050523817695055} {:x 1, :y 0.12118948094455258} {:x 2, :y 0.0685220286006852} {:x 3, :y 0.0} {:x 4, :y 0.0} {:x 5, :y 0.33137652909136334} {:x 6, :y 0.22199509321261565} {:x 7, :y 0.12834927760523412} {:x 8, :y 0.06288170601169404} {:x 9, :y 0.02833946281244483} {:x 10, :y 0.011349291045013778} {:x 11, :y 0.003986267298927514} {:x 12, :y 0.0014170188721193543} {:x 13, :y 3.708176965825454E-4} {:x 14, :y 1.5892152993304522E-4})}], :marks [{:type \"rect\", :from {:data \"9aead214-90d4-40ad-9ad0-875d3ba202e7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"9aead214-90d4-40ad-9ad0-875d3ba202e7\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"9aead214-90d4-40ad-9ad0-875d3ba202e7\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
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
              (remove #(nil? (key %)))
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"52ddc61f-e1d0-4c7c-ad2c-276cbd04e32f","values":[{"x":4.045757490560675,"y":-2.8680615793368673},{"x":4.096910013008056,"y":-3.001237887177251},{"x":4.154901959985743,"y":-3.0449579993083495},{"x":4.221848749616356,"y":-2.9290740321317217},{"x":4.30102999566398,"y":-2.972181567464205},{"x":4.3979400086720375,"y":-3.053387816386841},{"x":4.5228787452803365,"y":-3.2346725652683186},{"x":4.698970004336019,"y":-3.296449304514896},{"x":5.0,"y":-3.4853353851775015}]}],"marks":[{"type":"line","from":{"data":"52ddc61f-e1d0-4c7c-ad2c-276cbd04e32f"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#05A"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"52ddc61f-e1d0-4c7c-ad2c-276cbd04e32f","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"52ddc61f-e1d0-4c7c-ad2c-276cbd04e32f","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"log number of samples","titleOffset":30},{"type":"y","scale":"y","title":"log KL divergence","titleOffset":45}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"52ddc61f-e1d0-4c7c-ad2c-276cbd04e32f\", :values ({:x 4.045757490560675, :y -2.8680615793368673} {:x 4.096910013008056, :y -3.001237887177251} {:x 4.154901959985743, :y -3.0449579993083495} {:x 4.221848749616356, :y -2.9290740321317217} {:x 4.30102999566398, :y -2.972181567464205} {:x 4.3979400086720375, :y -3.053387816386841} {:x 4.5228787452803365, :y -3.2346725652683186} {:x 4.698970004336019, :y -3.296449304514896} {:x 5.0, :y -3.4853353851775015})}], :marks [{:type \"line\", :from {:data \"52ddc61f-e1d0-4c7c-ad2c-276cbd04e32f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#05A\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"52ddc61f-e1d0-4c7c-ad2c-276cbd04e32f\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"52ddc61f-e1d0-4c7c-ad2c-276cbd04e32f\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"log number of samples\", :titleOffset 30} {:type \"y\", :scale \"y\", :title \"log KL divergence\", :titleOffset 45}]}}"}
;; <=

;; @@

;; @@
