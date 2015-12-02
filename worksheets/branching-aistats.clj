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
        (predict :r r)
        (predict :l l))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/branching</span>","value":"#'aistats-examples/branching"}
;; <=

;; **
;;; ## Ground Truth Posterior
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
;;; ## Run inference
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
;;; &quot;Elapsed time: 26357.903 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/samples</span>","value":"#'aistats-examples/samples"}
;; <=

;; **
;;; ## Plot Empirical Posterior and True Posterior on r
;; **

;; @@
(def empirical-posterior 
  (->> samples
       (collect-by :r)
       s/empirical-distribution
  	   (into (sorted-map))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;aistats-examples/empirical-posterior</span>","value":"#'aistats-examples/empirical-posterior"}
;; <=

;; **
;;; **Empirical Posterior**
;; **

;; @@
(plot/bar-chart (keys empirical-posterior)
                (vals empirical-posterior))
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"18963791-6029-49b0-bbb5-c8cec01ed3a4","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"18963791-6029-49b0-bbb5-c8cec01ed3a4","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"18963791-6029-49b0-bbb5-c8cec01ed3a4"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"18963791-6029-49b0-bbb5-c8cec01ed3a4","values":[{"x":0,"y":0.02125000997801276},{"x":1,"y":0.12012582623480521},{"x":2,"y":0.06758472221181641},{"x":3,"y":1.0001741198362927E-9},{"x":4,"y":7.90471721854604E-54},{"x":5,"y":0.3333845181432517},{"x":6,"y":0.22175466853649725},{"x":7,"y":0.12651624214476073},{"x":8,"y":0.06395209561369257},{"x":9,"y":0.028063393797860558},{"x":10,"y":0.011339884606022886},{"x":11,"y":0.003974387940575261},{"x":12,"y":0.0014177516702855743},{"x":13,"y":4.810989151419543E-4},{"x":14,"y":1.1282408186957318E-4},{"x":15,"y":2.76738314019708E-5},{"x":16,"y":1.2772537570140368E-5},{"x":18,"y":2.1287562616900586E-6}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"18963791-6029-49b0-bbb5-c8cec01ed3a4\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"18963791-6029-49b0-bbb5-c8cec01ed3a4\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"18963791-6029-49b0-bbb5-c8cec01ed3a4\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"18963791-6029-49b0-bbb5-c8cec01ed3a4\", :values ({:x 0, :y 0.02125000997801276} {:x 1, :y 0.12012582623480521} {:x 2, :y 0.06758472221181641} {:x 3, :y 1.0001741198362927E-9} {:x 4, :y 7.90471721854604E-54} {:x 5, :y 0.3333845181432517} {:x 6, :y 0.22175466853649725} {:x 7, :y 0.12651624214476073} {:x 8, :y 0.06395209561369257} {:x 9, :y 0.028063393797860558} {:x 10, :y 0.011339884606022886} {:x 11, :y 0.003974387940575261} {:x 12, :y 0.0014177516702855743} {:x 13, :y 4.810989151419543E-4} {:x 14, :y 1.1282408186957318E-4} {:x 15, :y 2.76738314019708E-5} {:x 16, :y 1.2772537570140368E-5} {:x 18, :y 2.1287562616900586E-6})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
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
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"738bd2b8-cee4-4aee-a9d2-bb6be7ba2e77","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"738bd2b8-cee4-4aee-a9d2-bb6be7ba2e77","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"738bd2b8-cee4-4aee-a9d2-bb6be7ba2e77"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"738bd2b8-cee4-4aee-a9d2-bb6be7ba2e77","values":[{"x":0,"y":0.020050523817695055},{"x":1,"y":0.12118948094455258},{"x":2,"y":0.0685220286006852},{"x":3,"y":0.0},{"x":4,"y":0.0},{"x":5,"y":0.33137652909136334},{"x":6,"y":0.22199509321261565},{"x":7,"y":0.12834927760523412},{"x":8,"y":0.06288170601169404},{"x":9,"y":0.02833946281244483},{"x":10,"y":0.011349291045013778},{"x":11,"y":0.003986267298927514},{"x":12,"y":0.0014170188721193543},{"x":13,"y":3.708176965825454E-4},{"x":14,"y":1.5892152993304522E-4}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"738bd2b8-cee4-4aee-a9d2-bb6be7ba2e77\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"738bd2b8-cee4-4aee-a9d2-bb6be7ba2e77\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"738bd2b8-cee4-4aee-a9d2-bb6be7ba2e77\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"738bd2b8-cee4-4aee-a9d2-bb6be7ba2e77\", :values ({:x 0, :y 0.020050523817695055} {:x 1, :y 0.12118948094455258} {:x 2, :y 0.0685220286006852} {:x 3, :y 0.0} {:x 4, :y 0.0} {:x 5, :y 0.33137652909136334} {:x 6, :y 0.22199509321261565} {:x 7, :y 0.12834927760523412} {:x 8, :y 0.06288170601169404} {:x 9, :y 0.02833946281244483} {:x 10, :y 0.011349291045013778} {:x 11, :y 0.003986267298927514} {:x 12, :y 0.0014170188721193543} {:x 13, :y 3.708176965825454E-4} {:x 14, :y 1.5892152993304522E-4})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; ## Plot KL error relative to true posterior as a function of number of samples
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
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"00f68106-1bab-4e6c-adb6-fcb9fa8d1bd7","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"00f68106-1bab-4e6c-adb6-fcb9fa8d1bd7","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"00f68106-1bab-4e6c-adb6-fcb9fa8d1bd7"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#05A"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"00f68106-1bab-4e6c-adb6-fcb9fa8d1bd7","values":[{"x":4.0,"y":-2.8633448213175443},{"x":4.30102999566398,"y":-2.769420636261537},{"x":4.698970004336019,"y":-3.2932439183168194},{"x":5.0,"y":-3.6191270894074865},{"x":5.301029995663981,"y":-3.711997243564591},{"x":5.698970004336018,"y":-3.8677791162307087},{"x":5.999999999999999,"y":-3.8534986382908256}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"00f68106-1bab-4e6c-adb6-fcb9fa8d1bd7\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"00f68106-1bab-4e6c-adb6-fcb9fa8d1bd7\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"00f68106-1bab-4e6c-adb6-fcb9fa8d1bd7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#05A\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"00f68106-1bab-4e6c-adb6-fcb9fa8d1bd7\", :values ({:x 4.0, :y -2.8633448213175443} {:x 4.30102999566398, :y -2.769420636261537} {:x 4.698970004336019, :y -3.2932439183168194} {:x 5.0, :y -3.6191270894074865} {:x 5.301029995663981, :y -3.711997243564591} {:x 5.698970004336018, :y -3.8677791162307087} {:x 5.999999999999999, :y -3.8534986382908256})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@

;; @@
