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
        (predict :r r)
        (predict :l l))))
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
  "posterior on l (ranged 0 ... 15), calculated by enumeration"
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
(def number-of-samples 1000000)

(def samples
  (->> (doquery :importance branching [])
       (take number-of-samples)
       doall
       time))
;; @@
;; ->
;;; &quot;Elapsed time: 18375.681838 msecs&quot;
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
       (collect-by :r)
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
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"658a8a79-e122-4cc5-81cc-158a7b48081d","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"658a8a79-e122-4cc5-81cc-158a7b48081d","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"658a8a79-e122-4cc5-81cc-158a7b48081d"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"658a8a79-e122-4cc5-81cc-158a7b48081d","values":[{"x":0,"y":0.020702040141581654},{"x":1,"y":0.12056375461682114},{"x":2,"y":0.06822363937670387},{"x":3,"y":9.940909380176696E-10},{"x":4,"y":7.890880844891801E-54},{"x":5,"y":0.3329706821572669},{"x":6,"y":0.22222830068106306},{"x":7,"y":0.1269574693064457},{"x":8,"y":0.06274691456786273},{"x":9,"y":0.028039255148297353},{"x":10,"y":0.011444941771647281},{"x":11,"y":0.004271613598688119},{"x":12,"y":0.0013230284512645123},{"x":13,"y":3.9413891060215237E-4},{"x":14,"y":1.0013258809892539E-4},{"x":15,"y":2.3435286576344227E-5},{"x":16,"y":8.521922391397886E-6},{"x":17,"y":2.130480597849471E-6}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"658a8a79-e122-4cc5-81cc-158a7b48081d\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"658a8a79-e122-4cc5-81cc-158a7b48081d\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"658a8a79-e122-4cc5-81cc-158a7b48081d\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"658a8a79-e122-4cc5-81cc-158a7b48081d\", :values ({:x 0, :y 0.020702040141581654} {:x 1, :y 0.12056375461682114} {:x 2, :y 0.06822363937670387} {:x 3, :y 9.940909380176696E-10} {:x 4, :y 7.890880844891801E-54} {:x 5, :y 0.3329706821572669} {:x 6, :y 0.22222830068106306} {:x 7, :y 0.1269574693064457} {:x 8, :y 0.06274691456786273} {:x 9, :y 0.028039255148297353} {:x 10, :y 0.011444941771647281} {:x 11, :y 0.004271613598688119} {:x 12, :y 0.0013230284512645123} {:x 13, :y 3.9413891060215237E-4} {:x 14, :y 1.0013258809892539E-4} {:x 15, :y 2.3435286576344227E-5} {:x 16, :y 8.521922391397886E-6} {:x 17, :y 2.130480597849471E-6})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"}
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
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"fcaba202-46da-44bc-a805-83ff53cef8aa","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"fcaba202-46da-44bc-a805-83ff53cef8aa","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"fcaba202-46da-44bc-a805-83ff53cef8aa"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"fcaba202-46da-44bc-a805-83ff53cef8aa","values":[{"x":0,"y":0.020050523817695055},{"x":1,"y":0.12118948094455258},{"x":2,"y":0.0685220286006852},{"x":3,"y":0.0},{"x":4,"y":0.0},{"x":5,"y":0.33137652909136334},{"x":6,"y":0.22199509321261565},{"x":7,"y":0.12834927760523412},{"x":8,"y":0.06288170601169404},{"x":9,"y":0.02833946281244483},{"x":10,"y":0.011349291045013778},{"x":11,"y":0.003986267298927514},{"x":12,"y":0.0014170188721193543},{"x":13,"y":3.708176965825454E-4},{"x":14,"y":1.5892152993304522E-4}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"fcaba202-46da-44bc-a805-83ff53cef8aa\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"fcaba202-46da-44bc-a805-83ff53cef8aa\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"fcaba202-46da-44bc-a805-83ff53cef8aa\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"fcaba202-46da-44bc-a805-83ff53cef8aa\", :values ({:x 0, :y 0.020050523817695055} {:x 1, :y 0.12118948094455258} {:x 2, :y 0.0685220286006852} {:x 3, :y 0.0} {:x 4, :y 0.0} {:x 5, :y 0.33137652909136334} {:x 6, :y 0.22199509321261565} {:x 7, :y 0.12834927760523412} {:x 8, :y 0.06288170601169404} {:x 9, :y 0.02833946281244483} {:x 10, :y 0.011349291045013778} {:x 11, :y 0.003986267298927514} {:x 12, :y 0.0014170188721193543} {:x 13, :y 3.708176965825454E-4} {:x 14, :y 1.5892152993304522E-4})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"}
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
;;; {"type":"vega","content":{"axes":[{"titleOffset":30,"title":"log number of samples","scale":"x","type":"x"},{"titleOffset":45,"title":"log KL divergence","scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"845c9a08-0d0d-437d-8109-336da60f94d4","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"845c9a08-0d0d-437d-8109-336da60f94d4","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"845c9a08-0d0d-437d-8109-336da60f94d4"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#05A"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"845c9a08-0d0d-437d-8109-336da60f94d4","values":[{"x":4.0,"y":-3.013727512156517},{"x":4.30102999566398,"y":-3.4030940078847567},{"x":4.698970004336019,"y":-3.381719805873984},{"x":5.0,"y":-3.459599253277658},{"x":5.301029995663981,"y":-3.6796802515574805},{"x":5.698970004336018,"y":-3.998158334751102},{"x":5.999999999999999,"y":-4.051820312166588}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:titleOffset 30, :title \"log number of samples\", :scale \"x\", :type \"x\"} {:titleOffset 45, :title \"log KL divergence\", :scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"845c9a08-0d0d-437d-8109-336da60f94d4\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"845c9a08-0d0d-437d-8109-336da60f94d4\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"845c9a08-0d0d-437d-8109-336da60f94d4\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#05A\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"845c9a08-0d0d-437d-8109-336da60f94d4\", :values ({:x 4.0, :y -3.013727512156517} {:x 4.30102999566398, :y -3.4030940078847567} {:x 4.698970004336019, :y -3.381719805873984} {:x 5.0, :y -3.459599253277658} {:x 5.301029995663981, :y -3.6796802515574805} {:x 5.698970004336018, :y -3.998158334751102} {:x 5.999999999999999, :y -4.051820312166588})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"}
;; <=
