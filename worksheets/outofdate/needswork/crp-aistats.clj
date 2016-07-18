;; gorilla-repl.fileformat = 1

;; **
;;; # CRP Gaussian Mixture (AISTATS)
;;; 
;;; This is the CRP Gaussian mixture benchmark from the 2014 AISTATS paper, with 10 observations and a fully enumerated ground truth posterior.
;; **

;; @@
 (ns crp-aistats
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/kl-categorical</span>","value":"#'crp-aistats/kl-categorical"}
;; <=

;; **
;;; ## Define model
;; **

;; @@
(defquery crp-mixture
  "CRP gaussian mixture model"
  [observations alpha mu beta a b]
  (let [precision-prior (gamma a b)]
    (loop [observations observations
           state-proc (CRP alpha)
           obs-dists {}
           states []]
      (if (empty? observations)
          (count obs-dists)
        (let [state (sample (produce state-proc))
              obs-dist (get obs-dists
                            state
                            (let [l (sample precision-prior)
                                  s (sqrt (/ (* beta l)))
                                  m (sample (normal mu s))]
                              (normal m (sqrt (/ l)))))]
          ;(observe obs-dist (first observations))
          (recur (rest observations)
                 (absorb state-proc state)
                 (assoc obs-dists state obs-dist)
                 (conj states state)))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/crp-mixture</span>","value":"#'crp-aistats/crp-mixture"}
;; <=

;; **
;;; ## Define data and true posterior
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/posterior</span>","value":"#'crp-aistats/posterior"}
;; <=

;; **
;;; ## Run inference
;; **

;; @@
(def number-of-particles 100)
(def number-of-samples 50000)

(def samples
  (->> (doquery :lmh crp-mixture
                [data 1.72 0.0 100.0 1.0 10.0]
                :number-of-particles number-of-particles)
       (take number-of-samples)
       doall
       time))
;; @@
;; ->
;;; &quot;Elapsed time: 28507.216151 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/samples</span>","value":"#'crp-aistats/samples"}
;; <=

;; **
;;; ## Plot Empirical and True Posterior
;; **

;; @@
(def empirical-posterior 
  (->> samples
       (collect-by :result)
       s/empirical-distribution
  	   (into (sorted-map))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/empirical-posterior</span>","value":"#'crp-aistats/empirical-posterior"}
;; <=

;; **
;;; **Empirical Posterior**
;; **

;; @@
(plot/bar-chart (sort (keys posterior))
                (map #(get empirical-posterior % 0.0) 
                     (sort (keys posterior)))) 
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"19ed1ce6-3d73-447a-9eca-acd9bf8909d0","values":[{"x":1,"y":0.13573999999999997},{"x":2,"y":0.3653000000000003},{"x":3,"y":0.34573999999999966},{"x":4,"y":0.13284},{"x":5,"y":0.020100000000000073},{"x":6,"y":2.8000000000000155E-4},{"x":7,"y":0.0},{"x":8,"y":0.0},{"x":9,"y":0.0},{"x":10,"y":0.0}]}],"marks":[{"type":"rect","from":{"data":"19ed1ce6-3d73-447a-9eca-acd9bf8909d0"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"19ed1ce6-3d73-447a-9eca-acd9bf8909d0","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"19ed1ce6-3d73-447a-9eca-acd9bf8909d0","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"19ed1ce6-3d73-447a-9eca-acd9bf8909d0\", :values ({:x 1, :y 0.13573999999999997} {:x 2, :y 0.3653000000000003} {:x 3, :y 0.34573999999999966} {:x 4, :y 0.13284} {:x 5, :y 0.020100000000000073} {:x 6, :y 2.8000000000000155E-4} {:x 7, :y 0.0} {:x 8, :y 0.0} {:x 9, :y 0.0} {:x 10, :y 0.0})}], :marks [{:type \"rect\", :from {:data \"19ed1ce6-3d73-447a-9eca-acd9bf8909d0\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"19ed1ce6-3d73-447a-9eca-acd9bf8909d0\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"19ed1ce6-3d73-447a-9eca-acd9bf8909d0\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"52e70d2e-8e8b-4e47-8555-1c36869738ee","values":[{"x":1,"y":1.0458453073364188E-5},{"x":2,"y":0.3521493160516654},{"x":3,"y":0.40147902040466416},{"x":4,"y":0.19103473668703566},{"x":5,"y":0.04808427876972766},{"x":6,"y":0.006748061503565986},{"x":7,"y":5.090827403037801E-4},{"x":8,"y":1.7630150940851895E-5},{"x":9,"y":1.6031904113694912E-7},{"x":10,"y":3.9501396390265077E-10}]}],"marks":[{"type":"rect","from":{"data":"52e70d2e-8e8b-4e47-8555-1c36869738ee"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"52e70d2e-8e8b-4e47-8555-1c36869738ee","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"52e70d2e-8e8b-4e47-8555-1c36869738ee","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"52e70d2e-8e8b-4e47-8555-1c36869738ee\", :values ({:x 1, :y 1.0458453073364188E-5} {:x 2, :y 0.3521493160516654} {:x 3, :y 0.40147902040466416} {:x 4, :y 0.19103473668703566} {:x 5, :y 0.04808427876972766} {:x 6, :y 0.006748061503565986} {:x 7, :y 5.090827403037801E-4} {:x 8, :y 1.7630150940851895E-5} {:x 9, :y 1.6031904113694912E-7} {:x 10, :y 3.9501396390265077E-10})}], :marks [{:type \"rect\", :from {:data \"52e70d2e-8e8b-4e47-8555-1c36869738ee\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"52e70d2e-8e8b-4e47-8555-1c36869738ee\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"52e70d2e-8e8b-4e47-8555-1c36869738ee\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; ## Plot L2 error relative to true posterior as a function of number of samples
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/KL-errors</span>","value":"#'crp-aistats/KL-errors"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"4444aa5c-de7d-4b99-afac-70daad85773a","values":[{"x":2.6989700043360183,"y":"Infinity"},{"x":2.9999999999999996,"y":"Infinity"},{"x":3.397940008672037,"y":"Infinity"},{"x":3.6989700043360187,"y":"Infinity"},{"x":4.0,"y":"Infinity"},{"x":4.3979400086720375,"y":"Infinity"},{"x":4.698970004336019,"y":"Infinity"}]}],"marks":[{"type":"line","from":{"data":"4444aa5c-de7d-4b99-afac-70daad85773a"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#05A"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"4444aa5c-de7d-4b99-afac-70daad85773a","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"4444aa5c-de7d-4b99-afac-70daad85773a","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"log number of samples","titleOffset":30},{"type":"y","scale":"y","title":"log L2 error","titleOffset":45}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"4444aa5c-de7d-4b99-afac-70daad85773a\", :values ({:x 2.6989700043360183, :y Infinity} {:x 2.9999999999999996, :y Infinity} {:x 3.397940008672037, :y Infinity} {:x 3.6989700043360187, :y Infinity} {:x 4.0, :y Infinity} {:x 4.3979400086720375, :y Infinity} {:x 4.698970004336019, :y Infinity})}], :marks [{:type \"line\", :from {:data \"4444aa5c-de7d-4b99-afac-70daad85773a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#05A\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"4444aa5c-de7d-4b99-afac-70daad85773a\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"4444aa5c-de7d-4b99-afac-70daad85773a\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"log number of samples\", :titleOffset 30} {:type \"y\", :scale \"y\", :title \"log L2 error\", :titleOffset 45}]}}"}
;; <=

;; @@
KL-errors
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>Infinity</span>","value":"Infinity"},{"type":"html","content":"<span class='clj-double'>Infinity</span>","value":"Infinity"},{"type":"html","content":"<span class='clj-double'>Infinity</span>","value":"Infinity"},{"type":"html","content":"<span class='clj-double'>Infinity</span>","value":"Infinity"},{"type":"html","content":"<span class='clj-double'>Infinity</span>","value":"Infinity"},{"type":"html","content":"<span class='clj-double'>Infinity</span>","value":"Infinity"},{"type":"html","content":"<span class='clj-double'>Infinity</span>","value":"Infinity"}],"value":"(Infinity Infinity Infinity Infinity Infinity Infinity Infinity)"}
;; <=

;; @@
(defn crp-prior-1 [num]
  (loop [state-proc (CRP 1.72)
         z []
         N num]
    (if (= 0 N)
      z
      (let [d (produce state-proc)
            nz (sample* d)]
        (recur (absorb state-proc nz) (conj z nz) (dec N))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/crp-prior-1</span>","value":"#'crp-aistats/crp-prior-1"}
;; <=

;; @@
(def Ks-dist-1 (loop [n 400
       Ks []]
  (if (= 0 n)
    (into (sorted-map) (frequencies Ks))
    (let [k (count (frequencies (crp-prior-1 n)))]
      (recur (dec n) (conj Ks k))))))
  
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/Ks-dist-1</span>","value":"#'crp-aistats/Ks-dist-1"}
;; <=

;; @@
(plot/bar-chart (keys Ks-dist-1) (vals Ks-dist-1))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"24da1dd3-1ff3-4459-a9c3-499145dfd759","values":[{"x":1,"y":2},{"x":2,"y":6},{"x":3,"y":10},{"x":4,"y":24},{"x":5,"y":25},{"x":6,"y":50},{"x":7,"y":57},{"x":8,"y":57},{"x":9,"y":55},{"x":10,"y":36},{"x":11,"y":34},{"x":12,"y":13},{"x":13,"y":14},{"x":14,"y":12},{"x":15,"y":3},{"x":17,"y":2}]}],"marks":[{"type":"rect","from":{"data":"24da1dd3-1ff3-4459-a9c3-499145dfd759"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"24da1dd3-1ff3-4459-a9c3-499145dfd759","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"24da1dd3-1ff3-4459-a9c3-499145dfd759","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"24da1dd3-1ff3-4459-a9c3-499145dfd759\", :values ({:x 1, :y 2} {:x 2, :y 6} {:x 3, :y 10} {:x 4, :y 24} {:x 5, :y 25} {:x 6, :y 50} {:x 7, :y 57} {:x 8, :y 57} {:x 9, :y 55} {:x 10, :y 36} {:x 11, :y 34} {:x 12, :y 13} {:x 13, :y 14} {:x 14, :y 12} {:x 15, :y 3} {:x 17, :y 2})}], :marks [{:type \"rect\", :from {:data \"24da1dd3-1ff3-4459-a9c3-499145dfd759\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"24da1dd3-1ff3-4459-a9c3-499145dfd759\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"24da1dd3-1ff3-4459-a9c3-499145dfd759\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@
(defn crp-prior-2 [N]
  (loop [Kplus1 1
         alpha 1.72
         counts []
         n 1
         z []]
    (if (= n N)
      z
      (let [params (zipmap (range 1 (inc Kplus1)) (conj counts alpha))
            ;_ (prn params)
            nz (sample* (categorical params))
            ;_ (prn [nz counts])
            pc (if (< nz (dec Kplus1)) (nth counts (dec nz)) nil)
            ;_ (prn pc)
            Kplus1 (if (>= nz (dec Kplus1)) (inc Kplus1) Kplus1)]
            ;_ (prn Kplus1)]
        (recur Kplus1 
               alpha 
               (if pc 
                 (assoc-in counts [(dec nz)] (inc (if pc pc 0)))
                 (conj counts 1))
               (inc n)
               (conj z nz))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/crp-prior-2</span>","value":"#'crp-aistats/crp-prior-2"}
;; <=

;; @@
(crp-prior-2 50)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>11</span>","value":"11"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>11</span>","value":"11"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"[1 2 2 3 3 5 5 2 1 6 2 8 2 9 4 8 1 2 6 8 8 6 1 2 9 2 2 10 1 7 2 1 1 1 4 11 2 4 8 6 11 1 6 6 7 1 8 2 5]"}
;; <=

;; @@
(def Ks-dist-2 (loop [n 400
       Ks []]
  (if (= 0 n)
    (into (sorted-map) (frequencies Ks))
    (let [k (count (frequencies (crp-prior-2 n)))]
      (recur (dec n) (conj Ks k))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/Ks-dist-2</span>","value":"#'crp-aistats/Ks-dist-2"}
;; <=

;; @@
(plot/bar-chart (keys Ks-dist-2) (vals Ks-dist-2))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"258f6658-12d8-4e1b-bce5-ba3760f9c6ca","values":[{"x":0,"y":1},{"x":1,"y":1},{"x":2,"y":3},{"x":3,"y":2},{"x":4,"y":7},{"x":5,"y":10},{"x":6,"y":20},{"x":7,"y":15},{"x":8,"y":25},{"x":9,"y":25},{"x":10,"y":43},{"x":11,"y":35},{"x":12,"y":33},{"x":13,"y":45},{"x":14,"y":36},{"x":15,"y":37},{"x":16,"y":25},{"x":17,"y":24},{"x":18,"y":7},{"x":19,"y":4},{"x":20,"y":1},{"x":24,"y":1}]}],"marks":[{"type":"rect","from":{"data":"258f6658-12d8-4e1b-bce5-ba3760f9c6ca"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"258f6658-12d8-4e1b-bce5-ba3760f9c6ca","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"258f6658-12d8-4e1b-bce5-ba3760f9c6ca","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"258f6658-12d8-4e1b-bce5-ba3760f9c6ca\", :values ({:x 0, :y 1} {:x 1, :y 1} {:x 2, :y 3} {:x 3, :y 2} {:x 4, :y 7} {:x 5, :y 10} {:x 6, :y 20} {:x 7, :y 15} {:x 8, :y 25} {:x 9, :y 25} {:x 10, :y 43} {:x 11, :y 35} {:x 12, :y 33} {:x 13, :y 45} {:x 14, :y 36} {:x 15, :y 37} {:x 16, :y 25} {:x 17, :y 24} {:x 18, :y 7} {:x 19, :y 4} {:x 20, :y 1} {:x 24, :y 1})}], :marks [{:type \"rect\", :from {:data \"258f6658-12d8-4e1b-bce5-ba3760f9c6ca\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"258f6658-12d8-4e1b-bce5-ba3760f9c6ca\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"258f6658-12d8-4e1b-bce5-ba3760f9c6ca\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@
(defdist categorical-crp-l
  "categorical distribution extended
  by a random sample, for use with CRP"
  [counts alpha] [dist (categorical
                         (vec (conj counts [::new alpha])))
                  K (count counts)]
  (sample* [this]
    (let [s (sample* dist)]
      (if (= s ::new)
        (inc K) s)))

  (observe* [this value]
    (if (contains? counts value)
      ;; The value is one of absorbed values.
      (observe* dist value)
      ;; The value is a new value.
      (observe* dist ::new))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#multifn[print-method 0x6208b5db]</span>","value":"#multifn[print-method 0x6208b5db]"}
;; <=

;; @@
(defproc CRP-l
  "Chinese Restaurant process"
  [alpha] [counts {}]
  (produce [this] (categorical-crp-l counts alpha))
  (absorb [this sample]
    (CRP-l alpha (update-in counts [sample] (fnil inc 0)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#multifn[print-method 0x6208b5db]</span>","value":"#multifn[print-method 0x6208b5db]"}
;; <=

;; @@
(defn crp-prior-3 [num]
  (loop [state-proc (CRP-l 1.72)
         z []
         N num]
    (if (= 0 N)
      z
      (let [d (produce state-proc)
            nz (sample* d)]
        (recur (absorb state-proc nz) (conj z nz) (dec N))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/crp-prior-3</span>","value":"#'crp-aistats/crp-prior-3"}
;; <=

;; @@
(def Ks-dist-3 (loop [n 400
       Ks []]
  (if (= 0 n)
    (into (sorted-map) (frequencies Ks))
    (let [k (count (frequencies (crp-prior-3 n)))]
      (recur (dec n) (conj Ks k))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/Ks-dist-3</span>","value":"#'crp-aistats/Ks-dist-3"}
;; <=

;; @@
(plot/bar-chart (keys Ks-dist-3) (vals Ks-dist-3))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"a5fffb19-f898-4458-a3ca-95345562e932","values":[{"x":1,"y":2},{"x":2,"y":8},{"x":3,"y":13},{"x":4,"y":17},{"x":5,"y":31},{"x":6,"y":44},{"x":7,"y":46},{"x":8,"y":54},{"x":9,"y":59},{"x":10,"y":41},{"x":11,"y":38},{"x":12,"y":17},{"x":13,"y":10},{"x":14,"y":12},{"x":15,"y":4},{"x":16,"y":3},{"x":17,"y":1}]}],"marks":[{"type":"rect","from":{"data":"a5fffb19-f898-4458-a3ca-95345562e932"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"a5fffb19-f898-4458-a3ca-95345562e932","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"a5fffb19-f898-4458-a3ca-95345562e932","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"a5fffb19-f898-4458-a3ca-95345562e932\", :values ({:x 1, :y 2} {:x 2, :y 8} {:x 3, :y 13} {:x 4, :y 17} {:x 5, :y 31} {:x 6, :y 44} {:x 7, :y 46} {:x 8, :y 54} {:x 9, :y 59} {:x 10, :y 41} {:x 11, :y 38} {:x 12, :y 17} {:x 13, :y 10} {:x 14, :y 12} {:x 15, :y 4} {:x 16, :y 3} {:x 17, :y 1})}], :marks [{:type \"rect\", :from {:data \"a5fffb19-f898-4458-a3ca-95345562e932\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"a5fffb19-f898-4458-a3ca-95345562e932\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"a5fffb19-f898-4458-a3ca-95345562e932\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@
(with-primitive-procedures [CRP-l] (defquery crp-mixture-l
  "CRP gaussian mixture model"
  [observations alpha mu beta a b]
  (let [precision-prior (gamma a b)]
    (loop [observations observations
           state-proc (CRP-l alpha)
           obs-dists {}
           states []]
      (if (empty? observations)
          (count obs-dists)
        (let [state (sample (produce state-proc))
             ; _ (prn ["state " state])
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
                 (conj states state))))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/crp-mixture-l</span>","value":"#'crp-aistats/crp-mixture-l"}
;; <=

;; @@
(def samples
  (->> (doquery :smc crp-mixture-l
                [data 1.72 0.0 100.0 1.0 10.0]
                :number-of-particles number-of-particles)
       (take number-of-samples)
       doall
       time))
;; @@
;; ->
;;; &quot;Elapsed time: 40727.169446 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/samples</span>","value":"#'crp-aistats/samples"}
;; <=

;; @@
(def empirical-posterior 
  (->> samples
       (collect-by :result)
       s/empirical-distribution
  	   (into (sorted-map))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;crp-aistats/empirical-posterior</span>","value":"#'crp-aistats/empirical-posterior"}
;; <=

;; @@
(plot/bar-chart (sort (keys posterior))
                (map #(get empirical-posterior % 0.0) 
                     (sort (keys posterior)))) 
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"c7381f10-8e3f-4be8-8e5e-ae7c529f9347","values":[{"x":1,"y":1.8713619308718852E-8},{"x":2,"y":0.026667738284678617},{"x":3,"y":0.17624388178560982},{"x":4,"y":0.37448026369360427},{"x":5,"y":0.29925171487644386},{"x":6,"y":0.10378674495067572},{"x":7,"y":0.018691319964263074},{"x":8,"y":8.775242972225556E-4},{"x":9,"y":7.934338866297761E-7},{"x":10,"y":0.0}]}],"marks":[{"type":"rect","from":{"data":"c7381f10-8e3f-4be8-8e5e-ae7c529f9347"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"c7381f10-8e3f-4be8-8e5e-ae7c529f9347","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"c7381f10-8e3f-4be8-8e5e-ae7c529f9347","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"c7381f10-8e3f-4be8-8e5e-ae7c529f9347\", :values ({:x 1, :y 1.8713619308718852E-8} {:x 2, :y 0.026667738284678617} {:x 3, :y 0.17624388178560982} {:x 4, :y 0.37448026369360427} {:x 5, :y 0.29925171487644386} {:x 6, :y 0.10378674495067572} {:x 7, :y 0.018691319964263074} {:x 8, :y 8.775242972225556E-4} {:x 9, :y 7.934338866297761E-7} {:x 10, :y 0.0})}], :marks [{:type \"rect\", :from {:data \"c7381f10-8e3f-4be8-8e5e-ae7c529f9347\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"c7381f10-8e3f-4be8-8e5e-ae7c529f9347\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"c7381f10-8e3f-4be8-8e5e-ae7c529f9347\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@

;; @@
