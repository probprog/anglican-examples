;; gorilla-repl.fileformat = 1

;; **
;;; # INTERNAL RATE OF RETURN, PART I
;;; 
;;; 
;; **

;; @@
(ns irr-1
  (:require [gorilla-plot.core :as plot]
            [clojure.core.matrix :as m])
  (:use clojure.repl
        clojure.pprint
        [anglican core runtime emit stat]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Consider the following financial time series:
;; **

;; @@
(def times [1 2 3 4 5 6 7 8 9])
(def values [12.1 11.99 15.05 19.99 24.59 35.04 43.84 42.82 46.14])

values

(def data (mapv (fn [t v] [t v]) times values))


(plot/compose
  (plot/list-plot data :plot-range [[1 10] :all]))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/times</span>","value":"#'gda-ns/times"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/values</span>","value":"#'gda-ns/values"}],"value":"[#'gda-ns/times,#'gda-ns/values]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>12.1</span>","value":"12.1"},{"type":"html","content":"<span class='clj-double'>11.99</span>","value":"11.99"},{"type":"html","content":"<span class='clj-double'>15.05</span>","value":"15.05"},{"type":"html","content":"<span class='clj-double'>19.99</span>","value":"19.99"},{"type":"html","content":"<span class='clj-double'>24.59</span>","value":"24.59"},{"type":"html","content":"<span class='clj-double'>35.04</span>","value":"35.04"},{"type":"html","content":"<span class='clj-double'>43.84</span>","value":"43.84"},{"type":"html","content":"<span class='clj-double'>42.82</span>","value":"42.82"},{"type":"html","content":"<span class='clj-double'>46.14</span>","value":"46.14"}],"value":"[12.1 11.99 15.05 19.99 24.59 35.04 43.84 42.82 46.14]"}],"value":"[[#'gda-ns/times,#'gda-ns/values],[12.1 11.99 15.05 19.99 24.59 35.04 43.84 42.82 46.14]]"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/data</span>","value":"#'gda-ns/data"}],"value":"[[[#'gda-ns/times,#'gda-ns/values],[12.1 11.99 15.05 19.99 24.59 35.04 43.84 42.82 46.14]],#'gda-ns/data]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[1,10]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"5744ae79-feaf-40de-91b2-b3e2f4468c72","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"5744ae79-feaf-40de-91b2-b3e2f4468c72","values":[{"x":1,"y":12.1},{"x":2,"y":11.99},{"x":3,"y":15.05},{"x":4,"y":19.99},{"x":5,"y":24.59},{"x":6,"y":35.04},{"x":7,"y":43.84},{"x":8,"y":42.82},{"x":9,"y":46.14}]}],"marks":[{"type":"symbol","from":{"data":"5744ae79-feaf-40de-91b2-b3e2f4468c72"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [1 10]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"5744ae79-feaf-40de-91b2-b3e2f4468c72\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"5744ae79-feaf-40de-91b2-b3e2f4468c72\", :values ({:x 1, :y 12.1} {:x 2, :y 11.99} {:x 3, :y 15.05} {:x 4, :y 19.99} {:x 5, :y 24.59} {:x 6, :y 35.04} {:x 7, :y 43.84} {:x 8, :y 42.82} {:x 9, :y 46.14})}), :marks ({:type \"symbol\", :from {:data \"5744ae79-feaf-40de-91b2-b3e2f4468c72\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}"}],"value":"[[[[#'gda-ns/times,#'gda-ns/values],[12.1 11.99 15.05 19.99 24.59 35.04 43.84 42.82 46.14]],#'gda-ns/data],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [1 10]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"5744ae79-feaf-40de-91b2-b3e2f4468c72\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"5744ae79-feaf-40de-91b2-b3e2f4468c72\", :values ({:x 1, :y 12.1} {:x 2, :y 11.99} {:x 3, :y 15.05} {:x 4, :y 19.99} {:x 5, :y 24.59} {:x 6, :y 35.04} {:x 7, :y 43.84} {:x 8, :y 42.82} {:x 9, :y 46.14})}), :marks ({:type \"symbol\", :from {:data \"5744ae79-feaf-40de-91b2-b3e2f4468c72\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}})}}]"}
;; <=

;; **
;;; We will (somewhat inadvisably) model this series as a Gaussian random walk, with noise on the observations. Our goal right now is to infer the drift of this Gaussian random walk - the expected increase of this series at each timestep. We frame this problem as the following query:
;; **

;; @@
(defquery infer-drift [times values]
  (let [drift (sample (normal 0 2))
        noise (sample (exponential 1))
        initial-value 10
        
        projected-values (map #(+ initial-value (* % drift)) times)]
    
    (loop [n 0]
      (if (< n (count data))
        (observe (normal (nth projected-values n) noise) (nth values n))))
    
    drift))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/infer-drift</span>","value":"#'gda-ns/infer-drift"}
;; <=

;; **
;;; We perform inference using sequential Monte Carlo (SMC), and obtain approximately the following posterior distribution over the drift:
;; **

;; @@
(def S 10000)

(def results (map :result (take S 
                                (doquery :smc infer-drift [times values]
                                         :number-of-particles S))))

(plot/histogram results :normalize :probability)
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/S</span>","value":"#'gda-ns/S"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/results</span>","value":"#'gda-ns/results"}],"value":"[#'gda-ns/S,#'gda-ns/results]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"da9f247b-72eb-42c1-8306-e09661580596","values":[{"x":-4.86614356443926,"y":0},{"x":-4.151573256935056,"y":0.0004},{"x":-3.4370029494308527,"y":0.0005},{"x":-2.722432641926649,"y":0.0016},{"x":-2.0078623344224455,"y":0.0041},{"x":-1.293292026918242,"y":0.0099},{"x":-0.5787217194140385,"y":0.0208},{"x":0.13584858809016498,"y":0.0418},{"x":0.8504188955943685,"y":0.0822},{"x":1.564989203098572,"y":0.1679},{"x":2.2795595106027755,"y":0.4717},{"x":2.994129818106979,"y":0.1574},{"x":3.7087001256111827,"y":0.0335},{"x":4.423270433115386,"y":0.0069},{"x":5.137840740619589,"y":0.001},{"x":5.852411048123793,"y":0.0003},{"x":6.566981355627997,"y":0}]}],"marks":[{"type":"line","from":{"data":"da9f247b-72eb-42c1-8306-e09661580596"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"da9f247b-72eb-42c1-8306-e09661580596","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"da9f247b-72eb-42c1-8306-e09661580596","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"da9f247b-72eb-42c1-8306-e09661580596\", :values ({:x -4.86614356443926, :y 0} {:x -4.151573256935056, :y 4.0E-4} {:x -3.4370029494308527, :y 5.0E-4} {:x -2.722432641926649, :y 0.0016} {:x -2.0078623344224455, :y 0.0041} {:x -1.293292026918242, :y 0.0099} {:x -0.5787217194140385, :y 0.0208} {:x 0.13584858809016498, :y 0.0418} {:x 0.8504188955943685, :y 0.0822} {:x 1.564989203098572, :y 0.1679} {:x 2.2795595106027755, :y 0.4717} {:x 2.994129818106979, :y 0.1574} {:x 3.7087001256111827, :y 0.0335} {:x 4.423270433115386, :y 0.0069} {:x 5.137840740619589, :y 0.001} {:x 5.852411048123793, :y 3.0E-4} {:x 6.566981355627997, :y 0})}], :marks [{:type \"line\", :from {:data \"da9f247b-72eb-42c1-8306-e09661580596\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"da9f247b-72eb-42c1-8306-e09661580596\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"da9f247b-72eb-42c1-8306-e09661580596\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[[#'gda-ns/S,#'gda-ns/results],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"da9f247b-72eb-42c1-8306-e09661580596\", :values ({:x -4.86614356443926, :y 0} {:x -4.151573256935056, :y 4.0E-4} {:x -3.4370029494308527, :y 5.0E-4} {:x -2.722432641926649, :y 0.0016} {:x -2.0078623344224455, :y 0.0041} {:x -1.293292026918242, :y 0.0099} {:x -0.5787217194140385, :y 0.0208} {:x 0.13584858809016498, :y 0.0418} {:x 0.8504188955943685, :y 0.0822} {:x 1.564989203098572, :y 0.1679} {:x 2.2795595106027755, :y 0.4717} {:x 2.994129818106979, :y 0.1574} {:x 3.7087001256111827, :y 0.0335} {:x 4.423270433115386, :y 0.0069} {:x 5.137840740619589, :y 0.001} {:x 5.852411048123793, :y 3.0E-4} {:x 6.566981355627997, :y 0})}], :marks [{:type \"line\", :from {:data \"da9f247b-72eb-42c1-8306-e09661580596\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"da9f247b-72eb-42c1-8306-e09661580596\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"da9f247b-72eb-42c1-8306-e09661580596\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"}
;; <=

;; **
;;; So we see that this inference problem is not particularly challenging. However, why does the drift of this time series concern us? The answer is because it will allow us to predict future values of this financial time series, given previous values. In the below query, we augment the above model by first inferring the drift and noise of this Gaussian random walk, but then using these values to sample a possible value of the time series at a future time `T`:
;; **

;; @@

(def T 15)

(defquery predict-process [times values T]
  (let [drift (sample (normal 0 2))
        noise (sample (exponential 1))
        initial-value 10
        
        projected-values (map #(+ initial-value (* % drift)) times)]
    
    (loop [n 0]
      (if (< n (count data))
        (observe (normal (nth projected-values n) noise) (nth values n))))
    
    (sample (normal
              (+ initial-value (* T drift))
              noise))))

(def S 10000)

(def results (map :result (take S 
                                (doquery :smc predict-process [times values T]
                                         :number-of-particles S))))

(plot/histogram results :normalize :probability)
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/T</span>","value":"#'gda-ns/T"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/predict-process</span>","value":"#'gda-ns/predict-process"}],"value":"[#'gda-ns/T,#'gda-ns/predict-process]"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/S</span>","value":"#'gda-ns/S"}],"value":"[[#'gda-ns/T,#'gda-ns/predict-process],#'gda-ns/S]"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/results</span>","value":"#'gda-ns/results"}],"value":"[[[#'gda-ns/T,#'gda-ns/predict-process],#'gda-ns/S],#'gda-ns/results]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"729abf11-178a-47ea-8efd-bc10b99b17aa","values":[{"x":-68.57327343498059,"y":0},{"x":-57.78275914815296,"y":0.0002},{"x":-46.99224486132533,"y":0.0002},{"x":-36.2017305744977,"y":0.0012},{"x":-25.411216287670072,"y":0.0023},{"x":-14.620702000842444,"y":0.0057},{"x":-3.8301877140148157,"y":0.0141},{"x":6.9603265728128125,"y":0.0287},{"x":17.75084085964044,"y":0.0553},{"x":28.54135514646807,"y":0.1158},{"x":39.3318694332957,"y":0.2578},{"x":50.12238372012333,"y":0.4351},{"x":60.91289800695096,"y":0.0637},{"x":71.70341229377858,"y":0.0146},{"x":82.49392658060621,"y":0.0042},{"x":93.28444086743384,"y":0.0011},{"x":104.07495515426147,"y":0}]}],"marks":[{"type":"line","from":{"data":"729abf11-178a-47ea-8efd-bc10b99b17aa"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"729abf11-178a-47ea-8efd-bc10b99b17aa","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"729abf11-178a-47ea-8efd-bc10b99b17aa","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"729abf11-178a-47ea-8efd-bc10b99b17aa\", :values ({:x -68.57327343498059, :y 0} {:x -57.78275914815296, :y 2.0E-4} {:x -46.99224486132533, :y 2.0E-4} {:x -36.2017305744977, :y 0.0012} {:x -25.411216287670072, :y 0.0023} {:x -14.620702000842444, :y 0.0057} {:x -3.8301877140148157, :y 0.0141} {:x 6.9603265728128125, :y 0.0287} {:x 17.75084085964044, :y 0.0553} {:x 28.54135514646807, :y 0.1158} {:x 39.3318694332957, :y 0.2578} {:x 50.12238372012333, :y 0.4351} {:x 60.91289800695096, :y 0.0637} {:x 71.70341229377858, :y 0.0146} {:x 82.49392658060621, :y 0.0042} {:x 93.28444086743384, :y 0.0011} {:x 104.07495515426147, :y 0})}], :marks [{:type \"line\", :from {:data \"729abf11-178a-47ea-8efd-bc10b99b17aa\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"729abf11-178a-47ea-8efd-bc10b99b17aa\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"729abf11-178a-47ea-8efd-bc10b99b17aa\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[[[[#'gda-ns/T,#'gda-ns/predict-process],#'gda-ns/S],#'gda-ns/results],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"729abf11-178a-47ea-8efd-bc10b99b17aa\", :values ({:x -68.57327343498059, :y 0} {:x -57.78275914815296, :y 2.0E-4} {:x -46.99224486132533, :y 2.0E-4} {:x -36.2017305744977, :y 0.0012} {:x -25.411216287670072, :y 0.0023} {:x -14.620702000842444, :y 0.0057} {:x -3.8301877140148157, :y 0.0141} {:x 6.9603265728128125, :y 0.0287} {:x 17.75084085964044, :y 0.0553} {:x 28.54135514646807, :y 0.1158} {:x 39.3318694332957, :y 0.2578} {:x 50.12238372012333, :y 0.4351} {:x 60.91289800695096, :y 0.0637} {:x 71.70341229377858, :y 0.0146} {:x 82.49392658060621, :y 0.0042} {:x 93.28444086743384, :y 0.0011} {:x 104.07495515426147, :y 0})}], :marks [{:type \"line\", :from {:data \"729abf11-178a-47ea-8efd-bc10b99b17aa\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"729abf11-178a-47ea-8efd-bc10b99b17aa\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"729abf11-178a-47ea-8efd-bc10b99b17aa\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"}
;; <=

;; **
;;; Predicting the future value of a financial time series is useful for a number of reasons. One big one, though, is assessing the value of an investment. We often compare investments using a quantity called IRR, which is calculated in the below Clojure code. In broad strokes, the IRR of a cash-flow is the risk-free interest rate you'd need to have in order to achieve the same return as that cash-flow. For instance, if you buy $1,000 worth of stock and sell it in 10 years for $2,000, the IRR of this cash-flow is approximately 7.2%. This tells us that, if we could have found a bank that gave us a guaranteed 7.2% interest rate (which we couldn't have), we'd have the same amount in 10 years, but with less risk. In general, if an investment has a higher IRR, it is more desirable.
;; **

;; @@
(defn npv-helper [rate lst]
  (apply +
         (map
           (fn [x i] (/ x
                        (Math/pow (+ 1 rate) i)))
           lst
           (range 1 (+ 1 (count lst))))))


;; for Newton-Raphson, we need the derivative of the above npv-helper function:
(defn npv-derivative [rate lst]
  (- (reduce +
            (map
              (fn [x i] (/
                          (* i x)
                          (Math/pow (+ 1 rate) (+ i 1))))
              lst
              (range 1 (+ 1 (count lst)))))))

;; for Halley's method, we need the second derivative:
(defn npv-second-derivative [rate lst]
  (reduce +
         (map
           (fn [x i] (/
                       (* i (+ i 1) x)
                       (Math/pow (+ 1 rate) (+ i 2))))
           lst
           (range 1 (+ 1 (count lst))))))




;; IRR-helper implements the IRR function using Halley's method
;; using the above two derivatives of the NPV function.

(defn newton-raphson [cash-flows guess]
  (let [max-iteration-count 20
        accuracy 0.0001]
    (loop [i 0
           x guess]
      (if (= i max-iteration-count)
        Double/NaN
        (let [value (npv-helper x cash-flows)
              derivative (npv-derivative x cash-flows)

              ratio (/ value derivative)
              next-x (- x ratio)
              current-accuracy (Math/abs (- next-x x))]

          (if (<= current-accuracy accuracy)
            x
            (recur (+ i 1) next-x)))))))

(defn irr [cash-flows guess]
  (newton-raphson cash-flows guess))



(irr [-1000 0 0 0 0 0 0 0 0 0 2000] 0.1)
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/npv-helper</span>","value":"#'gda-ns/npv-helper"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/npv-derivative</span>","value":"#'gda-ns/npv-derivative"}],"value":"[#'gda-ns/npv-helper,#'gda-ns/npv-derivative]"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/npv-second-derivative</span>","value":"#'gda-ns/npv-second-derivative"}],"value":"[[#'gda-ns/npv-helper,#'gda-ns/npv-derivative],#'gda-ns/npv-second-derivative]"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/newton-raphson</span>","value":"#'gda-ns/newton-raphson"}],"value":"[[[#'gda-ns/npv-helper,#'gda-ns/npv-derivative],#'gda-ns/npv-second-derivative],#'gda-ns/newton-raphson]"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/irr</span>","value":"#'gda-ns/irr"}],"value":"[[[[#'gda-ns/npv-helper,#'gda-ns/npv-derivative],#'gda-ns/npv-second-derivative],#'gda-ns/newton-raphson],#'gda-ns/irr]"},{"type":"html","content":"<span class='clj-double'>0.07177327702983588</span>","value":"0.07177327702983588"}],"value":"[[[[[#'gda-ns/npv-helper,#'gda-ns/npv-derivative],#'gda-ns/npv-second-derivative],#'gda-ns/newton-raphson],#'gda-ns/irr],0.07177327702983588]"}
;; <=

;; **
;;; Now let's return to our financial time series from before. We now interpret this as the value of a stock at time t, which we bought at time `t=0` for $10, and are going to sell at time `T=15`. Using the above code, we calculated a distribution over possible values of the stock at time `T=15`. We now use these predictions to calculate a range of possible IRRs for this investment:
;; **

;; @@
;; So that's interesting. But now that we've inferred the drift, we can go on to marginalize OVER the drift & noise parameters we've inferred, in order to project the value of this process at a future time:

(def T 15)

(defquery predict-irr [times values T]
  (declare :primitive irr)
  (let [drift (sample (normal 0 2))
        noise (sample (exponential 1))
        initial-value 10
        
        projected-values (map #(+ initial-value (* % drift)) (range 0 T))
        
        cash-flow (concat
                    (list (- initial-value))
                    (repeat (- T 1) 0)
                    (list (last projected-values)))]
    
    (loop [n 0]
      (if (< n (count data))
        (observe (normal (nth projected-values n) noise) (nth values n))))
    
    (irr cash-flow 0.1)))
    

(def S 10000)

(def results (map :result (take S 
                                (doquery :smc predict-irr [times values T]
                                         :number-of-particles S))))

(def not-nan-irrs (filter #(not (Double/isNaN %)) results))

(plot/histogram not-nan-irrs :normalize :probability :plot-range [[-0.2 0.3] :all])
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/T</span>","value":"#'gda-ns/T"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/predict-irr</span>","value":"#'gda-ns/predict-irr"}],"value":"[#'gda-ns/T,#'gda-ns/predict-irr]"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/S</span>","value":"#'gda-ns/S"}],"value":"[[#'gda-ns/T,#'gda-ns/predict-irr],#'gda-ns/S]"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/results</span>","value":"#'gda-ns/results"}],"value":"[[[#'gda-ns/T,#'gda-ns/predict-irr],#'gda-ns/S],#'gda-ns/results]"},{"type":"html","content":"<span class='clj-var'>#&#x27;gda-ns/not-nan-irrs</span>","value":"#'gda-ns/not-nan-irrs"}],"value":"[[[[#'gda-ns/T,#'gda-ns/predict-irr],#'gda-ns/S],#'gda-ns/results],#'gda-ns/not-nan-irrs]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"68696b76-5af7-483e-8ad5-f27b4fe8332d","values":[{"x":-0.2,"y":0},{"x":-0.1642857142857143,"y":0},{"x":-0.12857142857142861,"y":0},{"x":-0.0928571428571429,"y":0},{"x":-0.05714285714285719,"y":0},{"x":-0.021428571428571477,"y":0.03824542323726663},{"x":0.014285714285714235,"y":0.12941816204458945},{"x":0.04999999999999995,"y":0.2276599601232554},{"x":0.08571428571428566,"y":0.26119267717962663},{"x":0.12142857142857137,"y":0.25774877650897227},{"x":0.15714285714285708,"y":0.081747326445532},{"x":0.19285714285714278,"y":0.003987674460757658},{"x":0.22857142857142848,"y":0},{"x":0.2642857142857142,"y":0},{"x":0.2999999999999999,"y":0},{"x":0.3357142857142856,"y":0},{"x":0.3714285714285713,"y":0}]}],"marks":[{"type":"line","from":{"data":"68696b76-5af7-483e-8ad5-f27b4fe8332d"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-0.2,0.3]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"68696b76-5af7-483e-8ad5-f27b4fe8332d","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"68696b76-5af7-483e-8ad5-f27b4fe8332d\", :values ({:x -0.2, :y 0} {:x -0.1642857142857143, :y 0.0} {:x -0.12857142857142861, :y 0.0} {:x -0.0928571428571429, :y 0.0} {:x -0.05714285714285719, :y 0.0} {:x -0.021428571428571477, :y 0.03824542323726663} {:x 0.014285714285714235, :y 0.12941816204458945} {:x 0.04999999999999995, :y 0.2276599601232554} {:x 0.08571428571428566, :y 0.26119267717962663} {:x 0.12142857142857137, :y 0.25774877650897227} {:x 0.15714285714285708, :y 0.081747326445532} {:x 0.19285714285714278, :y 0.003987674460757658} {:x 0.22857142857142848, :y 0.0} {:x 0.2642857142857142, :y 0.0} {:x 0.2999999999999999, :y 0.0} {:x 0.3357142857142856, :y 0.0} {:x 0.3714285714285713, :y 0})}], :marks [{:type \"line\", :from {:data \"68696b76-5af7-483e-8ad5-f27b4fe8332d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-0.2 0.3]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"68696b76-5af7-483e-8ad5-f27b4fe8332d\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[[[[[#'gda-ns/T,#'gda-ns/predict-irr],#'gda-ns/S],#'gda-ns/results],#'gda-ns/not-nan-irrs],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"68696b76-5af7-483e-8ad5-f27b4fe8332d\", :values ({:x -0.2, :y 0} {:x -0.1642857142857143, :y 0.0} {:x -0.12857142857142861, :y 0.0} {:x -0.0928571428571429, :y 0.0} {:x -0.05714285714285719, :y 0.0} {:x -0.021428571428571477, :y 0.03824542323726663} {:x 0.014285714285714235, :y 0.12941816204458945} {:x 0.04999999999999995, :y 0.2276599601232554} {:x 0.08571428571428566, :y 0.26119267717962663} {:x 0.12142857142857137, :y 0.25774877650897227} {:x 0.15714285714285708, :y 0.081747326445532} {:x 0.19285714285714278, :y 0.003987674460757658} {:x 0.22857142857142848, :y 0.0} {:x 0.2642857142857142, :y 0.0} {:x 0.2999999999999999, :y 0.0} {:x 0.3357142857142856, :y 0.0} {:x 0.3714285714285713, :y 0})}], :marks [{:type \"line\", :from {:data \"68696b76-5af7-483e-8ad5-f27b4fe8332d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-0.2 0.3]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"68696b76-5af7-483e-8ad5-f27b4fe8332d\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"}
;; <=

;; **
;;; We therefore see that, according to our model, this does not appear to be an extremely good investment. While it is certainly possible that the IRR of this investment might exceed 10% - which would be quite good - it is also quite likely that the IRR will be less than 3%, which we could probably obtain risk-free at a bank, and it is even quite likely to be negative. Therefore, while our results don't give a conclusive recommendation about whether to make this investment or not, they clearly mark it as a risky investment.
;; **
