;; gorilla-repl.fileformat = 1

;; **
;;; Boiler-plate code --- importing necessary things.
;; **

;; @@
(ns deli
  (:require [clojure.pprint :refer [pprint]]
            [gorilla-plot.core :as plot]
            [anglican.core :refer [doquery]]
            [anglican.emit :refer [query defquery conditional fm defm]]
            [anglican.inference :refer [equalize collect-by log-marginal]]
            [anglican.state :refer [get-predicts]]
            [anglican.stat :refer [mean std]])
  (:use [anglican.runtime]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## The Deli Dilemma
;;; 
;;; A customer wearing round sunglasses came at 1:13pm, and grabbed a sandwitch and a coffee. Later on the same day, a customer wearing round sunglasses came at 6:09pm and ordered a dinner. Was it the same customer?
;;; 
;;; What we know:
;;; 
;;; * There is an adjacent office quarter, and it takes between 5 and 15 minutes from an office to the deli, varying for different buildings. 
;;; * Depending on traffic lights, the time varies by about 2 minutes.
;;; * The lunch break is at 1:00pm, and the workday ends at 6:00pm.
;;; * The waiter's odds that this is the same customer are 2 to 1.
;;; 
;;; Let's formalize this knowledge (times are in minutes):
;; **

;; @@
(def p-same "prior probability that this is the same customer" (/ 2. 3.))
(def mean-time-to-arrive "average time to arrive from the office quarter" 10)
(def sd-time-to-arrive "standard deviation of arrival time" 3.)
(def time-sd "time deviation" 1)
(def lunch-delay "time between lunch break and lunch order" 13)
(def dinner-delay "time between end of day and dinner order" 9)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/dinner-delay</span>","value":"#'deli/dinner-delay"}
;; <=

;; @@
(defquery deli
  (let [time-to-arrive-prior (normal mean-time-to-arrive sd-time-to-arrive)
         same-customer (sample (flip p-same))]
    (predict :same-customer same-customer)
    (if same-customer
      ;; One customer
      (let [time-to-arrive (sample time-to-arrive-prior)]
        (observe (normal time-to-arrive time-sd) lunch-delay)
        (observe (normal time-to-arrive time-sd) dinner-delay)
        (predict :time-to-arrive time-to-arrive))
      ;; Two customers
      (let [time-to-arrive-1 (sample time-to-arrive-prior)
            time-to-arrive-2 (sample time-to-arrive-prior)]
        (observe (normal time-to-arrive-1 time-sd) lunch-delay)
        (observe (normal time-to-arrive-2 time-sd) dinner-delay)
        (predict :times-to-arrive [time-to-arrive-1 time-to-arrive-2])))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/deli</span>","value":"#'deli/deli"}
;; <=

;; **
;;; Now, we lazily perform the inference.
;; **

;; @@
(def samples (doquery :bbvb deli nil))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/samples</span>","value":"#'deli/samples"}
;; <=

;; **
;;; And retrieve predicts from the lazy sequence.
;; **

;; @@
(def N 1000)
(def predicts (map get-predicts (take N (drop N samples))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/predicts</span>","value":"#'deli/predicts"}
;; <=

;; **
;;; Let's compute the probability that this is the same customer, and arrival times for each case:
;; **

;; @@
(def p-same+ (/ (count (filter :same-customer predicts)) (double N)))
                            

;; single customer                      
(def time-to-arrive+ (map :time-to-arrive (filter :same-customer predicts)))
(def mean-to-arrive+ (mean time-to-arrive+))
(def sd-to-arrive+ (std time-to-arrive+))

;; two customers
(def times-to-arrive+ (map :times-to-arrive 
                           (filter (complement :same-customer) predicts)))
(def mean-1-to-arrive+ (mean (map first times-to-arrive+)))
(def sd-1-to-arrive+ (std (map first times-to-arrive+)))
(def mean-2-to-arrive+ (mean (map second times-to-arrive+)))
(def sd-2-to-arrive+ (std (map second times-to-arrive+)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-2-to-arrive+</span>","value":"#'deli/sd-2-to-arrive+"}
;; <=

;; @@
(plot/histogram (map #(if (:same-customer %) 1 2) predicts)
                :bins 4 :x-title (format "number of o customers, p-same=%6g" p-same+))
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"titleOffset":30,"title":"number of o customers, p-same=0.176000","scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"2e24f1d8-f088-4ab4-abf1-826efa2f05ce","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"2e24f1d8-f088-4ab4-abf1-826efa2f05ce","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"2e24f1d8-f088-4ab4-abf1-826efa2f05ce"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"2e24f1d8-f088-4ab4-abf1-826efa2f05ce","values":[{"x":1.0,"y":0},{"x":1.25,"y":176.0},{"x":1.5,"y":0.0},{"x":1.75,"y":0.0},{"x":2.0,"y":0.0},{"x":2.25,"y":824.0},{"x":2.5,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:titleOffset 30, :title \"number of o customers, p-same=0.176000\", :scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"2e24f1d8-f088-4ab4-abf1-826efa2f05ce\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"2e24f1d8-f088-4ab4-abf1-826efa2f05ce\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"2e24f1d8-f088-4ab4-abf1-826efa2f05ce\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"2e24f1d8-f088-4ab4-abf1-826efa2f05ce\", :values ({:x 1.0, :y 0} {:x 1.25, :y 176.0} {:x 1.5, :y 0.0} {:x 1.75, :y 0.0} {:x 2.0, :y 0.0} {:x 2.25, :y 824.0} {:x 2.5, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"}
;; <=

;; **
;;; If there is a single customer, there is one arrival time, let's see how it is distributed:
;; **

;; @@
(plot/histogram  time-to-arrive+
                 :x-title (format "arrival time: mean=%6g sd=%6g" 
                                  mean-to-arrive+
                                  sd-to-arrive+))

;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"titleOffset":30,"title":"arrival time: mean=11.0250 sd=0.708710","scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"ae09ac06-9fe9-4c05-9393-a9d85023b915","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"ae09ac06-9fe9-4c05-9393-a9d85023b915","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"ae09ac06-9fe9-4c05-9393-a9d85023b915"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"ae09ac06-9fe9-4c05-9393-a9d85023b915","values":[{"x":9.049514480876672,"y":0},{"x":9.440569821350566,"y":3.0},{"x":9.83162516182446,"y":8.0},{"x":10.222680502298354,"y":12.0},{"x":10.613735842772249,"y":22.0},{"x":11.004791183246143,"y":36.0},{"x":11.395846523720037,"y":40.0},{"x":11.786901864193931,"y":27.0},{"x":12.177957204667825,"y":20.0},{"x":12.56901254514172,"y":8.0},{"x":12.960067885615613,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:titleOffset 30, :title \"arrival time: mean=11.0250 sd=0.708710\", :scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"ae09ac06-9fe9-4c05-9393-a9d85023b915\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"ae09ac06-9fe9-4c05-9393-a9d85023b915\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"ae09ac06-9fe9-4c05-9393-a9d85023b915\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"ae09ac06-9fe9-4c05-9393-a9d85023b915\", :values ({:x 9.049514480876672, :y 0} {:x 9.440569821350566, :y 3.0} {:x 9.83162516182446, :y 8.0} {:x 10.222680502298354, :y 12.0} {:x 10.613735842772249, :y 22.0} {:x 11.004791183246143, :y 36.0} {:x 11.395846523720037, :y 40.0} {:x 11.786901864193931, :y 27.0} {:x 12.177957204667825, :y 20.0} {:x 12.56901254514172, :y 8.0} {:x 12.960067885615613, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"}
;; <=

;; **
;;; For two customers there are two different time distributions, let's compare them.
;; **

;; @@
(plot/compose 
  (plot/histogram (map first times-to-arrive+) 
                  :x-title (format 
                             "arrival times: mean1=%6g, sd1=%6g; mean2=%6g, sd2=%6g" 
                             mean-1-to-arrive+ sd-1-to-arrive+
                             mean-2-to-arrive+ sd-2-to-arrive+)
                  :plot-range [[6 16] :all])
  (plot/histogram (map second times-to-arrive+)))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[6,16]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"04ca449a-6979-49e3-9fd4-05704d0c6f0f","field":"data.y"}}],"axes":[{"titleOffset":30,"title":"arrival times: mean1=12.6279, sd1=0.978435; mean2=9.14033, sd2=0.943749","scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"04ca449a-6979-49e3-9fd4-05704d0c6f0f","values":[{"x":6.0,"y":0},{"x":6.909090909090909,"y":0.0},{"x":7.818181818181818,"y":0.0},{"x":8.727272727272728,"y":0.0},{"x":9.636363636363638,"y":2.0},{"x":10.545454545454549,"y":9.0},{"x":11.454545454545459,"y":86.0},{"x":12.363636363636369,"y":229.0},{"x":13.272727272727279,"y":289.0},{"x":14.181818181818189,"y":162.0},{"x":15.090909090909099,"y":44.0},{"x":16.000000000000007,"y":3.0},{"x":16.909090909090917,"y":0}]},{"name":"6a4662b8-7077-407d-922c-463a83fc7005","values":[{"x":5.87910439550883,"y":0},{"x":6.430882910962219,"y":2.0},{"x":6.982661426415608,"y":6.0},{"x":7.534439941868997,"y":27.0},{"x":8.086218457322385,"y":81.0},{"x":8.637996972775774,"y":123.0},{"x":9.189775488229163,"y":186.0},{"x":9.741554003682552,"y":181.0},{"x":10.29333251913594,"y":124.0},{"x":10.84511103458933,"y":62.0},{"x":11.396889550042719,"y":28.0},{"x":11.948668065496108,"y":4.0},{"x":12.500446580949497,"y":0}]}],"marks":[{"type":"line","from":{"data":"04ca449a-6979-49e3-9fd4-05704d0c6f0f"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"6a4662b8-7077-407d-922c-463a83fc7005"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [6 16]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"04ca449a-6979-49e3-9fd4-05704d0c6f0f\", :field \"data.y\"}}], :axes [{:titleOffset 30, :title \"arrival times: mean1=12.6279, sd1=0.978435; mean2=9.14033, sd2=0.943749\", :scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"04ca449a-6979-49e3-9fd4-05704d0c6f0f\", :values ({:x 6.0, :y 0} {:x 6.909090909090909, :y 0.0} {:x 7.818181818181818, :y 0.0} {:x 8.727272727272728, :y 0.0} {:x 9.636363636363638, :y 2.0} {:x 10.545454545454549, :y 9.0} {:x 11.454545454545459, :y 86.0} {:x 12.363636363636369, :y 229.0} {:x 13.272727272727279, :y 289.0} {:x 14.181818181818189, :y 162.0} {:x 15.090909090909099, :y 44.0} {:x 16.000000000000007, :y 3.0} {:x 16.909090909090917, :y 0})} {:name \"6a4662b8-7077-407d-922c-463a83fc7005\", :values ({:x 5.87910439550883, :y 0} {:x 6.430882910962219, :y 2.0} {:x 6.982661426415608, :y 6.0} {:x 7.534439941868997, :y 27.0} {:x 8.086218457322385, :y 81.0} {:x 8.637996972775774, :y 123.0} {:x 9.189775488229163, :y 186.0} {:x 9.741554003682552, :y 181.0} {:x 10.29333251913594, :y 124.0} {:x 10.84511103458933, :y 62.0} {:x 11.396889550042719, :y 28.0} {:x 11.948668065496108, :y 4.0} {:x 12.500446580949497, :y 0})}), :marks ({:type \"line\", :from {:data \"04ca449a-6979-49e3-9fd4-05704d0c6f0f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"6a4662b8-7077-407d-922c-463a83fc7005\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; **
;;; What if we had an algorithm that constructs the posterior? Let's rewrite the `deli` query with posterior distributions and without observations.
;; **

;; @@
(defquery deli+
  (let [same-customer (sample (flip p-same+))]
    (predict :same-customer same-customer)
    (if same-customer
      ;; One customer
      (let [time-to-arrive (sample (normal mean-to-arrive+ sd-to-arrive+))]
         (predict :time-to-arrive time-to-arrive))
      ;; Two customers
      (let [time-to-arrive-1 (sample (normal mean-1-to-arrive+ sd-1-to-arrive+))
            time-to-arrive-2 (sample (normal mean-2-to-arrive+ sd-2-to-arrive+))]
        (predict :times-to-arrive [time-to-arrive-1 time-to-arrive-2])))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/deli+</span>","value":"#'deli/deli+"}
;; <=

;; **
;;; This is what **Variational Inference** algorithm does **AUTOMATICALLY**.
;; **

;; @@
(clojure.pprint/pprint (anglican.bbvb/get-variational (nth samples N)))
;; @@
;; ->
;;; {S25104
;;;  {(0 anglican.runtime.flip-distribution)
;;;   {:p 0.14524873062891994,
;;;    :dist #&lt;Uniform cern.jet.random.Uniform(0.0,1.0)&gt;}},
;;;  S25094
;;;  {(0 anglican.runtime.normal-distribution)
;;;   {:mean 10.952360446876533,
;;;    :sd 0.7015973219750063,
;;;    :dist__22971__auto__
;;;    #&lt;Normal cern.jet.random.Normal(10.952360446876533,0.7015973219750063)&gt;}},
;;;  S25102
;;;  {(0 anglican.runtime.normal-distribution)
;;;   {:mean 12.69487644034261,
;;;    :sd 0.9617303591078367,
;;;    :dist__22971__auto__
;;;    #&lt;Normal cern.jet.random.Normal(12.69487644034261,0.9617303591078367)&gt;}},
;;;  S25100
;;;  {(0 anglican.runtime.normal-distribution)
;;;   {:mean 9.102111161519082,
;;;    :sd 0.9486047692907004,
;;;    :dist__22971__auto__
;;;    #&lt;Normal cern.jet.random.Normal(9.102111161519082,0.9486047692907004)&gt;}}}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=
