;; gorilla-repl.fileformat = 1

;; **
;;; Boiler-plate code --- importing necessary things.
;; **

;; @@
(use 'nstools.ns)
(ns+ deli
  (:like anglican-user.worksheet)
  (:require [anglican.stat :refer [mean std] ]))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[nil,nil]"}
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
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/p-same</span>","value":"#'deli/p-same"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-time-to-arrive</span>","value":"#'deli/mean-time-to-arrive"}],"value":"[#'deli/p-same,#'deli/mean-time-to-arrive]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-time-to-arrive</span>","value":"#'deli/sd-time-to-arrive"}],"value":"[[#'deli/p-same,#'deli/mean-time-to-arrive],#'deli/sd-time-to-arrive]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/time-sd</span>","value":"#'deli/time-sd"}],"value":"[[[#'deli/p-same,#'deli/mean-time-to-arrive],#'deli/sd-time-to-arrive],#'deli/time-sd]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/lunch-delay</span>","value":"#'deli/lunch-delay"}],"value":"[[[[#'deli/p-same,#'deli/mean-time-to-arrive],#'deli/sd-time-to-arrive],#'deli/time-sd],#'deli/lunch-delay]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/dinner-delay</span>","value":"#'deli/dinner-delay"}],"value":"[[[[[#'deli/p-same,#'deli/mean-time-to-arrive],#'deli/sd-time-to-arrive],#'deli/time-sd],#'deli/lunch-delay],#'deli/dinner-delay]"}
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
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/N</span>","value":"#'deli/N"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/predicts</span>","value":"#'deli/predicts"}],"value":"[#'deli/N,#'deli/predicts]"}
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
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/p-same+</span>","value":"#'deli/p-same+"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/time-to-arrive+</span>","value":"#'deli/time-to-arrive+"}],"value":"[#'deli/p-same+,#'deli/time-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-to-arrive+</span>","value":"#'deli/mean-to-arrive+"}],"value":"[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-to-arrive+</span>","value":"#'deli/sd-to-arrive+"}],"value":"[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/times-to-arrive+</span>","value":"#'deli/times-to-arrive+"}],"value":"[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-1-to-arrive+</span>","value":"#'deli/mean-1-to-arrive+"}],"value":"[[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+],#'deli/mean-1-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-1-to-arrive+</span>","value":"#'deli/sd-1-to-arrive+"}],"value":"[[[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+],#'deli/mean-1-to-arrive+],#'deli/sd-1-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-2-to-arrive+</span>","value":"#'deli/mean-2-to-arrive+"}],"value":"[[[[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+],#'deli/mean-1-to-arrive+],#'deli/sd-1-to-arrive+],#'deli/mean-2-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-2-to-arrive+</span>","value":"#'deli/sd-2-to-arrive+"}],"value":"[[[[[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+],#'deli/mean-1-to-arrive+],#'deli/sd-1-to-arrive+],#'deli/mean-2-to-arrive+],#'deli/sd-2-to-arrive+]"}
;; <=

;; @@
(plot/histogram (map #(if (:same-customer %) 1 2) predicts)
                :bins 4 :x-title (format "number of o customers, p-same=%6g" p-same+))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"2619a25a-757b-4fe6-946f-ba1ca15939cd","values":[{"x":1.0,"y":0},{"x":1.25,"y":124.0},{"x":1.5,"y":0.0},{"x":1.75,"y":0.0},{"x":2.0,"y":0.0},{"x":2.25,"y":876.0},{"x":2.5,"y":0}]}],"marks":[{"type":"line","from":{"data":"2619a25a-757b-4fe6-946f-ba1ca15939cd"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"2619a25a-757b-4fe6-946f-ba1ca15939cd","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"2619a25a-757b-4fe6-946f-ba1ca15939cd","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"number of o customers, p-same=0.124000","titleOffset":30},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"2619a25a-757b-4fe6-946f-ba1ca15939cd\", :values ({:x 1.0, :y 0} {:x 1.25, :y 124.0} {:x 1.5, :y 0.0} {:x 1.75, :y 0.0} {:x 2.0, :y 0.0} {:x 2.25, :y 876.0} {:x 2.5, :y 0})}], :marks [{:type \"line\", :from {:data \"2619a25a-757b-4fe6-946f-ba1ca15939cd\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"2619a25a-757b-4fe6-946f-ba1ca15939cd\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"2619a25a-757b-4fe6-946f-ba1ca15939cd\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"number of o customers, p-same=0.124000\", :titleOffset 30} {:type \"y\", :scale \"y\"}]}}"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"813e9d41-4a48-4272-850f-d8115c6ce221","values":[{"x":8.200074402691635,"y":0},{"x":8.741754790906818,"y":1.0},{"x":9.283435179121998,"y":0.0},{"x":9.825115567337178,"y":2.0},{"x":10.366795955552359,"y":14.0},{"x":10.90847634376754,"y":36.0},{"x":11.45015673198272,"y":36.0},{"x":11.9918371201979,"y":20.0},{"x":12.53351750841308,"y":14.0},{"x":13.07519789662826,"y":1.0},{"x":13.616878284843441,"y":0}]}],"marks":[{"type":"line","from":{"data":"813e9d41-4a48-4272-850f-d8115c6ce221"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"813e9d41-4a48-4272-850f-d8115c6ce221","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"813e9d41-4a48-4272-850f-d8115c6ce221","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"arrival time: mean=11.0640 sd=0.701887","titleOffset":30},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"813e9d41-4a48-4272-850f-d8115c6ce221\", :values ({:x 8.200074402691635, :y 0} {:x 8.741754790906818, :y 1.0} {:x 9.283435179121998, :y 0.0} {:x 9.825115567337178, :y 2.0} {:x 10.366795955552359, :y 14.0} {:x 10.90847634376754, :y 36.0} {:x 11.45015673198272, :y 36.0} {:x 11.9918371201979, :y 20.0} {:x 12.53351750841308, :y 14.0} {:x 13.07519789662826, :y 1.0} {:x 13.616878284843441, :y 0})}], :marks [{:type \"line\", :from {:data \"813e9d41-4a48-4272-850f-d8115c6ce221\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"813e9d41-4a48-4272-850f-d8115c6ce221\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"813e9d41-4a48-4272-850f-d8115c6ce221\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"arrival time: mean=11.0640 sd=0.701887\", :titleOffset 30} {:type \"y\", :scale \"y\"}]}}"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[6,16]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"60ee33fd-ac88-4a3a-8949-8e4951642e52","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"arrival times: mean1=12.5304, sd1=0.927482; mean2=9.11105, sd2=0.940239","titleOffset":30},{"type":"y","scale":"y"}],"data":[{"name":"60ee33fd-ac88-4a3a-8949-8e4951642e52","values":[{"x":6.0,"y":0},{"x":6.909090909090909,"y":0.0},{"x":7.818181818181818,"y":0.0},{"x":8.727272727272728,"y":0.0},{"x":9.636363636363638,"y":2.0},{"x":10.545454545454549,"y":11.0},{"x":11.454545454545459,"y":94.0},{"x":12.363636363636369,"y":272.0},{"x":13.272727272727279,"y":316.0},{"x":14.181818181818189,"y":146.0},{"x":15.090909090909099,"y":33.0},{"x":16.000000000000007,"y":2.0},{"x":16.909090909090917,"y":0}]},{"name":"fabdd059-27b5-4f01-9d6c-b6b1630dac46","values":[{"x":6.286449973678611,"y":0},{"x":6.824534084508058,"y":10.0},{"x":7.362618195337506,"y":26.0},{"x":7.900702306166953,"y":49.0},{"x":8.4387864169964,"y":114.0},{"x":8.976870527825849,"y":174.0},{"x":9.514954638655297,"y":213.0},{"x":10.053038749484745,"y":150.0},{"x":10.591122860314194,"y":92.0},{"x":11.129206971143642,"y":35.0},{"x":11.66729108197309,"y":10.0},{"x":12.205375192802538,"y":3.0},{"x":12.743459303631987,"y":0}]}],"marks":[{"type":"line","from":{"data":"60ee33fd-ac88-4a3a-8949-8e4951642e52"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"fabdd059-27b5-4f01-9d6c-b6b1630dac46"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [6 16]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"60ee33fd-ac88-4a3a-8949-8e4951642e52\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"arrival times: mean1=12.5304, sd1=0.927482; mean2=9.11105, sd2=0.940239\", :titleOffset 30} {:type \"y\", :scale \"y\"}], :data ({:name \"60ee33fd-ac88-4a3a-8949-8e4951642e52\", :values ({:x 6.0, :y 0} {:x 6.909090909090909, :y 0.0} {:x 7.818181818181818, :y 0.0} {:x 8.727272727272728, :y 0.0} {:x 9.636363636363638, :y 2.0} {:x 10.545454545454549, :y 11.0} {:x 11.454545454545459, :y 94.0} {:x 12.363636363636369, :y 272.0} {:x 13.272727272727279, :y 316.0} {:x 14.181818181818189, :y 146.0} {:x 15.090909090909099, :y 33.0} {:x 16.000000000000007, :y 2.0} {:x 16.909090909090917, :y 0})} {:name \"fabdd059-27b5-4f01-9d6c-b6b1630dac46\", :values ({:x 6.286449973678611, :y 0} {:x 6.824534084508058, :y 10.0} {:x 7.362618195337506, :y 26.0} {:x 7.900702306166953, :y 49.0} {:x 8.4387864169964, :y 114.0} {:x 8.976870527825849, :y 174.0} {:x 9.514954638655297, :y 213.0} {:x 10.053038749484745, :y 150.0} {:x 10.591122860314194, :y 92.0} {:x 11.129206971143642, :y 35.0} {:x 11.66729108197309, :y 10.0} {:x 12.205375192802538, :y 3.0} {:x 12.743459303631987, :y 0})}), :marks ({:type \"line\", :from {:data \"60ee33fd-ac88-4a3a-8949-8e4951642e52\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"fabdd059-27b5-4f01-9d6c-b6b1630dac46\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
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
;;; {S28219
;;;  {(0 anglican.runtime.flip-distribution)
;;;   {:p 0.11671977016875924,
;;;    :dist
;;;    {:min 0.0,
;;;     :max 1.0,
;;;     :dist25354
;;;     #object[org.apache.commons.math3.distribution.UniformRealDistribution 0x577c8884 &quot;org.apache.commons.math3.distribution.UniformRealDistribution@577c8884&quot;]}}},
;;;  S28209
;;;  {(0 anglican.runtime.normal-distribution)
;;;   {:mean 10.99753360180663,
;;;    :sd 0.7290976433082352,
;;;    :dist25299
;;;    #object[org.apache.commons.math3.distribution.NormalDistribution 0x6301b0b8 &quot;org.apache.commons.math3.distribution.NormalDistribution@6301b0b8&quot;]}},
;;;  S28217
;;;  {(0 anglican.runtime.normal-distribution)
;;;   {:mean 12.668050292254202,
;;;    :sd 0.9446695174790353,
;;;    :dist25299
;;;    #object[org.apache.commons.math3.distribution.NormalDistribution 0x3765b5c3 &quot;org.apache.commons.math3.distribution.NormalDistribution@3765b5c3&quot;]}},
;;;  S28215
;;;  {(0 anglican.runtime.normal-distribution)
;;;   {:mean 9.104132559955836,
;;;    :sd 0.9479290526821788,
;;;    :dist25299
;;;    #object[org.apache.commons.math3.distribution.NormalDistribution 0x147980e &quot;org.apache.commons.math3.distribution.NormalDistribution@147980e&quot;]}}}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=
