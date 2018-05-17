;; gorilla-repl.fileformat = 1

;; **
;;; Boiler-plate code -- importing necessary things.
;; **

;; @@
(ns deli
  (:require [clojure.pprint :refer [pprint]]
            [gorilla-plot.core :as plot]
            [anglican.core :refer [doquery]]
            [anglican.emit :refer [query defquery conditional]]
            [anglican.runtime :refer [mean std]])
  (:use [anglican.runtime]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## The Deli Dilemma
;;; 
;;; In this worksheet, we discuss Anglican's black-box variational inference algorithm (BBVI), in the context of the following problem.
;;; 
;;; A customer comes to a deli wearing round sunglasses came at 1:13pm, and grabbed a sandwich and a coffee. Later on the same day, a customer wearing round sunglasses came at 6:09pm and ordered a dinner. Was it the same customer?
;;; 
;;; What we know:
;;; 
;;; * There is an adjacent office quarter, and it takes between 5 and 15 minutes from an office to the deli, varying for different buildings. 
;;; * Depending on traffic lights, the time varies by about 2 minutes.
;;; * The lunch break is at 1:00pm, and the workday ends at 6:00pm.
;;; * The waiter's subjective odds that this is the same customer are 2 to 1.
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
         same-customer? (sample (flip p-same))]
    (if same-customer?
      
      ;; One customer
      (let [time-to-arrive (sample time-to-arrive-prior)]
        (observe (normal time-to-arrive time-sd) lunch-delay)
        (observe (normal time-to-arrive time-sd) dinner-delay)
        {:same-customer? same-customer? :time-to-arrive time-to-arrive})
      
      ;; Two customers
      (let [time-to-arrive-1 (sample time-to-arrive-prior)
            time-to-arrive-2 (sample time-to-arrive-prior)]
        (observe (normal time-to-arrive-1 time-sd) lunch-delay)
        (observe (normal time-to-arrive-2 time-sd) dinner-delay)
        {:same-customer? same-customer?
         :times-to-arrive [time-to-arrive-1 time-to-arrive-2]}))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/deli</span>","value":"#'deli/deli"}
;; <=

;; **
;;; Now, we lazily perform inference using black-box variational inference (BBVI)...
;; **

;; @@
(def samples (doquery :bbvb deli nil))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/samples</span>","value":"#'deli/samples"}
;; <=

;; **
;;; and retrieve our results from the lazy sequence.
;; **

;; @@
(def N 1000)
(def results (map :result (take N (drop N samples))))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/N</span>","value":"#'deli/N"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/results</span>","value":"#'deli/results"}],"value":"[#'deli/N,#'deli/results]"}
;; <=

;; **
;;; Let's compute the probability that this is the same customer both times, and arrival times in each case:
;; **

;; @@
(def p-same+ (/ (count (filter :same-customer? results)) (double N)))
                            

;; single customer                      
(def time-to-arrive+ (map :time-to-arrive (filter :same-customer? results)))
(def mean-to-arrive+ (mean time-to-arrive+))
(def sd-to-arrive+ (std time-to-arrive+))

;; two customers
(def times-to-arrive+ (map :times-to-arrive 
                           (filter (complement :same-customer?) results)))
(def mean-1-to-arrive+ (mean (map first times-to-arrive+)))
(def sd-1-to-arrive+ (std (map first times-to-arrive+)))
(def mean-2-to-arrive+ (mean (map second times-to-arrive+)))
(def sd-2-to-arrive+ (std (map second times-to-arrive+)))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/p-same+</span>","value":"#'deli/p-same+"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/time-to-arrive+</span>","value":"#'deli/time-to-arrive+"}],"value":"[#'deli/p-same+,#'deli/time-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-to-arrive+</span>","value":"#'deli/mean-to-arrive+"}],"value":"[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-to-arrive+</span>","value":"#'deli/sd-to-arrive+"}],"value":"[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/times-to-arrive+</span>","value":"#'deli/times-to-arrive+"}],"value":"[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-1-to-arrive+</span>","value":"#'deli/mean-1-to-arrive+"}],"value":"[[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+],#'deli/mean-1-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-1-to-arrive+</span>","value":"#'deli/sd-1-to-arrive+"}],"value":"[[[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+],#'deli/mean-1-to-arrive+],#'deli/sd-1-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-2-to-arrive+</span>","value":"#'deli/mean-2-to-arrive+"}],"value":"[[[[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+],#'deli/mean-1-to-arrive+],#'deli/sd-1-to-arrive+],#'deli/mean-2-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-2-to-arrive+</span>","value":"#'deli/sd-2-to-arrive+"}],"value":"[[[[[[[[#'deli/p-same+,#'deli/time-to-arrive+],#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#'deli/times-to-arrive+],#'deli/mean-1-to-arrive+],#'deli/sd-1-to-arrive+],#'deli/mean-2-to-arrive+],#'deli/sd-2-to-arrive+]"}
;; <=

;; @@
(plot/histogram (map #(if (:same-customer? %) 1 2) results)
                :bins 4 :x-title (format "number of customers; p-same=%6g" p-same+))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"d3d5c213-d014-424f-b3fa-6d9273db7cbe","values":[{"x":1.0,"y":0},{"x":1.25,"y":178.0},{"x":1.5,"y":0.0},{"x":1.75,"y":0.0},{"x":2.0,"y":0.0},{"x":2.25,"y":822.0},{"x":2.5,"y":0}]}],"marks":[{"type":"line","from":{"data":"d3d5c213-d014-424f-b3fa-6d9273db7cbe"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"d3d5c213-d014-424f-b3fa-6d9273db7cbe","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"d3d5c213-d014-424f-b3fa-6d9273db7cbe","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"number of customers; p-same=0.178000","titleOffset":30},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"d3d5c213-d014-424f-b3fa-6d9273db7cbe\", :values ({:x 1.0, :y 0} {:x 1.25, :y 178.0} {:x 1.5, :y 0.0} {:x 1.75, :y 0.0} {:x 2.0, :y 0.0} {:x 2.25, :y 822.0} {:x 2.5, :y 0})}], :marks [{:type \"line\", :from {:data \"d3d5c213-d014-424f-b3fa-6d9273db7cbe\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"d3d5c213-d014-424f-b3fa-6d9273db7cbe\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"d3d5c213-d014-424f-b3fa-6d9273db7cbe\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"number of customers; p-same=0.178000\", :titleOffset 30} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; If there is a single customer, then there's only one arrival time. Let's see how it is distributed:
;; **

;; @@
(plot/histogram  time-to-arrive+
                 :x-title (format "arrival time: mean=%6g sd=%6g" 
                                  mean-to-arrive+
                                  sd-to-arrive+))

;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"6550d018-d54c-498c-b703-8ef8e5469cf7","values":[{"x":8.283897681349826,"y":0},{"x":8.787086311624236,"y":1.0},{"x":9.290274941898646,"y":2.0},{"x":9.793463572173057,"y":14.0},{"x":10.296652202447467,"y":31.0},{"x":10.799840832721877,"y":35.0},{"x":11.303029462996287,"y":40.0},{"x":11.806218093270697,"y":33.0},{"x":12.309406723545107,"y":15.0},{"x":12.812595353819518,"y":7.0},{"x":13.315783984093928,"y":0}]}],"marks":[{"type":"line","from":{"data":"6550d018-d54c-498c-b703-8ef8e5469cf7"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"6550d018-d54c-498c-b703-8ef8e5469cf7","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"6550d018-d54c-498c-b703-8ef8e5469cf7","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"arrival time: mean=10.8434 sd=0.829047","titleOffset":30},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"6550d018-d54c-498c-b703-8ef8e5469cf7\", :values ({:x 8.283897681349826, :y 0} {:x 8.787086311624236, :y 1.0} {:x 9.290274941898646, :y 2.0} {:x 9.793463572173057, :y 14.0} {:x 10.296652202447467, :y 31.0} {:x 10.799840832721877, :y 35.0} {:x 11.303029462996287, :y 40.0} {:x 11.806218093270697, :y 33.0} {:x 12.309406723545107, :y 15.0} {:x 12.812595353819518, :y 7.0} {:x 13.315783984093928, :y 0})}], :marks [{:type \"line\", :from {:data \"6550d018-d54c-498c-b703-8ef8e5469cf7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"6550d018-d54c-498c-b703-8ef8e5469cf7\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"6550d018-d54c-498c-b703-8ef8e5469cf7\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"arrival time: mean=10.8434 sd=0.829047\", :titleOffset 30} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; For two customers, there are two different time distributions. Let's compare them:
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[6,16]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"c3f3f8c1-1344-4527-b058-7d3971925333","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"arrival times: mean1=12.7171, sd1=0.926334; mean2=9.14224, sd2=0.947756","titleOffset":30},{"type":"y","scale":"y"}],"data":[{"name":"c3f3f8c1-1344-4527-b058-7d3971925333","values":[{"x":6.0,"y":0},{"x":6.909090909090909,"y":0.0},{"x":7.818181818181818,"y":0.0},{"x":8.727272727272728,"y":0.0},{"x":9.636363636363638,"y":0.0},{"x":10.545454545454549,"y":12.0},{"x":11.454545454545459,"y":60.0},{"x":12.363636363636369,"y":217.0},{"x":13.272727272727279,"y":303.0},{"x":14.181818181818189,"y":186.0},{"x":15.090909090909099,"y":40.0},{"x":16.000000000000007,"y":4.0},{"x":16.909090909090917,"y":0}]},{"name":"944abc30-bb92-4a1a-99ba-b7900282f7a7","values":[{"x":6.07127406774788,"y":0},{"x":6.593526059330616,"y":4.0},{"x":7.1157780509133515,"y":14.0},{"x":7.638030042496087,"y":26.0},{"x":8.160282034078822,"y":80.0},{"x":8.682534025661557,"y":135.0},{"x":9.204786017244292,"y":169.0},{"x":9.727038008827027,"y":169.0},{"x":10.249290000409761,"y":126.0},{"x":10.771541991992496,"y":70.0},{"x":11.293793983575231,"y":21.0},{"x":11.816045975157966,"y":7.0},{"x":12.3382979667407,"y":1.0},{"x":12.860549958323436,"y":0}]}],"marks":[{"type":"line","from":{"data":"c3f3f8c1-1344-4527-b058-7d3971925333"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"944abc30-bb92-4a1a-99ba-b7900282f7a7"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [6 16]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"c3f3f8c1-1344-4527-b058-7d3971925333\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"arrival times: mean1=12.7171, sd1=0.926334; mean2=9.14224, sd2=0.947756\", :titleOffset 30} {:type \"y\", :scale \"y\"}], :data ({:name \"c3f3f8c1-1344-4527-b058-7d3971925333\", :values ({:x 6.0, :y 0} {:x 6.909090909090909, :y 0.0} {:x 7.818181818181818, :y 0.0} {:x 8.727272727272728, :y 0.0} {:x 9.636363636363638, :y 0.0} {:x 10.545454545454549, :y 12.0} {:x 11.454545454545459, :y 60.0} {:x 12.363636363636369, :y 217.0} {:x 13.272727272727279, :y 303.0} {:x 14.181818181818189, :y 186.0} {:x 15.090909090909099, :y 40.0} {:x 16.000000000000007, :y 4.0} {:x 16.909090909090917, :y 0})} {:name \"944abc30-bb92-4a1a-99ba-b7900282f7a7\", :values ({:x 6.07127406774788, :y 0} {:x 6.593526059330616, :y 4.0} {:x 7.1157780509133515, :y 14.0} {:x 7.638030042496087, :y 26.0} {:x 8.160282034078822, :y 80.0} {:x 8.682534025661557, :y 135.0} {:x 9.204786017244292, :y 169.0} {:x 9.727038008827027, :y 169.0} {:x 10.249290000409761, :y 126.0} {:x 10.771541991992496, :y 70.0} {:x 11.293793983575231, :y 21.0} {:x 11.816045975157966, :y 7.0} {:x 12.3382979667407, :y 1.0} {:x 12.860549958323436, :y 0})}), :marks ({:type \"line\", :from {:data \"c3f3f8c1-1344-4527-b058-7d3971925333\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"944abc30-bb92-4a1a-99ba-b7900282f7a7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; **
;;; What if we had an algorithm that constructs the posterior? Let's rewrite the `deli` query with posterior distributions and without observations.
;; **

;; @@
(defquery deli+
  (let [same-customer? (sample (flip p-same+))]
    (if same-customer?
      
      ;; One customer
      (let [time-to-arrive (sample (normal mean-to-arrive+ sd-to-arrive+))]
         {:same-customer? same-customer? :time-to-arrive time-to-arrive})
      
      ;; Two customers
      (let [time-to-arrive-1 (sample (normal mean-1-to-arrive+ sd-1-to-arrive+))
            time-to-arrive-2 (sample (normal mean-2-to-arrive+ sd-2-to-arrive+))]
        {:same-customer? same-customer?
         :times-to-arrive [time-to-arrive-1 time-to-arrive-2]}))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/deli+</span>","value":"#'deli/deli+"}
;; <=

;; **
;;; This is what the **Variational Inference** algorithm does **automatically**.
;; **

;; @@
(clojure.pprint/pprint (anglican.bbvb/get-variational (nth samples N)))
;; @@
;; ->
;;; {S29332
;;;  {(0 anglican.runtime.flip-distribution)
;;;   {:p 0.15269696539778807,
;;;    :dist
;;;    {:min 0.0,
;;;     :max 1.0,
;;;     :dist24987
;;;     #object[org.apache.commons.math3.distribution.UniformRealDistribution 0x57791391 &quot;org.apache.commons.math3.distribution.UniformRealDistribution@57791391&quot;]}}},
;;;  S29330
;;;  {(0 anglican.runtime.normal-distribution)
;;;   {:mean 12.699581824918642,
;;;    :sd 0.9486381976963099,
;;;    :dist24932
;;;    #object[org.apache.commons.math3.distribution.NormalDistribution 0x53a8341c &quot;org.apache.commons.math3.distribution.NormalDistribution@53a8341c&quot;]}},
;;;  S29328
;;;  {(0 anglican.runtime.normal-distribution)
;;;   {:mean 9.100005407191913,
;;;    :sd 0.9486820711201321,
;;;    :dist24932
;;;    #object[org.apache.commons.math3.distribution.NormalDistribution 0x5c679e5b &quot;org.apache.commons.math3.distribution.NormalDistribution@5c679e5b&quot;]}},
;;;  S29322
;;;  {(0 anglican.runtime.normal-distribution)
;;;   {:mean 10.971384908971686,
;;;    :sd 0.745040548096741,
;;;    :dist24932
;;;    #object[org.apache.commons.math3.distribution.NormalDistribution 0x77cecd13 &quot;org.apache.commons.math3.distribution.NormalDistribution@77cecd13&quot;]}}}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@

;; @@
