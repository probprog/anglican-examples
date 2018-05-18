;; gorilla-repl.fileformat = 1

;; **
;;; Boiler-plate code -- importing necessary things.
;; **

;; @@
(ns deli
  (:require [clojure.pprint :refer [pprint]]
            [gorilla-plot.core :as plot])
  (:use [anglican core emit runtime]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## The Deli Dilemma
;;; 
;;; In this worksheet, we introduce Anglican's black-box variational inference algorithm (BBVI), in the context of the following problem.
;;; 
;;; A customer wearing round sunglasses came to a deli at 1:13pm, and grabbed a sandwich and a coffee. Later on the same day, a customer wearing round sunglasses came at 6:09pm and ordered dinner. Was it the same customer?
;;; 
;;; Here's what we know:
;;; 
;;; * There is an adjacent office quarter, and it takes between 5 and 15 minutes from an office to the deli (depending on the office that the customer is coming from). 
;;; * Depending on traffic conditions, travel time varies by about 2 minutes.
;;; * Lunch break is at 1:00pm, and the workday ends at 6:00pm.
;;; * The waiter's subjective odds that this is the same customer are 2 to 1; he looked quite similar, but the waiter could not be sure.
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
;;; We lazily perform inference on this query using one of the most basic inference algorithms, sequential Monte Carlo (SMC). Clearly, this won't give us a *perfect* approximation of the posterior distribution, but it will give us valuable information about its rough shape.
;; **

;; @@
(def N 10000)
(def samples (take N (doquery :smc deli [] :number-of-particles N)))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/N</span>","value":"#'deli/N"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/samples</span>","value":"#'deli/samples"}],"value":"[#'deli/N,#'deli/samples]"}
;; <=

;; **
;;; Our estimated posterior probability that this is the same customer both times is simply the empirical fraction of our samples in which `same-customer?` is true. Below, we see that our rough approach has concluded that there is an approximately 90% chance that it was in fact two different customers.
;; **

;; @@
(def p-same+ (/ (count (filter :same-customer? results)) (double N)))
                            
(plot/histogram (map #(if (:same-customer? %) 1 2) results)
                :bins 10 :x-title (format "number of customers; p-same=%6g" p-same+))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/p-same+</span>","value":"#'deli/p-same+"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"9b82a504-5c60-4aef-a2e0-0b9e8e5f2fa0","values":[{"x":1,"y":0},{"x":1.1,"y":1024},{"x":1.2000000000000002,"y":0},{"x":1.3000000000000003,"y":0},{"x":1.4000000000000004,"y":0},{"x":1.5000000000000004,"y":0},{"x":1.6000000000000005,"y":0},{"x":1.7000000000000006,"y":0},{"x":1.8000000000000007,"y":0},{"x":1.9000000000000008,"y":0},{"x":2.000000000000001,"y":8976},{"x":2.100000000000001,"y":0}]}],"marks":[{"type":"line","from":{"data":"9b82a504-5c60-4aef-a2e0-0b9e8e5f2fa0"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"9b82a504-5c60-4aef-a2e0-0b9e8e5f2fa0","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"9b82a504-5c60-4aef-a2e0-0b9e8e5f2fa0","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"number of customers; p-same=0.102400","titleOffset":30},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"9b82a504-5c60-4aef-a2e0-0b9e8e5f2fa0\", :values ({:x 1.0, :y 0} {:x 1.1, :y 1024.0} {:x 1.2000000000000002, :y 0.0} {:x 1.3000000000000003, :y 0.0} {:x 1.4000000000000004, :y 0.0} {:x 1.5000000000000004, :y 0.0} {:x 1.6000000000000005, :y 0.0} {:x 1.7000000000000006, :y 0.0} {:x 1.8000000000000007, :y 0.0} {:x 1.9000000000000008, :y 0.0} {:x 2.000000000000001, :y 8976.0} {:x 2.100000000000001, :y 0})}], :marks [{:type \"line\", :from {:data \"9b82a504-5c60-4aef-a2e0-0b9e8e5f2fa0\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"9b82a504-5c60-4aef-a2e0-0b9e8e5f2fa0\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"9b82a504-5c60-4aef-a2e0-0b9e8e5f2fa0\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"number of customers; p-same=0.102400\", :titleOffset 30} {:type \"y\", :scale \"y\"}]}}"}],"value":"[#'deli/p-same+,#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"9b82a504-5c60-4aef-a2e0-0b9e8e5f2fa0\", :values ({:x 1.0, :y 0} {:x 1.1, :y 1024.0} {:x 1.2000000000000002, :y 0.0} {:x 1.3000000000000003, :y 0.0} {:x 1.4000000000000004, :y 0.0} {:x 1.5000000000000004, :y 0.0} {:x 1.6000000000000005, :y 0.0} {:x 1.7000000000000006, :y 0.0} {:x 1.8000000000000007, :y 0.0} {:x 1.9000000000000008, :y 0.0} {:x 2.000000000000001, :y 8976.0} {:x 2.100000000000001, :y 0})}], :marks [{:type \"line\", :from {:data \"9b82a504-5c60-4aef-a2e0-0b9e8e5f2fa0\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"9b82a504-5c60-4aef-a2e0-0b9e8e5f2fa0\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"9b82a504-5c60-4aef-a2e0-0b9e8e5f2fa0\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"number of customers; p-same=0.102400\", :titleOffset 30} {:type \"y\", :scale \"y\"}]}}]"}
;; <=

;; **
;;; If there is a single customer, then there's only one arrival time. So, in the subset of samples in which there was only one customer, let's see how the estimated arrival time is distributed:
;; **

;; @@
;; single customer                      
(def time-to-arrive+ (map :time-to-arrive (filter :same-customer? results)))
(def mean-to-arrive+ (mean time-to-arrive+))
(def sd-to-arrive+ (std time-to-arrive+))

(plot/histogram  time-to-arrive+
                 :x-title (format "arrival time: mean=%6g sd=%6g" 
                                  mean-to-arrive+
                                  sd-to-arrive+))

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/time-to-arrive+</span>","value":"#'deli/time-to-arrive+"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-to-arrive+</span>","value":"#'deli/mean-to-arrive+"}],"value":"[#'deli/time-to-arrive+,#'deli/mean-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-to-arrive+</span>","value":"#'deli/sd-to-arrive+"}],"value":"[[#'deli/time-to-arrive+,#'deli/mean-to-arrive+],#'deli/sd-to-arrive+]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"925194ea-d11e-4167-b049-16909ca16ec2","values":[{"x":9.030155611442773,"y":0},{"x":9.44161650900857,"y":32},{"x":9.853077406574368,"y":24},{"x":10.264538304140165,"y":121},{"x":10.675999201705963,"y":168},{"x":11.08746009927176,"y":240},{"x":11.498920996837558,"y":212},{"x":11.910381894403356,"y":137},{"x":12.321842791969154,"y":58},{"x":12.733303689534951,"y":28},{"x":13.144764587100749,"y":2},{"x":13.556225484666546,"y":2},{"x":13.967686382232344,"y":0}]}],"marks":[{"type":"line","from":{"data":"925194ea-d11e-4167-b049-16909ca16ec2"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"925194ea-d11e-4167-b049-16909ca16ec2","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"925194ea-d11e-4167-b049-16909ca16ec2","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"arrival time: mean=10.9607 sd=0.711009","titleOffset":30},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"925194ea-d11e-4167-b049-16909ca16ec2\", :values ({:x 9.030155611442773, :y 0} {:x 9.44161650900857, :y 32.0} {:x 9.853077406574368, :y 24.0} {:x 10.264538304140165, :y 121.0} {:x 10.675999201705963, :y 168.0} {:x 11.08746009927176, :y 240.0} {:x 11.498920996837558, :y 212.0} {:x 11.910381894403356, :y 137.0} {:x 12.321842791969154, :y 58.0} {:x 12.733303689534951, :y 28.0} {:x 13.144764587100749, :y 2.0} {:x 13.556225484666546, :y 2.0} {:x 13.967686382232344, :y 0})}], :marks [{:type \"line\", :from {:data \"925194ea-d11e-4167-b049-16909ca16ec2\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"925194ea-d11e-4167-b049-16909ca16ec2\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"925194ea-d11e-4167-b049-16909ca16ec2\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"arrival time: mean=10.9607 sd=0.711009\", :titleOffset 30} {:type \"y\", :scale \"y\"}]}}"}],"value":"[[[#'deli/time-to-arrive+,#'deli/mean-to-arrive+],#'deli/sd-to-arrive+],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"925194ea-d11e-4167-b049-16909ca16ec2\", :values ({:x 9.030155611442773, :y 0} {:x 9.44161650900857, :y 32.0} {:x 9.853077406574368, :y 24.0} {:x 10.264538304140165, :y 121.0} {:x 10.675999201705963, :y 168.0} {:x 11.08746009927176, :y 240.0} {:x 11.498920996837558, :y 212.0} {:x 11.910381894403356, :y 137.0} {:x 12.321842791969154, :y 58.0} {:x 12.733303689534951, :y 28.0} {:x 13.144764587100749, :y 2.0} {:x 13.556225484666546, :y 2.0} {:x 13.967686382232344, :y 0})}], :marks [{:type \"line\", :from {:data \"925194ea-d11e-4167-b049-16909ca16ec2\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"925194ea-d11e-4167-b049-16909ca16ec2\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"925194ea-d11e-4167-b049-16909ca16ec2\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"arrival time: mean=10.9607 sd=0.711009\", :titleOffset 30} {:type \"y\", :scale \"y\"}]}}]"}
;; <=

;; **
;;; If there are two different customers, then there will be two different distributions of arrival times. Let's compare them:
;; **

;; @@
;; two customers
(def times-to-arrive+ (map :times-to-arrive 
                           (filter (complement :same-customer?) results)))
(def mean-1-to-arrive+ (mean (map first times-to-arrive+)))
(def sd-1-to-arrive+ (std (map first times-to-arrive+)))
(def mean-2-to-arrive+ (mean (map second times-to-arrive+)))
(def sd-2-to-arrive+ (std (map second times-to-arrive+)))

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
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/times-to-arrive+</span>","value":"#'deli/times-to-arrive+"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-1-to-arrive+</span>","value":"#'deli/mean-1-to-arrive+"}],"value":"[#'deli/times-to-arrive+,#'deli/mean-1-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-1-to-arrive+</span>","value":"#'deli/sd-1-to-arrive+"}],"value":"[[#'deli/times-to-arrive+,#'deli/mean-1-to-arrive+],#'deli/sd-1-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/mean-2-to-arrive+</span>","value":"#'deli/mean-2-to-arrive+"}],"value":"[[[#'deli/times-to-arrive+,#'deli/mean-1-to-arrive+],#'deli/sd-1-to-arrive+],#'deli/mean-2-to-arrive+]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sd-2-to-arrive+</span>","value":"#'deli/sd-2-to-arrive+"}],"value":"[[[[#'deli/times-to-arrive+,#'deli/mean-1-to-arrive+],#'deli/sd-1-to-arrive+],#'deli/mean-2-to-arrive+],#'deli/sd-2-to-arrive+]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[6,16]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"f25c38dd-3613-43f0-957b-9acc3d69004f","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"arrival times: mean1=12.6736, sd1=0.904110; mean2=9.04398, sd2=0.885576","titleOffset":30},{"type":"y","scale":"y"}],"data":[{"name":"f25c38dd-3613-43f0-957b-9acc3d69004f","values":[{"x":6,"y":0},{"x":6.666666666666667,"y":0},{"x":7.333333333333334,"y":0},{"x":8,"y":0},{"x":8.666666666666666,"y":0},{"x":9.333333333333332,"y":0},{"x":9.999999999999998,"y":25},{"x":10.666666666666664,"y":111},{"x":11.33333333333333,"y":514},{"x":11.999999999999996,"y":1425},{"x":12.666666666666663,"y":2441},{"x":13.333333333333329,"y":2173},{"x":13.999999999999995,"y":1686},{"x":14.66666666666666,"y":490},{"x":15.333333333333327,"y":88},{"x":15.999999999999993,"y":23},{"x":16.66666666666666,"y":0},{"x":17.33333333333333,"y":0}]},{"name":"8fc23e16-7a17-4fb8-8154-594eaa2119f0","values":[{"x":5.535630973747839,"y":0},{"x":5.985410025243265,"y":4},{"x":6.435189076738691,"y":13},{"x":6.884968128234117,"y":54},{"x":7.334747179729543,"y":206},{"x":7.7845262312249695,"y":339},{"x":8.234305282720396,"y":875},{"x":8.684084334215822,"y":1579},{"x":9.133863385711248,"y":1916},{"x":9.583642437206674,"y":1607},{"x":10.0334214887021,"y":1206},{"x":10.483200540197526,"y":645},{"x":10.932979591692952,"y":339},{"x":11.382758643188378,"y":149},{"x":11.832537694683804,"y":39},{"x":12.28231674617923,"y":4},{"x":12.732095797674656,"y":1},{"x":13.181874849170082,"y":0}]}],"marks":[{"type":"line","from":{"data":"f25c38dd-3613-43f0-957b-9acc3d69004f"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"8fc23e16-7a17-4fb8-8154-594eaa2119f0"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [6 16]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"f25c38dd-3613-43f0-957b-9acc3d69004f\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"arrival times: mean1=12.6736, sd1=0.904110; mean2=9.04398, sd2=0.885576\", :titleOffset 30} {:type \"y\", :scale \"y\"}], :data ({:name \"f25c38dd-3613-43f0-957b-9acc3d69004f\", :values ({:x 6.0, :y 0} {:x 6.666666666666667, :y 0.0} {:x 7.333333333333334, :y 0.0} {:x 8.0, :y 0.0} {:x 8.666666666666666, :y 0.0} {:x 9.333333333333332, :y 0.0} {:x 9.999999999999998, :y 25.0} {:x 10.666666666666664, :y 111.0} {:x 11.33333333333333, :y 514.0} {:x 11.999999999999996, :y 1425.0} {:x 12.666666666666663, :y 2441.0} {:x 13.333333333333329, :y 2173.0} {:x 13.999999999999995, :y 1686.0} {:x 14.66666666666666, :y 490.0} {:x 15.333333333333327, :y 88.0} {:x 15.999999999999993, :y 23.0} {:x 16.66666666666666, :y 0.0} {:x 17.33333333333333, :y 0})} {:name \"8fc23e16-7a17-4fb8-8154-594eaa2119f0\", :values ({:x 5.535630973747839, :y 0} {:x 5.985410025243265, :y 4.0} {:x 6.435189076738691, :y 13.0} {:x 6.884968128234117, :y 54.0} {:x 7.334747179729543, :y 206.0} {:x 7.7845262312249695, :y 339.0} {:x 8.234305282720396, :y 875.0} {:x 8.684084334215822, :y 1579.0} {:x 9.133863385711248, :y 1916.0} {:x 9.583642437206674, :y 1607.0} {:x 10.0334214887021, :y 1206.0} {:x 10.483200540197526, :y 645.0} {:x 10.932979591692952, :y 339.0} {:x 11.382758643188378, :y 149.0} {:x 11.832537694683804, :y 39.0} {:x 12.28231674617923, :y 4.0} {:x 12.732095797674656, :y 1.0} {:x 13.181874849170082, :y 0})}), :marks ({:type \"line\", :from {:data \"f25c38dd-3613-43f0-957b-9acc3d69004f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"8fc23e16-7a17-4fb8-8154-594eaa2119f0\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}],"value":"[[[[[#'deli/times-to-arrive+,#'deli/mean-1-to-arrive+],#'deli/sd-1-to-arrive+],#'deli/mean-2-to-arrive+],#'deli/sd-2-to-arrive+],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [6 16]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"f25c38dd-3613-43f0-957b-9acc3d69004f\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"arrival times: mean1=12.6736, sd1=0.904110; mean2=9.04398, sd2=0.885576\", :titleOffset 30} {:type \"y\", :scale \"y\"}], :data ({:name \"f25c38dd-3613-43f0-957b-9acc3d69004f\", :values ({:x 6.0, :y 0} {:x 6.666666666666667, :y 0.0} {:x 7.333333333333334, :y 0.0} {:x 8.0, :y 0.0} {:x 8.666666666666666, :y 0.0} {:x 9.333333333333332, :y 0.0} {:x 9.999999999999998, :y 25.0} {:x 10.666666666666664, :y 111.0} {:x 11.33333333333333, :y 514.0} {:x 11.999999999999996, :y 1425.0} {:x 12.666666666666663, :y 2441.0} {:x 13.333333333333329, :y 2173.0} {:x 13.999999999999995, :y 1686.0} {:x 14.66666666666666, :y 490.0} {:x 15.333333333333327, :y 88.0} {:x 15.999999999999993, :y 23.0} {:x 16.66666666666666, :y 0.0} {:x 17.33333333333333, :y 0})} {:name \"8fc23e16-7a17-4fb8-8154-594eaa2119f0\", :values ({:x 5.535630973747839, :y 0} {:x 5.985410025243265, :y 4.0} {:x 6.435189076738691, :y 13.0} {:x 6.884968128234117, :y 54.0} {:x 7.334747179729543, :y 206.0} {:x 7.7845262312249695, :y 339.0} {:x 8.234305282720396, :y 875.0} {:x 8.684084334215822, :y 1579.0} {:x 9.133863385711248, :y 1916.0} {:x 9.583642437206674, :y 1607.0} {:x 10.0334214887021, :y 1206.0} {:x 10.483200540197526, :y 645.0} {:x 10.932979591692952, :y 339.0} {:x 11.382758643188378, :y 149.0} {:x 11.832537694683804, :y 39.0} {:x 12.28231674617923, :y 4.0} {:x 12.732095797674656, :y 1.0} {:x 13.181874849170082, :y 0})}), :marks ({:type \"line\", :from {:data \"f25c38dd-3613-43f0-957b-9acc3d69004f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"8fc23e16-7a17-4fb8-8154-594eaa2119f0\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}]"}
;; <=

;; **
;;; 
;; **

;; **
;;; 
;; **

;; **
;;; Now, SMC is, as mentioned above, a particularly high-variance (noisy) inference algorithm. However, in this case, we can cut through that noise to reach a better approximation of the posterior. All we must do is notice that the shape of the posterior distribution over `times-to-arrive` is approximately normally-distributed, and that any deviations from the perfect bell curve can be reasonably explained as deviations due to SMC's imperfect, high-variance inference. This insight will allow us to find an inference algorithm that works better in this case.
;;; 
;;; Regardless of our inference results, we would in fact expect the posterior distribution over `times-to-arrive` to be normally-distributed, seeing as it is (essentially) formed by conditioning Gaussian observations on a Gaussian prior. So the assumption that the posterior distribution is normally-distributed is an emininently reasonable one.
;;; 
;;; Normal distributions are parameterized by only two numbers: a mean and a standard deviation. Therefore, on the assumption that the posterior distribution is known to be normally-distributed, we can compress our long list of Monte Carlo samples, all that data, into just two numbers: a mean and a standard deviation. We can do the same for each of the normally-distributed quantities in this inference problem. In the exact same way, all the relevant information we've learned about whether the two customers are in fact one customer can be represented by the probability `p-same+`: we can use `p-same+` as the parameter of a `flip` distribution approximating the true posterior probability that the two customers are the same given the observed arrival times.
;;; 
;;; That is, if we have the quantities `p-same+`, `mean-to-arrive+`, `sd-to-arrive+`, `mean-1-to-arrive+`, `sd-1-to-arrive+`, `mean-2-to-arrive+`, and `sd-2-to-arrive+`, we can take approximate samples from the posterior simply by sampling from the below query:
;; **

;; @@
(defquery deli-conditional
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/deli-conditional</span>","value":"#'deli/deli-conditional"}
;; <=

;; **
;;; This is the fundamental insight of **variational inference**. By making this approximation, we have done away with the noise in our inference results, replacing it with a smooth distribution - subject to the approximate correctness of our distributional assumptions.
;;; 
;;; In variational inference, we make this type of approximation at the outset, instead of explicitly using Monte Carlo samples as an intermediary step. Rather than build up an approximation to the posterior by taking Monte Carlo samples, we can build such approximations by assuming that the posterior distribution is known to have a particular parameterized shape, and then directly searching for parameters that are good matches for the true posterior, the true data supplied.
;;; 
;;; Anglican contains a range of algorithms performing variational inference, depending on the particular assumptions made about the properties of the posterior distribution. The variational inference algorithm we'll use here, which is probably the simplest of the lot, is **black-box variational Bayesian inference** (BBVB).
;;; 
;;; We won't dive into the particular assumptions made by the BBVB algorithm here, in the context of comparing it to other variational inference algorithms, but the important part is that we don't actually have to. Anglican will itself make a set of assumptions about the posterior distribution, and use it to calculate approximate samples from the posterior. We can use these samples to represent what we've learned about the posterior distribution, or we can pull out a variational approximation to the posterior directly.
;;; 
;;; Using BBVB is as simple as using any other inference algorithm. Just call BBVB and take samples:
;; **

;; @@
(def samples+ (take N (doquery :bbvb deli [])))

(take-last 3 samples+)
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/samples+</span>","value":"#'deli/samples+"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:log-weight</span>","value":":log-weight"},{"type":"html","content":"<span class='clj-double'>-5.615470814721627</span>","value":"-5.615470814721627"}],"value":"[:log-weight -5.615470814721627]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:result</span>","value":":result"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:same-customer?</span>","value":":same-customer?"},{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}],"value":"[:same-customer? false]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:times-to-arrive</span>","value":":times-to-arrive"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>13.365501961513846</span>","value":"13.365501961513846"},{"type":"html","content":"<span class='clj-double'>9.610885783315878</span>","value":"9.610885783315878"}],"value":"[13.365501961513846 9.610885783315878]"}],"value":"[:times-to-arrive [13.365501961513846 9.610885783315878]]"}],"value":"{:same-customer? false, :times-to-arrive [13.365501961513846 9.610885783315878]}"}],"value":"[:result {:same-customer? false, :times-to-arrive [13.365501961513846 9.610885783315878]}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:predicts</span>","value":":predicts"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[],"value":"[]"}],"value":"[:predicts []]"}],"value":"{:log-weight -5.615470814721627, :result {:same-customer? false, :times-to-arrive [13.365501961513846 9.610885783315878]}, :predicts []}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:log-weight</span>","value":":log-weight"},{"type":"html","content":"<span class='clj-double'>-5.615470814723419</span>","value":"-5.615470814723419"}],"value":"[:log-weight -5.615470814723419]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:result</span>","value":":result"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:same-customer?</span>","value":":same-customer?"},{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}],"value":"[:same-customer? false]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:times-to-arrive</span>","value":":times-to-arrive"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>12.073506293353397</span>","value":"12.073506293353397"},{"type":"html","content":"<span class='clj-double'>7.958169310857886</span>","value":"7.958169310857886"}],"value":"[12.073506293353397 7.958169310857886]"}],"value":"[:times-to-arrive [12.073506293353397 7.958169310857886]]"}],"value":"{:same-customer? false, :times-to-arrive [12.073506293353397 7.958169310857886]}"}],"value":"[:result {:same-customer? false, :times-to-arrive [12.073506293353397 7.958169310857886]}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:predicts</span>","value":":predicts"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[],"value":"[]"}],"value":"[:predicts []]"}],"value":"{:log-weight -5.615470814723419, :result {:same-customer? false, :times-to-arrive [12.073506293353397 7.958169310857886]}, :predicts []}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:log-weight</span>","value":":log-weight"},{"type":"html","content":"<span class='clj-double'>-5.615470814721256</span>","value":"-5.615470814721256"}],"value":"[:log-weight -5.615470814721256]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:result</span>","value":":result"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:same-customer?</span>","value":":same-customer?"},{"type":"html","content":"<span class='clj-unkown'>false</span>","value":"false"}],"value":"[:same-customer? false]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:times-to-arrive</span>","value":":times-to-arrive"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>13.13605770184928</span>","value":"13.13605770184928"},{"type":"html","content":"<span class='clj-double'>9.222419625215457</span>","value":"9.222419625215457"}],"value":"[13.13605770184928 9.222419625215457]"}],"value":"[:times-to-arrive [13.13605770184928 9.222419625215457]]"}],"value":"{:same-customer? false, :times-to-arrive [13.13605770184928 9.222419625215457]}"}],"value":"[:result {:same-customer? false, :times-to-arrive [13.13605770184928 9.222419625215457]}]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:predicts</span>","value":":predicts"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[],"value":"[]"}],"value":"[:predicts []]"}],"value":"{:log-weight -5.615470814721256, :result {:same-customer? false, :times-to-arrive [13.13605770184928 9.222419625215457]}, :predicts []}"}],"value":"({:log-weight -5.615470814721627, :result {:same-customer? false, :times-to-arrive [13.365501961513846 9.610885783315878]}, :predicts []} {:log-weight -5.615470814723419, :result {:same-customer? false, :times-to-arrive [12.073506293353397 7.958169310857886]}, :predicts []} {:log-weight -5.615470814721256, :result {:same-customer? false, :times-to-arrive [13.13605770184928 9.222419625215457]}, :predicts []})"}],"value":"[#'deli/samples+,({:log-weight -5.615470814721627, :result {:same-customer? false, :times-to-arrive [13.365501961513846 9.610885783315878]}, :predicts []} {:log-weight -5.615470814723419, :result {:same-customer? false, :times-to-arrive [12.073506293353397 7.958169310857886]}, :predicts []} {:log-weight -5.615470814721256, :result {:same-customer? false, :times-to-arrive [13.13605770184928 9.222419625215457]}, :predicts []})]"}
;; <=

;; **
;;; However, in a variational inference algorithm, we can also pull out from the inference results actual learned variational approximations to the posterior. Below are some of the distributions BBVB has used to infer the posterior distributions of the random variables in the query `deli`; while the names of each random variable are inscrutable, you should be able to infer which random variable is which based on the type of distribution and the distribution's properties.
;; **

;; @@
(def distributions (anglican.bbvb/get-variational (last samples+)))

(pprint distributions)
;; @@
;; ->
;;; {S32957
;;;  {(0 anglican.runtime.flip-distribution)
;;;   {:p 0.11626372542049783,
;;;    :dist
;;;    {:min 0.0,
;;;     :max 1.0,
;;;     :dist25434
;;;     #object[org.apache.commons.math3.distribution.UniformRealDistribution 0x507dd6db &quot;org.apache.commons.math3.distribution.UniformRealDistribution@507dd6db&quot;]}}},
;;;  S32947
;;;  {(0 anglican.runtime.normal-distribution)
;;;   {:mean 10.94736839713115,
;;;    :sd 0.6882472243305433,
;;;    :dist25379
;;;    #object[org.apache.commons.math3.distribution.NormalDistribution 0x53e39e8f &quot;org.apache.commons.math3.distribution.NormalDistribution@53e39e8f&quot;]}},
;;;  S32955
;;;  {(0 anglican.runtime.normal-distribution)
;;;   {:mean 12.699999999998912,
;;;    :sd 0.9486832980504298,
;;;    :dist25379
;;;    #object[org.apache.commons.math3.distribution.NormalDistribution 0x4060e7c6 &quot;org.apache.commons.math3.distribution.NormalDistribution@4060e7c6&quot;]}},
;;;  S32953
;;;  {(0 anglican.runtime.normal-distribution)
;;;   {:mean 9.100000000000609,
;;;    :sd 0.9486832980512022,
;;;    :dist25379
;;;    #object[org.apache.commons.math3.distribution.NormalDistribution 0x667e9761 &quot;org.apache.commons.math3.distribution.NormalDistribution@667e9761&quot;]}}}
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/distributions</span>","value":"#'deli/distributions"},{"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}],"value":"[#'deli/distributions,nil]"}
;; <=

;; **
;;; As more and more samples are taken from BBVB, its approximations to the posterior get better and better - in a fashion similar to MCMC inference. Now, the approximations made by BBVB are not applicable to all types of inference problem; more complex problems may be better solved by more advanced variational inference algorithms. This will be discussed elsewhere.
;; **
