;; gorilla-repl.fileformat = 1

;; **
;;; # Birthday
;;; 
;;; Shift + enter evaluates code. Click the menu icon for more commands ...
;;; 
;;; First we set up the Anglican and Clojure namespaces.  Also Gorilla plotting.
;; **

;; @@
(ns pdia
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [mrepl core]
        [embang runtime emit]
        [anglib crp]
        [clojure.string :only (join split blank?)]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Answering a question like “Approximately, what’s the probability that in a room filled with 23 people at least one pair of people have the same birthday?” is straightforward. All one needs to do is generate the underlying phenomena and then pose a question that computes the quantity of interest from the generated data.
;;; 
;;; To find out the approximate probability of a pair or more sharing the same birthday we need a model that generates birthdays. A simple and reasonably accurate model encodes the assumption that birthdays are generated independently for all people uniformly over the set of days in a year. The hardest part of answering the question is writing the function to compute the answer to the question of interest.
;; **

;; @@
(defn equal-pair-in-sequence [x] 
  (if (empty? x) false
    (if (not (empty? (filter #(= (first x) %) (rest x))))
      true
      (equal-pair-in-sequence (rest x)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/equal-pair-in-sequence</span>","value":"#'pdia/equal-pair-in-sequence"}
;; <=

;; @@
(with-primitive-procedures [equal-pair-in-sequence]
  (defquery equal-birthday-in-a-group-of N 
    "birthday: takes number of people in room"
     (let [birthday-distribution (uniform-discrete 1 366)
           birthdays (repeatedly N (fn [] (sample birthday-distribution)))]
           (predict :b birthdays)
           (predict :eq (equal-pair-in-sequence birthdays)))))
                           
          
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/equal-birthday-in-a-group-of</span>","value":"#'pdia/equal-birthday-in-a-group-of"}
;; <=

;; @@
(def N 23)
(def sampler (doquery :smc equal-birthday-in-a-group-of N :number-of-particles 1))
(def t (map #(if % 1 0) (map :eq (map get-predicts sampler))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/t</span>","value":"#'pdia/t"}
;; <=

;; @@
(plot/histogram (take 1000 t))
(def samples (take 1000 (map :eq (map get-predicts sampler))))
(float (/ (count (filter true? samples)) (count samples)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>0.523</span>","value":"0.523"}
;; <=

;; **
;;; Looks like around N = 23 the probability of finding two people with exactly the same birthday is arond 50/50.  Just for the fun of it let's use inference machinery to ask what number of people would be needed to make this probability 90%.  
;; **

;; @@
(with-primitive-procedures [doquery get-predicts]
  (defquery num-people-in-room-needed-to-have-X-percent-chance-of-overlapping-birthday X 
    "num-people-in-room-needed-to-have-X-percent-chance-of-overlapping-birthday: "
     (let [num-people-in-room (sample (uniform-discrete 23 50))
           outcomes (doquery :smc equal-birthday-in-a-group-of num-people-in-room)
           predicts (map get-predicts (take 1000 outcomes))
		   probability (float (/ (count (filter true? predicts)) (count predicts)))]
       (observe (beta (* 1000 X) (* 1000 (- 1 X))) probability)
       (predict :N num-people-in-room))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/num-people-in-room-needed-to-have-X-percent-chance-of-overlapping-birthday</span>","value":"#'pdia/num-people-in-room-needed-to-have-X-percent-chance-of-overlapping-birthday"}
;; <=

;; @@
(def sampler2 (doquery :almh num-people-in-room-needed-to-have-X-percent-chance-of-overlapping-birthday 0.9))
(def u (map :N (map get-predicts sampler2)))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/u</span>","value":"#'pdia/u"}
;; <=

;; @@
(time (plot/histogram (take 5000 u)))
;; @@
;; ->
;;; &quot;Elapsed time: 1213887.240166 msecs&quot;
;;; 
;; <-
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"e6395e57-91c9-4b6f-8915-fb19bdb5921f","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"e6395e57-91c9-4b6f-8915-fb19bdb5921f","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"e6395e57-91c9-4b6f-8915-fb19bdb5921f"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"e6395e57-91c9-4b6f-8915-fb19bdb5921f","values":[{"x":38.0,"y":0},{"x":39.0,"y":5000.0},{"x":40.0,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"e6395e57-91c9-4b6f-8915-fb19bdb5921f\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"e6395e57-91c9-4b6f-8915-fb19bdb5921f\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"e6395e57-91c9-4b6f-8915-fb19bdb5921f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"e6395e57-91c9-4b6f-8915-fb19bdb5921f\", :values ({:x 38.0, :y 0} {:x 39.0, :y 5000.0} {:x 40.0, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; Visually it would appear that this occurs around 38-40 people.  We can algorithmically find the mode for potential subsequent use:
;; **

;; @@
(def bucket-size 1)
(->> u
     (take 5000)
     (group-by #(quot % bucket-size))
     (sort-by (comp - count second))
     ffirst
     (* bucket-size))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>38</span>","value":"38"}
;; <=

;; @@

;; @@
