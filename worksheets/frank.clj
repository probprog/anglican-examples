;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns tworoads
  (:require [gorilla-plot.core :as plot]
   			[embang lmh
                    [state :refer [get-predicts]]
                    [inference :refer [infer warmup]]])
  (:use [embang emit runtime]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; @@
(def a 6)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/a</span>","value":"#'tworoads/a"}
;; <=

;; @@
(defquery unknown-mean 
  (let [a (sample (normal a 2))] 
    (observe (normal a 1) 18)
    (observe (normal a 3) 9))
     (predict :a a))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/unknown-mean</span>","value":"#'tworoads/unknown-mean"}
;; <=

;; @@
(def unknown-mean (warmup unknown-mean nil))

(def predicts (map get-predicts (infer :lmh unknown-mean nil)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/predicts</span>","value":"#'tworoads/predicts"}
;; <=

;; @@
(def results (take 900 predicts))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;tworoads/results</span>","value":"#'tworoads/results"}
;; <=

;; @@
 (plot/histogram (map second (map first results)))
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"605ef987-aa19-4081-ae60-9b1f39ef79b7","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"605ef987-aa19-4081-ae60-9b1f39ef79b7","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"605ef987-aa19-4081-ae60-9b1f39ef79b7"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"605ef987-aa19-4081-ae60-9b1f39ef79b7","values":[{"x":6.651703528271854,"y":0},{"x":7.288007548783075,"y":4.0},{"x":7.9243115692942965,"y":3.0},{"x":8.560615589805517,"y":0.0},{"x":9.196919610316737,"y":11.0},{"x":9.833223630827957,"y":0.0},{"x":10.469527651339178,"y":0.0},{"x":11.105831671850398,"y":78.0},{"x":11.742135692361618,"y":131.0},{"x":12.378439712872838,"y":0.0},{"x":13.014743733384059,"y":0.0},{"x":13.651047753895279,"y":0.0},{"x":14.287351774406499,"y":673.0},{"x":14.92365579491772,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"605ef987-aa19-4081-ae60-9b1f39ef79b7\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"605ef987-aa19-4081-ae60-9b1f39ef79b7\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"605ef987-aa19-4081-ae60-9b1f39ef79b7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"605ef987-aa19-4081-ae60-9b1f39ef79b7\", :values ({:x 6.651703528271854, :y 0} {:x 7.288007548783075, :y 4.0} {:x 7.9243115692942965, :y 3.0} {:x 8.560615589805517, :y 0.0} {:x 9.196919610316737, :y 11.0} {:x 9.833223630827957, :y 0.0} {:x 10.469527651339178, :y 0.0} {:x 11.105831671850398, :y 78.0} {:x 11.742135692361618, :y 131.0} {:x 12.378439712872838, :y 0.0} {:x 13.014743733384059, :y 0.0} {:x 13.651047753895279, :y 0.0} {:x 14.287351774406499, :y 673.0} {:x 14.92365579491772, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@

;; @@
