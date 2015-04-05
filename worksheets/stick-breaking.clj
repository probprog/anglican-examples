;; gorilla-repl.fileformat = 1

;; **
;;; # Stick breaking representation in Anglican
;;; 
;; **

;; @@
(ns stick-breaking
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

;; @@
(defm dirichlet-process-breaking-rule 
  [alpha k] (sample (beta 1.0 alpha)))

(defm remaining  
  [b k] 
    (if (<= k 0)
      1
      (* (- 1 (b k)) (remaining b (- k 1)))))

(defm stick [breaking-rule] 
  ; given a breaking-rule function which
  ; returns a value between 1 and 0 given a
  ; stick index k returns a function that
  ; returns the stick length for index k
  (let [b (mem breaking-rule)]
    (fn [k] 
      (if (< 0 k)
     	    (* (b k) 
               (remaining b (- k 1)))
            0))))

(defm pick-a-stick [stick v l k]
  ; picks a stick given a stick generator
  ; given a value v ~ uniform-continuous(0,1)
  ; should be called with  l = 0.0, k=1 
  (let [u (+ l (stick k))]
     (if (> u v)
        k
        (pick-a-stick stick v u (+ k 1)))))

(defm polya [stick] 
  ; given a stick generating function
  ; polya returns a function that samples
  ; stick indexes from the stick lengths
  (let [uc01 (uniform-continuous 0 1)]
    (fn [] (let [v (sample uc01)]
           (pick-a-stick stick v 0.0 1)))))


        
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;stick-breaking/polya</span>","value":"#'stick-breaking/polya"}
;; <=

;; @@
   (defquery dirichlet-process-experiment [] 
    (let [pi (stick (partial dirichlet-process-breaking-rule 10))
          z (polya pi)]
      
    (predict :pi (map pi (range 1 10)))
    (predict :z (repeatedly 10 z))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;stick-breaking/dirichlet-process-experiment</span>","value":"#'stick-breaking/dirichlet-process-experiment"}
;; <=

;; @@
(take 2 (map get-predicts (doquery :smc dirichlet-process-experiment [])))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:pi</span>","value":":pi"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.10130068203266263</span>","value":"0.10130068203266263"},{"type":"html","content":"<span class='clj-double'>0.006785121904872634</span>","value":"0.006785121904872634"},{"type":"html","content":"<span class='clj-double'>0.011994354152519319</span>","value":"0.011994354152519319"},{"type":"html","content":"<span class='clj-double'>0.06964589423171154</span>","value":"0.06964589423171154"},{"type":"html","content":"<span class='clj-double'>0.006994516892843838</span>","value":"0.006994516892843838"},{"type":"html","content":"<span class='clj-double'>0.04537401685158987</span>","value":"0.04537401685158987"},{"type":"html","content":"<span class='clj-double'>0.11228479985244012</span>","value":"0.11228479985244012"},{"type":"html","content":"<span class='clj-double'>0.04549877726754165</span>","value":"0.04549877726754165"},{"type":"html","content":"<span class='clj-double'>0.08667545517068206</span>","value":"0.08667545517068206"}],"value":"(0.10130068203266263 0.006785121904872634 0.011994354152519319 0.06964589423171154 0.006994516892843838 0.04537401685158987 0.11228479985244012 0.04549877726754165 0.08667545517068206)"}],"value":"[:pi (0.10130068203266263 0.006785121904872634 0.011994354152519319 0.06964589423171154 0.006994516892843838 0.04537401685158987 0.11228479985244012 0.04549877726754165 0.08667545517068206)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:z</span>","value":":z"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>13</span>","value":"13"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>23</span>","value":"23"},{"type":"html","content":"<span class='clj-long'>15</span>","value":"15"},{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>19</span>","value":"19"}],"value":"(13 1 23 15 14 7 9 4 6 19)"}],"value":"[:z (13 1 23 15 14 7 9 4 6 19)]"}],"value":"{:pi (0.10130068203266263 0.006785121904872634 0.011994354152519319 0.06964589423171154 0.006994516892843838 0.04537401685158987 0.11228479985244012 0.04549877726754165 0.08667545517068206), :z (13 1 23 15 14 7 9 4 6 19)}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:pi</span>","value":":pi"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.3031697884951441</span>","value":"0.3031697884951441"},{"type":"html","content":"<span class='clj-double'>0.021282692292871924</span>","value":"0.021282692292871924"},{"type":"html","content":"<span class='clj-double'>0.0760249195548477</span>","value":"0.0760249195548477"},{"type":"html","content":"<span class='clj-double'>0.0581778168613739</span>","value":"0.0581778168613739"},{"type":"html","content":"<span class='clj-double'>0.1136105677858063</span>","value":"0.1136105677858063"},{"type":"html","content":"<span class='clj-double'>0.06357327843521263</span>","value":"0.06357327843521263"},{"type":"html","content":"<span class='clj-double'>0.013352445891159928</span>","value":"0.013352445891159928"},{"type":"html","content":"<span class='clj-double'>0.06950998352934357</span>","value":"0.06950998352934357"},{"type":"html","content":"<span class='clj-double'>0.011318020158260531</span>","value":"0.011318020158260531"}],"value":"(0.3031697884951441 0.021282692292871924 0.0760249195548477 0.0581778168613739 0.1136105677858063 0.06357327843521263 0.013352445891159928 0.06950998352934357 0.011318020158260531)"}],"value":"[:pi (0.3031697884951441 0.021282692292871924 0.0760249195548477 0.0581778168613739 0.1136105677858063 0.06357327843521263 0.013352445891159928 0.06950998352934357 0.011318020158260531)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:z</span>","value":":z"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>15</span>","value":"15"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"}],"value":"(6 8 5 15 1 5 1 3 1 8)"}],"value":"[:z (6 8 5 15 1 5 1 3 1 8)]"}],"value":"{:pi (0.3031697884951441 0.021282692292871924 0.0760249195548477 0.0581778168613739 0.1136105677858063 0.06357327843521263 0.013352445891159928 0.06950998352934357 0.011318020158260531), :z (6 8 5 15 1 5 1 3 1 8)}"}],"value":"({:pi (0.10130068203266263 0.006785121904872634 0.011994354152519319 0.06964589423171154 0.006994516892843838 0.04537401685158987 0.11228479985244012 0.04549877726754165 0.08667545517068206), :z (13 1 23 15 14 7 9 4 6 19)} {:pi (0.3031697884951441 0.021282692292871924 0.0760249195548477 0.0581778168613739 0.1136105677858063 0.06357327843521263 0.013352445891159928 0.06950998352934357 0.011318020158260531), :z (6 8 5 15 1 5 1 3 1 8)})"}
;; <=

;; @@

;; @@

;; @@

;; @@
