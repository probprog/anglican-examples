;; gorilla-repl.fileformat = 1

;; **
;;; # Stick breaking representation in Anglican
;;; 
;; **

;; @@
(ns stick-breaking
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [anglican core runtime emit [state :only [get-predicts]]]))
;        [anglib crp]
      ;  [clojure.string :only (join split blank?)]))
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
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:pi</span>","value":":pi"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.0743735996700362</span>","value":"0.0743735996700362"},{"type":"html","content":"<span class='clj-double'>0.11236641601487504</span>","value":"0.11236641601487504"},{"type":"html","content":"<span class='clj-double'>0.045203379345561</span>","value":"0.045203379345561"},{"type":"html","content":"<span class='clj-double'>0.07009859019842828</span>","value":"0.07009859019842828"},{"type":"html","content":"<span class='clj-double'>0.06511641254265017</span>","value":"0.06511641254265017"},{"type":"html","content":"<span class='clj-double'>0.02073912045760101</span>","value":"0.02073912045760101"},{"type":"html","content":"<span class='clj-double'>0.010483361027018996</span>","value":"0.010483361027018996"},{"type":"html","content":"<span class='clj-double'>0.03502811194146763</span>","value":"0.03502811194146763"},{"type":"html","content":"<span class='clj-double'>0.03560056587217161</span>","value":"0.03560056587217161"}],"value":"(0.0743735996700362 0.11236641601487504 0.045203379345561 0.07009859019842828 0.06511641254265017 0.02073912045760101 0.010483361027018996 0.03502811194146763 0.03560056587217161)"}],"value":"[:pi (0.0743735996700362 0.11236641601487504 0.045203379345561 0.07009859019842828 0.06511641254265017 0.02073912045760101 0.010483361027018996 0.03502811194146763 0.03560056587217161)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:z</span>","value":":z"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>21</span>","value":"21"},{"type":"html","content":"<span class='clj-long'>20</span>","value":"20"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>21</span>","value":"21"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"}],"value":"(10 10 21 20 4 21 2 8 10 8)"}],"value":"[:z (10 10 21 20 4 21 2 8 10 8)]"}],"value":"{:pi (0.0743735996700362 0.11236641601487504 0.045203379345561 0.07009859019842828 0.06511641254265017 0.02073912045760101 0.010483361027018996 0.03502811194146763 0.03560056587217161), :z (10 10 21 20 4 21 2 8 10 8)}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:pi</span>","value":":pi"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.203148190575967</span>","value":"0.203148190575967"},{"type":"html","content":"<span class='clj-double'>0.036732304870702455</span>","value":"0.036732304870702455"},{"type":"html","content":"<span class='clj-double'>0.09367049121552136</span>","value":"0.09367049121552136"},{"type":"html","content":"<span class='clj-double'>0.016910136906767143</span>","value":"0.016910136906767143"},{"type":"html","content":"<span class='clj-double'>0.023640800151762665</span>","value":"0.023640800151762665"},{"type":"html","content":"<span class='clj-double'>0.024462575948985474</span>","value":"0.024462575948985474"},{"type":"html","content":"<span class='clj-double'>0.03774611892749036</span>","value":"0.03774611892749036"},{"type":"html","content":"<span class='clj-double'>0.16706000874924282</span>","value":"0.16706000874924282"},{"type":"html","content":"<span class='clj-double'>0.032043056479568593</span>","value":"0.032043056479568593"}],"value":"(0.203148190575967 0.036732304870702455 0.09367049121552136 0.016910136906767143 0.023640800151762665 0.024462575948985474 0.03774611892749036 0.16706000874924282 0.032043056479568593)"}],"value":"[:pi (0.203148190575967 0.036732304870702455 0.09367049121552136 0.016910136906767143 0.023640800151762665 0.024462575948985474 0.03774611892749036 0.16706000874924282 0.032043056479568593)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:z</span>","value":":z"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>26</span>","value":"26"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>25</span>","value":"25"},{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-long'>35</span>","value":"35"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}],"value":"(8 26 1 25 7 35 9 2 1 1)"}],"value":"[:z (8 26 1 25 7 35 9 2 1 1)]"}],"value":"{:pi (0.203148190575967 0.036732304870702455 0.09367049121552136 0.016910136906767143 0.023640800151762665 0.024462575948985474 0.03774611892749036 0.16706000874924282 0.032043056479568593), :z (8 26 1 25 7 35 9 2 1 1)}"}],"value":"({:pi (0.0743735996700362 0.11236641601487504 0.045203379345561 0.07009859019842828 0.06511641254265017 0.02073912045760101 0.010483361027018996 0.03502811194146763 0.03560056587217161), :z (10 10 21 20 4 21 2 8 10 8)} {:pi (0.203148190575967 0.036732304870702455 0.09367049121552136 0.016910136906767143 0.023640800151762665 0.024462575948985474 0.03774611892749036 0.16706000874924282 0.032043056479568593), :z (8 26 1 25 7 35 9 2 1 1)})"}
;; <=

;; @@

;; @@

;; @@

;; @@
