;; gorilla-repl.fileformat = 1

;; **
;;; # Stick breaking representation in Anglican
;;; 
;; **

;; **
;;; In this tutorial we will create a probabilistic program that samples from the stick-breaking representation of the Pitman-Yor process.
;;; 
;;; First to load the required libraries,
;; **

;; @@
(ns stick-breaking-pyp
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [anglican core runtime emit [state :only [get-predicts]]]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; According to the stick-breaking procedure, if we let @@\beta_k\sim Beta(1-\sigma,\alpha+\sigma k)@@ and @@w_k=\beta_k\prod_j^{k-1}(1-\beta_j)@@ then @@w=(w_1,w_2,\ldots)@@ is distributed according to the weights of the random atoms of a draw from the Pitman-Yor process.
;;; 
;;; We define memoized functions to calculate these values,
;; **

;; @@
(defm pyp-breaking-rule 
  [alpha sigma k] (sample (beta (- 1.0 sigma) (+ alpha (* sigma k)))))

(defm remaining  
  [b k] 
    (if (<= k 0)
      1
      (* (- 1 (b k)) (remaining b (- k 1)))))

(defm stick [breaking-rule]
  (let [b (mem breaking-rule)
        r (mem (partial remaining b))]
    (fn [k] 
      (if (< 0 k)
     	    (* (b k) 
               (r (- k 1)))
            0))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;stick-breaking-pyp/stick</span>","value":"#'stick-breaking-pyp/stick"}
;; <=

;; **
;;; Now to define functions that sample the stick,
;; **

;; @@
(defm pick-a-stick
  "Returns the index such that the cumulative sum of w_k is less than or equal to v."
  [w v]
  (let [impl (fn impl [u k]
          (if (> u v)
             k
             (impl (+ u (w k)) (+ k 1))))]
    (impl 0.0 1)))

(defm polya [w]
  "Return a function that samples an integer from an infinite sequence of weights"
  (let [uc01 (uniform-continuous 0 1)]
    (fn [] (let [v (sample uc01)]
           (pick-a-stick w v)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;stick-breaking-pyp/polya</span>","value":"#'stick-breaking-pyp/polya"}
;; <=

;; **
;;; Notice how we define a recursive function inside pick-a-stick: this is the use of scope to encapsulate implementation details. Currently, to do this with defm we need to name the function after "fn."
;;; 
;;; The query we define will sample 10 weights and their indices, and will do this for two random draws from the Pitman-Yor process.
;; **

;; @@
(defquery dirichlet-process-experiment [breaking-rule] 
  (let [pi (stick (partial pyp-breaking-rule 10 0.5))
         ;pi (stick geometric-weights)
        z (polya pi)]
    (predict :pi (map pi (range 1 10)))
    (predict :z (repeatedly 10 z))))

(take 2 (map get-predicts (doquery :smc dirichlet-process-experiment [3])))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:pi</span>","value":":pi"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.03455627553100551</span>","value":"0.03455627553100551"},{"type":"html","content":"<span class='clj-double'>0.01278934097465247</span>","value":"0.01278934097465247"},{"type":"html","content":"<span class='clj-double'>0.04831670293676449</span>","value":"0.04831670293676449"},{"type":"html","content":"<span class='clj-double'>4.2869888514357425E-4</span>","value":"4.2869888514357425E-4"},{"type":"html","content":"<span class='clj-double'>0.015022987571213979</span>","value":"0.015022987571213979"},{"type":"html","content":"<span class='clj-double'>0.033006686555447515</span>","value":"0.033006686555447515"},{"type":"html","content":"<span class='clj-double'>0.03764791829773285</span>","value":"0.03764791829773285"},{"type":"html","content":"<span class='clj-double'>0.01970898356336695</span>","value":"0.01970898356336695"},{"type":"html","content":"<span class='clj-double'>0.03632073446997118</span>","value":"0.03632073446997118"}],"value":"(0.03455627553100551 0.01278934097465247 0.04831670293676449 4.2869888514357425E-4 0.015022987571213979 0.033006686555447515 0.03764791829773285 0.01970898356336695 0.03632073446997118)"}],"value":"[:pi (0.03455627553100551 0.01278934097465247 0.04831670293676449 4.2869888514357425E-4 0.015022987571213979 0.033006686555447515 0.03764791829773285 0.01970898356336695 0.03632073446997118)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:z</span>","value":":z"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"},{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>94</span>","value":"94"},{"type":"html","content":"<span class='clj-long'>67</span>","value":"67"},{"type":"html","content":"<span class='clj-long'>11</span>","value":"11"},{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"}],"value":"(14 14 2 2 94 67 11 10 14 5)"}],"value":"[:z (14 14 2 2 94 67 11 10 14 5)]"}],"value":"{:pi (0.03455627553100551 0.01278934097465247 0.04831670293676449 4.2869888514357425E-4 0.015022987571213979 0.033006686555447515 0.03764791829773285 0.01970898356336695 0.03632073446997118), :z (14 14 2 2 94 67 11 10 14 5)}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:pi</span>","value":":pi"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.06087782606336578</span>","value":"0.06087782606336578"},{"type":"html","content":"<span class='clj-double'>0.02188509827020702</span>","value":"0.02188509827020702"},{"type":"html","content":"<span class='clj-double'>0.049246565344307124</span>","value":"0.049246565344307124"},{"type":"html","content":"<span class='clj-double'>0.01638076252084799</span>","value":"0.01638076252084799"},{"type":"html","content":"<span class='clj-double'>0.05913134054727563</span>","value":"0.05913134054727563"},{"type":"html","content":"<span class='clj-double'>1.762583855721312E-5</span>","value":"1.762583855721312E-5"},{"type":"html","content":"<span class='clj-double'>0.11051466629595645</span>","value":"0.11051466629595645"},{"type":"html","content":"<span class='clj-double'>0.011330979416903439</span>","value":"0.011330979416903439"},{"type":"html","content":"<span class='clj-double'>0.007230356141154757</span>","value":"0.007230356141154757"}],"value":"(0.06087782606336578 0.02188509827020702 0.049246565344307124 0.01638076252084799 0.05913134054727563 1.762583855721312E-5 0.11051466629595645 0.011330979416903439 0.007230356141154757)"}],"value":"[:pi (0.06087782606336578 0.02188509827020702 0.049246565344307124 0.01638076252084799 0.05913134054727563 1.762583855721312E-5 0.11051466629595645 0.011330979416903439 0.007230356141154757)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:z</span>","value":":z"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>752</span>","value":"752"},{"type":"html","content":"<span class='clj-long'>21</span>","value":"21"},{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-long'>27</span>","value":"27"},{"type":"html","content":"<span class='clj-long'>117</span>","value":"117"},{"type":"html","content":"<span class='clj-long'>185</span>","value":"185"},{"type":"html","content":"<span class='clj-long'>26</span>","value":"26"},{"type":"html","content":"<span class='clj-long'>24</span>","value":"24"},{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"},{"type":"html","content":"<span class='clj-long'>43</span>","value":"43"}],"value":"(752 21 9 27 117 185 26 24 14 43)"}],"value":"[:z (752 21 9 27 117 185 26 24 14 43)]"}],"value":"{:pi (0.06087782606336578 0.02188509827020702 0.049246565344307124 0.01638076252084799 0.05913134054727563 1.762583855721312E-5 0.11051466629595645 0.011330979416903439 0.007230356141154757), :z (752 21 9 27 117 185 26 24 14 43)}"}],"value":"({:pi (0.03455627553100551 0.01278934097465247 0.04831670293676449 4.2869888514357425E-4 0.015022987571213979 0.033006686555447515 0.03764791829773285 0.01970898356336695 0.03632073446997118), :z (14 14 2 2 94 67 11 10 14 5)} {:pi (0.06087782606336578 0.02188509827020702 0.049246565344307124 0.01638076252084799 0.05913134054727563 1.762583855721312E-5 0.11051466629595645 0.011330979416903439 0.007230356141154757), :z (752 21 9 27 117 185 26 24 14 43)})"}
;; <=

;; **
;;; An interesting exercise might be to define general stick-breaking as a built-in random distribution.
;; **
