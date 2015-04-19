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
(ns stick-breaking
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [anglican core runtime emit [state :only [get-predicts]]] 
        [anglib crp]
        [clojure.string :only (join split blank?)]))
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;stick-breaking/stick</span>","value":"#'stick-breaking/stick"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;stick-breaking/polya</span>","value":"#'stick-breaking/polya"}
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
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:pi</span>","value":":pi"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.025586458324360604</span>","value":"0.025586458324360604"},{"type":"html","content":"<span class='clj-double'>0.029557234678674927</span>","value":"0.029557234678674927"},{"type":"html","content":"<span class='clj-double'>0.03258150113760542</span>","value":"0.03258150113760542"},{"type":"html","content":"<span class='clj-double'>0.0018754523655111267</span>","value":"0.0018754523655111267"},{"type":"html","content":"<span class='clj-double'>0.037801407316912616</span>","value":"0.037801407316912616"},{"type":"html","content":"<span class='clj-double'>3.612004428335309E-6</span>","value":"3.612004428335309E-6"},{"type":"html","content":"<span class='clj-double'>0.005078326396688783</span>","value":"0.005078326396688783"},{"type":"html","content":"<span class='clj-double'>0.04161163687356416</span>","value":"0.04161163687356416"},{"type":"html","content":"<span class='clj-double'>0.020822609093748494</span>","value":"0.020822609093748494"}],"value":"(0.025586458324360604 0.029557234678674927 0.03258150113760542 0.0018754523655111267 0.037801407316912616 3.612004428335309E-6 0.005078326396688783 0.04161163687356416 0.020822609093748494)"}],"value":"[:pi (0.025586458324360604 0.029557234678674927 0.03258150113760542 0.0018754523655111267 0.037801407316912616 3.612004428335309E-6 0.005078326396688783 0.04161163687356416 0.020822609093748494)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:z</span>","value":":z"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>71</span>","value":"71"},{"type":"html","content":"<span class='clj-long'>21</span>","value":"21"},{"type":"html","content":"<span class='clj-long'>120</span>","value":"120"},{"type":"html","content":"<span class='clj-long'>55</span>","value":"55"},{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"},{"type":"html","content":"<span class='clj-long'>17</span>","value":"17"},{"type":"html","content":"<span class='clj-long'>19</span>","value":"19"},{"type":"html","content":"<span class='clj-long'>19</span>","value":"19"},{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"}],"value":"(2 71 21 120 55 14 17 19 19 14)"}],"value":"[:z (2 71 21 120 55 14 17 19 19 14)]"}],"value":"{:pi (0.025586458324360604 0.029557234678674927 0.03258150113760542 0.0018754523655111267 0.037801407316912616 3.612004428335309E-6 0.005078326396688783 0.04161163687356416 0.020822609093748494), :z (2 71 21 120 55 14 17 19 19 14)}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:pi</span>","value":":pi"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.035109360197997215</span>","value":"0.035109360197997215"},{"type":"html","content":"<span class='clj-double'>0.09450073845943908</span>","value":"0.09450073845943908"},{"type":"html","content":"<span class='clj-double'>0.07916566722199402</span>","value":"0.07916566722199402"},{"type":"html","content":"<span class='clj-double'>0.0100487962425256</span>","value":"0.0100487962425256"},{"type":"html","content":"<span class='clj-double'>0.0032410839676216</span>","value":"0.0032410839676216"},{"type":"html","content":"<span class='clj-double'>3.4809821223789374E-4</span>","value":"3.4809821223789374E-4"},{"type":"html","content":"<span class='clj-double'>0.03789537713274865</span>","value":"0.03789537713274865"},{"type":"html","content":"<span class='clj-double'>0.01682728383016305</span>","value":"0.01682728383016305"},{"type":"html","content":"<span class='clj-double'>0.00587994979606661</span>","value":"0.00587994979606661"}],"value":"(0.035109360197997215 0.09450073845943908 0.07916566722199402 0.0100487962425256 0.0032410839676216 3.4809821223789374E-4 0.03789537713274865 0.01682728383016305 0.00587994979606661)"}],"value":"[:pi (0.035109360197997215 0.09450073845943908 0.07916566722199402 0.0100487962425256 0.0032410839676216 3.4809821223789374E-4 0.03789537713274865 0.01682728383016305 0.00587994979606661)]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:z</span>","value":":z"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>41</span>","value":"41"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>123</span>","value":"123"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>126</span>","value":"126"},{"type":"html","content":"<span class='clj-long'>31</span>","value":"31"}],"value":"(41 3 123 4 8 5 14 3 126 31)"}],"value":"[:z (41 3 123 4 8 5 14 3 126 31)]"}],"value":"{:pi (0.035109360197997215 0.09450073845943908 0.07916566722199402 0.0100487962425256 0.0032410839676216 3.4809821223789374E-4 0.03789537713274865 0.01682728383016305 0.00587994979606661), :z (41 3 123 4 8 5 14 3 126 31)}"}],"value":"({:pi (0.025586458324360604 0.029557234678674927 0.03258150113760542 0.0018754523655111267 0.037801407316912616 3.612004428335309E-6 0.005078326396688783 0.04161163687356416 0.020822609093748494), :z (2 71 21 120 55 14 17 19 19 14)} {:pi (0.035109360197997215 0.09450073845943908 0.07916566722199402 0.0100487962425256 0.0032410839676216 3.4809821223789374E-4 0.03789537713274865 0.01682728383016305 0.00587994979606661), :z (41 3 123 4 8 5 14 3 126 31)})"}
;; <=

;; **
;;; An interesting exercise might be to define general stick-breaking as a built-in random distribution.
;; **
