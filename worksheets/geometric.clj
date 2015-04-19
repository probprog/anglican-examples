;; gorilla-repl.fileformat = 1

;; **
;;; # Geometric
;;; 
;;; In probabilistic programming systems like Anglican that support recursion and branching on nondeterminism it is possible to write procedures in the language itself that sample from distributions with countable support.  Another way of saying this is that we can denote constructive definitions of distribution over countable support and sample from the same.
;; **

;; @@
(ns geometric
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [anglican core runtime emit [state :only [get-predicts]]] 
        [clojure.string :only (join split blank?)]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; We can write geometric in a number of ways, here are two simple examples.
;; **

;; @@
(defn geometric1 
  "generates geometrically distributed values in {0,1,2,...}"
  ([p] (geometric1 p 0))
  ([p n] (if (sample (flip p)) 
           n 
           (geometric1 p (+ n 1)))))

(defn geometric2 [p]
  "generates geometrically distributed values in {0,1,2,...}"
  (loop [n 0] 
    (if (sample (flip p)) 
      n 
      (recur (inc n)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;geometric/geometric2</span>","value":"#'geometric/geometric2"}
;; <=

;; **
;;; We can check that these generators produce sample distributions that are in accordance with our understanding of Geometric which they are.
;; **

;; @@
(defn update-values [m f & args]
  "update values in map m using f val args applied to each element of m"
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(defn estimate-empirical-discrete-dist-from-generator [f c]
  "given sample generator f and a sample count c to use"
  (let [bucket-size 1
        bucketed-counts (->> f
     				(repeatedly c)
     				(group-by #(quot % bucket-size))
     				(map #(hash-map (first %) ((comp count second) %)))
   					(into (sorted-map)))]
    (into (sorted-map) 
          (update-values 
            bucketed-counts 
            #(float (/ % (reduce + (vals bucketed-counts))))))))


(def geometric1dist (estimate-empirical-discrete-dist-from-generator (partial geometric1 0.2) 10000))       

(def geometric2dist (estimate-empirical-discrete-dist-from-generator (partial geometric2 0.2) 10000)) 

(plot/compose (plot/list-plot geometric1dist) (plot/list-plot geometric2dist))
geometric2dist
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-unkown'>0.204</span>","value":"0.204"}],"value":"[0 0.204]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>0.1593</span>","value":"0.1593"}],"value":"[1 0.1593]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>0.1264</span>","value":"0.1264"}],"value":"[2 0.1264]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-unkown'>0.1026</span>","value":"0.1026"}],"value":"[3 0.1026]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-unkown'>0.081</span>","value":"0.081"}],"value":"[4 0.081]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-unkown'>0.0658</span>","value":"0.0658"}],"value":"[5 0.0658]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-unkown'>0.0515</span>","value":"0.0515"}],"value":"[6 0.0515]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-unkown'>0.0427</span>","value":"0.0427"}],"value":"[7 0.0427]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-unkown'>0.0342</span>","value":"0.0342"}],"value":"[8 0.0342]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-unkown'>0.0252</span>","value":"0.0252"}],"value":"[9 0.0252]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-unkown'>0.0238</span>","value":"0.0238"}],"value":"[10 0.0238]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>11</span>","value":"11"},{"type":"html","content":"<span class='clj-unkown'>0.0172</span>","value":"0.0172"}],"value":"[11 0.0172]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>12</span>","value":"12"},{"type":"html","content":"<span class='clj-unkown'>0.0127</span>","value":"0.0127"}],"value":"[12 0.0127]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>13</span>","value":"13"},{"type":"html","content":"<span class='clj-unkown'>0.0103</span>","value":"0.0103"}],"value":"[13 0.0103]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"},{"type":"html","content":"<span class='clj-unkown'>0.0099</span>","value":"0.0099"}],"value":"[14 0.0099]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>15</span>","value":"15"},{"type":"html","content":"<span class='clj-unkown'>0.0075</span>","value":"0.0075"}],"value":"[15 0.0075]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>16</span>","value":"16"},{"type":"html","content":"<span class='clj-unkown'>0.0047</span>","value":"0.0047"}],"value":"[16 0.0047]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>17</span>","value":"17"},{"type":"html","content":"<span class='clj-unkown'>0.004</span>","value":"0.004"}],"value":"[17 0.004]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>18</span>","value":"18"},{"type":"html","content":"<span class='clj-unkown'>0.0045</span>","value":"0.0045"}],"value":"[18 0.0045]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>19</span>","value":"19"},{"type":"html","content":"<span class='clj-unkown'>0.0024</span>","value":"0.0024"}],"value":"[19 0.0024]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>20</span>","value":"20"},{"type":"html","content":"<span class='clj-unkown'>0.0015</span>","value":"0.0015"}],"value":"[20 0.0015]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>21</span>","value":"21"},{"type":"html","content":"<span class='clj-unkown'>0.0022</span>","value":"0.0022"}],"value":"[21 0.0022]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"},{"type":"html","content":"<span class='clj-unkown'>0.0018</span>","value":"0.0018"}],"value":"[22 0.0018]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>23</span>","value":"23"},{"type":"html","content":"<span class='clj-unkown'>0.0015</span>","value":"0.0015"}],"value":"[23 0.0015]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>24</span>","value":"24"},{"type":"html","content":"<span class='clj-unkown'>5.0E-4</span>","value":"5.0E-4"}],"value":"[24 5.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>25</span>","value":"25"},{"type":"html","content":"<span class='clj-unkown'>6.0E-4</span>","value":"6.0E-4"}],"value":"[25 6.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>26</span>","value":"26"},{"type":"html","content":"<span class='clj-unkown'>7.0E-4</span>","value":"7.0E-4"}],"value":"[26 7.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>27</span>","value":"27"},{"type":"html","content":"<span class='clj-unkown'>3.0E-4</span>","value":"3.0E-4"}],"value":"[27 3.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>28</span>","value":"28"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[28 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>30</span>","value":"30"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[30 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>31</span>","value":"31"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[31 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>32</span>","value":"32"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[32 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>33</span>","value":"33"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[33 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>34</span>","value":"34"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[34 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>35</span>","value":"35"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[35 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>40</span>","value":"40"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[40 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>55</span>","value":"55"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[55 1.0E-4]"}],"value":"{0 0.204, 1 0.1593, 2 0.1264, 3 0.1026, 4 0.081, 5 0.0658, 6 0.0515, 7 0.0427, 8 0.0342, 9 0.0252, 10 0.0238, 11 0.0172, 12 0.0127, 13 0.0103, 14 0.0099, 15 0.0075, 16 0.0047, 17 0.004, 18 0.0045, 19 0.0024, 20 0.0015, 21 0.0022, 22 0.0018, 23 0.0015, 24 5.0E-4, 25 6.0E-4, 26 7.0E-4, 27 3.0E-4, 28 2.0E-4, 30 2.0E-4, 31 1.0E-4, 32 1.0E-4, 33 2.0E-4, 34 1.0E-4, 35 1.0E-4, 40 1.0E-4, 55 1.0E-4}"}
;; <=

;; **
;;; What is interesting, however, is that one can't immediately deduce, even in this very simple example, that the return value from these procedures are "scorable" in the sense that they can be "observed" in Anglican. 
;;; 
;;; In order to make these procedures scorable one must, for now, be able to provide a log-pdf/pmf calculator.  
;; **

;; @@
(defdist geometric
  "Geometric distribution on support {0,1,2....}"
  [p] []
    (sample [this] (geometric2 p))
    (observe [this value] (+ (log p) (* value (log (- 1 p))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@6b32bf7f&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@6b32bf7f>"}
;; <=

;; @@
(exp (observe (geometric 0.2) 6))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-double'>0.052428800000000005</span>","value":"0.052428800000000005"}
;; <=

;; **
;;; One of the interesting research efforts involves writing procedures that can automatically generate scoring functions given sampling code, something like (derive-score-fn-for geometric) 
;; **

;; @@
(geometric 0.2)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-record'>#geometric.geometric-distribution{</span>","close":"<span class='clj-record'>}</span>","separator":" ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:p</span>","value":":p"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"[:p 0.2]"}],"value":"(geometric/geometric 0.2)"}
;; <=
