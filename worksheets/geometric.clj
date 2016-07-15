;; gorilla-repl.fileformat = 1

;; **
;;; # Geometric
;;; 
;;; In probabilistic programming systems like Anglican that support recursion and branching on nondeterminism it is possible to write procedures in the language itself that sample from distributions with countable support.  Another way of saying this is that we can denote constructive definitions of distribution over countable support and sample from the same.
;; **

;; @@
(ns geometric
    (:require [gorilla-plot.core :as plot]
            [clojure.core.matrix :as m]
            [anglican.stat :as s]
            :reload)
  (:use clojure.repl
        [anglican 
          core 
          runtime 
          emit 
          [state :only [get-predicts get-log-weight]]
          [inference :only [collect-by]]]))
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
  ([p n] (if (sample* (flip p)) 
           n 
           (geometric1 p (+ n 1)))))

(defn geometric2 [p]
  "generates geometrically distributed values in {0,1,2,...}"
  (loop [n 0] 
    (if (sample* (flip p)) 
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
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-unkown'>0.1963</span>","value":"0.1963"}],"value":"[0 0.1963]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>0.1627</span>","value":"0.1627"}],"value":"[1 0.1627]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>0.1268</span>","value":"0.1268"}],"value":"[2 0.1268]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-unkown'>0.1029</span>","value":"0.1029"}],"value":"[3 0.1029]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-unkown'>0.0794</span>","value":"0.0794"}],"value":"[4 0.0794]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-unkown'>0.0674</span>","value":"0.0674"}],"value":"[5 0.0674]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-unkown'>0.0544</span>","value":"0.0544"}],"value":"[6 0.0544]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-unkown'>0.0419</span>","value":"0.0419"}],"value":"[7 0.0419]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-unkown'>0.0358</span>","value":"0.0358"}],"value":"[8 0.0358]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-unkown'>0.0259</span>","value":"0.0259"}],"value":"[9 0.0259]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-unkown'>0.0203</span>","value":"0.0203"}],"value":"[10 0.0203]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>11</span>","value":"11"},{"type":"html","content":"<span class='clj-unkown'>0.0178</span>","value":"0.0178"}],"value":"[11 0.0178]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>12</span>","value":"12"},{"type":"html","content":"<span class='clj-unkown'>0.0152</span>","value":"0.0152"}],"value":"[12 0.0152]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>13</span>","value":"13"},{"type":"html","content":"<span class='clj-unkown'>0.0115</span>","value":"0.0115"}],"value":"[13 0.0115]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"},{"type":"html","content":"<span class='clj-unkown'>0.0086</span>","value":"0.0086"}],"value":"[14 0.0086]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>15</span>","value":"15"},{"type":"html","content":"<span class='clj-unkown'>0.0072</span>","value":"0.0072"}],"value":"[15 0.0072]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>16</span>","value":"16"},{"type":"html","content":"<span class='clj-unkown'>0.0044</span>","value":"0.0044"}],"value":"[16 0.0044]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>17</span>","value":"17"},{"type":"html","content":"<span class='clj-unkown'>0.0047</span>","value":"0.0047"}],"value":"[17 0.0047]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>18</span>","value":"18"},{"type":"html","content":"<span class='clj-unkown'>0.0045</span>","value":"0.0045"}],"value":"[18 0.0045]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>19</span>","value":"19"},{"type":"html","content":"<span class='clj-unkown'>0.0027</span>","value":"0.0027"}],"value":"[19 0.0027]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>20</span>","value":"20"},{"type":"html","content":"<span class='clj-unkown'>0.0019</span>","value":"0.0019"}],"value":"[20 0.0019]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>21</span>","value":"21"},{"type":"html","content":"<span class='clj-unkown'>0.0013</span>","value":"0.0013"}],"value":"[21 0.0013]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"},{"type":"html","content":"<span class='clj-unkown'>0.0012</span>","value":"0.0012"}],"value":"[22 0.0012]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>23</span>","value":"23"},{"type":"html","content":"<span class='clj-unkown'>7.0E-4</span>","value":"7.0E-4"}],"value":"[23 7.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>24</span>","value":"24"},{"type":"html","content":"<span class='clj-unkown'>0.0014</span>","value":"0.0014"}],"value":"[24 0.0014]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>25</span>","value":"25"},{"type":"html","content":"<span class='clj-unkown'>4.0E-4</span>","value":"4.0E-4"}],"value":"[25 4.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>26</span>","value":"26"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[26 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>27</span>","value":"27"},{"type":"html","content":"<span class='clj-unkown'>7.0E-4</span>","value":"7.0E-4"}],"value":"[27 7.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>28</span>","value":"28"},{"type":"html","content":"<span class='clj-unkown'>4.0E-4</span>","value":"4.0E-4"}],"value":"[28 4.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>29</span>","value":"29"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[29 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>30</span>","value":"30"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[30 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>32</span>","value":"32"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[32 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>33</span>","value":"33"},{"type":"html","content":"<span class='clj-unkown'>3.0E-4</span>","value":"3.0E-4"}],"value":"[33 3.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>34</span>","value":"34"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[34 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>35</span>","value":"35"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[35 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>37</span>","value":"37"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[37 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>41</span>","value":"41"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[41 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>44</span>","value":"44"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[44 1.0E-4]"}],"value":"{0 0.1963, 1 0.1627, 2 0.1268, 3 0.1029, 4 0.0794, 5 0.0674, 6 0.0544, 7 0.0419, 8 0.0358, 9 0.0259, 10 0.0203, 11 0.0178, 12 0.0152, 13 0.0115, 14 0.0086, 15 0.0072, 16 0.0044, 17 0.0047, 18 0.0045, 19 0.0027, 20 0.0019, 21 0.0013, 22 0.0012, 23 7.0E-4, 24 0.0014, 25 4.0E-4, 26 2.0E-4, 27 7.0E-4, 28 4.0E-4, 29 1.0E-4, 30 1.0E-4, 32 2.0E-4, 33 3.0E-4, 34 1.0E-4, 35 2.0E-4, 37 1.0E-4, 41 2.0E-4, 44 1.0E-4}"}
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
    (sample* [this] (geometric2 p))
    (observe* [this value] (+ (log p) (* value (log (- 1 p))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#multifn[print-method 0x51f4f763]</span>","value":"#multifn[print-method 0x51f4f763]"}
;; <=

;; @@
(exp (observe* (geometric 0.2) 6))
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
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:p</span>","value":":p"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"[:p 0.2]"}],"value":"(geometric/geometric 0.2)"}
;; <=

;; @@
(defquery geometric-query [p]
  "generates geometrically distributed values in {0,1,2,...}"
  (let [dist (flip p)
        until-success (fn until-success [p n] 
                         (if (sample dist)
                           n 
                           (until-success p (+ n 1))))]
       (predict :x (until-success p 0))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;geometric/geometric-query</span>","value":"#'geometric/geometric-query"}
;; <=

;; @@
(defquery geometric-query [p]
  "generates geometrically distributed 
   values in {0,1,2,...}"
  (let [dist (flip p)
        x (loop [n 0]
               (if (sample dist)
                   n 
                   (recur (+ n 1))))]
    x))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;geometric/geometric-query</span>","value":"#'geometric/geometric-query"}
;; <=

;; @@
(def geometric-dist (conditional geometric-query :lmh))
(def geometric (geometric-dist 0.4))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;geometric/geometric</span>","value":"#'geometric/geometric"}
;; <=

;; @@
(repeatedly 100 #(sample* geometric))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"}],"value":"(2 1 1 2 4 4 3 3 4 3 0 0 1 1 1 1 3 2 0 0 0 0 0 0 0 0 0 2 0 0 0 0 0 0 0 0 0 4 4 4 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 2 6 3 0 0 0 0 0 0 0 0 0 0 0 2 2 1 0 0 0 0 0 0 2 2 2 2 4 2 3 1 1 1 1 0 0 1 1 3 1 1 1 0 0)"}
;; <=
