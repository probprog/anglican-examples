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
        [mrepl core]
        [embang runtime emit]
        [anglib crp]
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
  "generates geometrically distributed r.v.'s in {0,1,2,...}"
  ([p] (geometric1 p 0))
  ([p n] (if (sample (flip p)) 
           n 
           (geometric1 p (+ n 1)))))

(defn geometric2 [p]
  "generates geometrically distributed r.v.'s in {0,1,2,...}"
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
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-unkown'>0.2037</span>","value":"0.2037"}],"value":"[0 0.2037]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>0.1568</span>","value":"0.1568"}],"value":"[1 0.1568]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>0.1247</span>","value":"0.1247"}],"value":"[2 0.1247]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-unkown'>0.0963</span>","value":"0.0963"}],"value":"[3 0.0963]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-unkown'>0.0894</span>","value":"0.0894"}],"value":"[4 0.0894]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-unkown'>0.0671</span>","value":"0.0671"}],"value":"[5 0.0671]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-unkown'>0.0524</span>","value":"0.0524"}],"value":"[6 0.0524]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-unkown'>0.0429</span>","value":"0.0429"}],"value":"[7 0.0429]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-unkown'>0.0341</span>","value":"0.0341"}],"value":"[8 0.0341]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-unkown'>0.0252</span>","value":"0.0252"}],"value":"[9 0.0252]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-unkown'>0.0232</span>","value":"0.0232"}],"value":"[10 0.0232]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>11</span>","value":"11"},{"type":"html","content":"<span class='clj-unkown'>0.0165</span>","value":"0.0165"}],"value":"[11 0.0165]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>12</span>","value":"12"},{"type":"html","content":"<span class='clj-unkown'>0.0124</span>","value":"0.0124"}],"value":"[12 0.0124]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>13</span>","value":"13"},{"type":"html","content":"<span class='clj-unkown'>0.0103</span>","value":"0.0103"}],"value":"[13 0.0103]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"},{"type":"html","content":"<span class='clj-unkown'>0.0114</span>","value":"0.0114"}],"value":"[14 0.0114]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>15</span>","value":"15"},{"type":"html","content":"<span class='clj-unkown'>0.0063</span>","value":"0.0063"}],"value":"[15 0.0063]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>16</span>","value":"16"},{"type":"html","content":"<span class='clj-unkown'>0.0053</span>","value":"0.0053"}],"value":"[16 0.0053]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>17</span>","value":"17"},{"type":"html","content":"<span class='clj-unkown'>0.0049</span>","value":"0.0049"}],"value":"[17 0.0049]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>18</span>","value":"18"},{"type":"html","content":"<span class='clj-unkown'>0.0031</span>","value":"0.0031"}],"value":"[18 0.0031]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>19</span>","value":"19"},{"type":"html","content":"<span class='clj-unkown'>0.003</span>","value":"0.003"}],"value":"[19 0.003]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>20</span>","value":"20"},{"type":"html","content":"<span class='clj-unkown'>0.0027</span>","value":"0.0027"}],"value":"[20 0.0027]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>21</span>","value":"21"},{"type":"html","content":"<span class='clj-unkown'>0.0019</span>","value":"0.0019"}],"value":"[21 0.0019]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"},{"type":"html","content":"<span class='clj-unkown'>0.0012</span>","value":"0.0012"}],"value":"[22 0.0012]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>23</span>","value":"23"},{"type":"html","content":"<span class='clj-unkown'>8.0E-4</span>","value":"8.0E-4"}],"value":"[23 8.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>24</span>","value":"24"},{"type":"html","content":"<span class='clj-unkown'>9.0E-4</span>","value":"9.0E-4"}],"value":"[24 9.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>25</span>","value":"25"},{"type":"html","content":"<span class='clj-unkown'>5.0E-4</span>","value":"5.0E-4"}],"value":"[25 5.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>26</span>","value":"26"},{"type":"html","content":"<span class='clj-unkown'>7.0E-4</span>","value":"7.0E-4"}],"value":"[26 7.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>27</span>","value":"27"},{"type":"html","content":"<span class='clj-unkown'>6.0E-4</span>","value":"6.0E-4"}],"value":"[27 6.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>28</span>","value":"28"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[28 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>29</span>","value":"29"},{"type":"html","content":"<span class='clj-unkown'>4.0E-4</span>","value":"4.0E-4"}],"value":"[29 4.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>30</span>","value":"30"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[30 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>31</span>","value":"31"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[31 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>32</span>","value":"32"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[32 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>35</span>","value":"35"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[35 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>38</span>","value":"38"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[38 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>39</span>","value":"39"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[39 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>41</span>","value":"41"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[41 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>48</span>","value":"48"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[48 1.0E-4]"}],"value":"{0 0.2037, 1 0.1568, 2 0.1247, 3 0.0963, 4 0.0894, 5 0.0671, 6 0.0524, 7 0.0429, 8 0.0341, 9 0.0252, 10 0.0232, 11 0.0165, 12 0.0124, 13 0.0103, 14 0.0114, 15 0.0063, 16 0.0053, 17 0.0049, 18 0.0031, 19 0.003, 20 0.0027, 21 0.0019, 22 0.0012, 23 8.0E-4, 24 9.0E-4, 25 5.0E-4, 26 7.0E-4, 27 6.0E-4, 28 2.0E-4, 29 4.0E-4, 30 2.0E-4, 31 1.0E-4, 32 1.0E-4, 35 2.0E-4, 38 1.0E-4, 39 2.0E-4, 41 1.0E-4, 48 1.0E-4}"}
;; <=

;; **
;;; What is interesting, however, is that one can't immediately deduce, even in this very simple example, that the return value from these procedures are "scorable" in the sense that they should be "observable" in Anglican. 
;;; 
;;; In order to make these procedures scorable one must, for now, be able to provide a log-pdf/pmf calculator.  
;; **

;; @@
(defn geometric
  "Geometric distribution on support {0,1,2....}"
  [p]
    (reify
      distribution
      (sample [this] (geometric2 p))
      (observe [this value] (+ (log p) (* value (log (- 1 p)))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;geometric/geometric</span>","value":"#'geometric/geometric"}
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

;; @@
