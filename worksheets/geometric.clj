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
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-unkown'>0.1972</span>","value":"0.1972"}],"value":"[0 0.1972]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>0.1647</span>","value":"0.1647"}],"value":"[1 0.1647]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>0.1294</span>","value":"0.1294"}],"value":"[2 0.1294]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-unkown'>0.0994</span>","value":"0.0994"}],"value":"[3 0.0994]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-unkown'>0.0804</span>","value":"0.0804"}],"value":"[4 0.0804]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-unkown'>0.0634</span>","value":"0.0634"}],"value":"[5 0.0634]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-unkown'>0.0552</span>","value":"0.0552"}],"value":"[6 0.0552]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-unkown'>0.0423</span>","value":"0.0423"}],"value":"[7 0.0423]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-unkown'>0.0338</span>","value":"0.0338"}],"value":"[8 0.0338]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-unkown'>0.0281</span>","value":"0.0281"}],"value":"[9 0.0281]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-unkown'>0.0207</span>","value":"0.0207"}],"value":"[10 0.0207]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>11</span>","value":"11"},{"type":"html","content":"<span class='clj-unkown'>0.0162</span>","value":"0.0162"}],"value":"[11 0.0162]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>12</span>","value":"12"},{"type":"html","content":"<span class='clj-unkown'>0.0138</span>","value":"0.0138"}],"value":"[12 0.0138]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>13</span>","value":"13"},{"type":"html","content":"<span class='clj-unkown'>0.0116</span>","value":"0.0116"}],"value":"[13 0.0116]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"},{"type":"html","content":"<span class='clj-unkown'>0.0087</span>","value":"0.0087"}],"value":"[14 0.0087]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>15</span>","value":"15"},{"type":"html","content":"<span class='clj-unkown'>0.0065</span>","value":"0.0065"}],"value":"[15 0.0065]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>16</span>","value":"16"},{"type":"html","content":"<span class='clj-unkown'>0.0068</span>","value":"0.0068"}],"value":"[16 0.0068]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>17</span>","value":"17"},{"type":"html","content":"<span class='clj-unkown'>0.0033</span>","value":"0.0033"}],"value":"[17 0.0033]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>18</span>","value":"18"},{"type":"html","content":"<span class='clj-unkown'>0.0029</span>","value":"0.0029"}],"value":"[18 0.0029]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>19</span>","value":"19"},{"type":"html","content":"<span class='clj-unkown'>0.003</span>","value":"0.003"}],"value":"[19 0.003]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>20</span>","value":"20"},{"type":"html","content":"<span class='clj-unkown'>0.0031</span>","value":"0.0031"}],"value":"[20 0.0031]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>21</span>","value":"21"},{"type":"html","content":"<span class='clj-unkown'>0.0017</span>","value":"0.0017"}],"value":"[21 0.0017]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"},{"type":"html","content":"<span class='clj-unkown'>0.0019</span>","value":"0.0019"}],"value":"[22 0.0019]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>23</span>","value":"23"},{"type":"html","content":"<span class='clj-unkown'>6.0E-4</span>","value":"6.0E-4"}],"value":"[23 6.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>24</span>","value":"24"},{"type":"html","content":"<span class='clj-unkown'>9.0E-4</span>","value":"9.0E-4"}],"value":"[24 9.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>25</span>","value":"25"},{"type":"html","content":"<span class='clj-unkown'>9.0E-4</span>","value":"9.0E-4"}],"value":"[25 9.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>26</span>","value":"26"},{"type":"html","content":"<span class='clj-unkown'>7.0E-4</span>","value":"7.0E-4"}],"value":"[26 7.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>27</span>","value":"27"},{"type":"html","content":"<span class='clj-unkown'>4.0E-4</span>","value":"4.0E-4"}],"value":"[27 4.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>28</span>","value":"28"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[28 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>29</span>","value":"29"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[29 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>30</span>","value":"30"},{"type":"html","content":"<span class='clj-unkown'>5.0E-4</span>","value":"5.0E-4"}],"value":"[30 5.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>31</span>","value":"31"},{"type":"html","content":"<span class='clj-unkown'>4.0E-4</span>","value":"4.0E-4"}],"value":"[31 4.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>32</span>","value":"32"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[32 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>33</span>","value":"33"},{"type":"html","content":"<span class='clj-unkown'>5.0E-4</span>","value":"5.0E-4"}],"value":"[33 5.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>34</span>","value":"34"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[34 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>35</span>","value":"35"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[35 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>36</span>","value":"36"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[36 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>49</span>","value":"49"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[49 1.0E-4]"}],"value":"{0 0.1972, 1 0.1647, 2 0.1294, 3 0.0994, 4 0.0804, 5 0.0634, 6 0.0552, 7 0.0423, 8 0.0338, 9 0.0281, 10 0.0207, 11 0.0162, 12 0.0138, 13 0.0116, 14 0.0087, 15 0.0065, 16 0.0068, 17 0.0033, 18 0.0029, 19 0.003, 20 0.0031, 21 0.0017, 22 0.0019, 23 6.0E-4, 24 9.0E-4, 25 9.0E-4, 26 7.0E-4, 27 4.0E-4, 28 2.0E-4, 29 2.0E-4, 30 5.0E-4, 31 4.0E-4, 32 2.0E-4, 33 5.0E-4, 34 1.0E-4, 35 1.0E-4, 36 1.0E-4, 49 1.0E-4}"}
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
;;; {"type":"html","content":"<span class='clj-unkown'>#&lt;MultiFn clojure.lang.MultiFn@22dc80b4&gt;</span>","value":"#<MultiFn clojure.lang.MultiFn@22dc80b4>"}
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
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:p</span>","value":":p"},{"type":"html","content":"<span class='clj-double'>0.2</span>","value":"0.2"}],"value":"[:p 0.2]"}],"value":"(geometric/geometric 0.2)"}
;; <=

;; @@

;; @@
