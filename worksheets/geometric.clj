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
(defn geometric 
  "generates geometrically distributed r.v.'s in {0,1,2,...}"
  ([p] (geometric p 0))
  ([p n] (if (sample (flip p)) 
           n 
           (geometric p (+ n 1)))))

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


(def geometric1dist (estimate-empirical-discrete-dist-from-generator (partial geometric 0.2) 10000))       

(def geometric2dist (estimate-empirical-discrete-dist-from-generator (partial geometric2 0.2) 10000)) 

(plot/compose (plot/list-plot geometric1dist) (plot/list-plot geometric2dist))
geometric2dist
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-unkown'>0.2018</span>","value":"0.2018"}],"value":"[0 0.2018]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-unkown'>0.1582</span>","value":"0.1582"}],"value":"[1 0.1582]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-unkown'>0.1246</span>","value":"0.1246"}],"value":"[2 0.1246]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>3</span>","value":"3"},{"type":"html","content":"<span class='clj-unkown'>0.1049</span>","value":"0.1049"}],"value":"[3 0.1049]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>4</span>","value":"4"},{"type":"html","content":"<span class='clj-unkown'>0.0807</span>","value":"0.0807"}],"value":"[4 0.0807]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>5</span>","value":"5"},{"type":"html","content":"<span class='clj-unkown'>0.0667</span>","value":"0.0667"}],"value":"[5 0.0667]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>6</span>","value":"6"},{"type":"html","content":"<span class='clj-unkown'>0.0532</span>","value":"0.0532"}],"value":"[6 0.0532]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>7</span>","value":"7"},{"type":"html","content":"<span class='clj-unkown'>0.0401</span>","value":"0.0401"}],"value":"[7 0.0401]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>8</span>","value":"8"},{"type":"html","content":"<span class='clj-unkown'>0.0322</span>","value":"0.0322"}],"value":"[8 0.0322]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>9</span>","value":"9"},{"type":"html","content":"<span class='clj-unkown'>0.029</span>","value":"0.029"}],"value":"[9 0.029]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>10</span>","value":"10"},{"type":"html","content":"<span class='clj-unkown'>0.0241</span>","value":"0.0241"}],"value":"[10 0.0241]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>11</span>","value":"11"},{"type":"html","content":"<span class='clj-unkown'>0.0176</span>","value":"0.0176"}],"value":"[11 0.0176]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>12</span>","value":"12"},{"type":"html","content":"<span class='clj-unkown'>0.0117</span>","value":"0.0117"}],"value":"[12 0.0117]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>13</span>","value":"13"},{"type":"html","content":"<span class='clj-unkown'>0.0095</span>","value":"0.0095"}],"value":"[13 0.0095]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>14</span>","value":"14"},{"type":"html","content":"<span class='clj-unkown'>0.0096</span>","value":"0.0096"}],"value":"[14 0.0096]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>15</span>","value":"15"},{"type":"html","content":"<span class='clj-unkown'>0.0085</span>","value":"0.0085"}],"value":"[15 0.0085]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>16</span>","value":"16"},{"type":"html","content":"<span class='clj-unkown'>0.0056</span>","value":"0.0056"}],"value":"[16 0.0056]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>17</span>","value":"17"},{"type":"html","content":"<span class='clj-unkown'>0.0044</span>","value":"0.0044"}],"value":"[17 0.0044]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>18</span>","value":"18"},{"type":"html","content":"<span class='clj-unkown'>0.0026</span>","value":"0.0026"}],"value":"[18 0.0026]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>19</span>","value":"19"},{"type":"html","content":"<span class='clj-unkown'>0.0035</span>","value":"0.0035"}],"value":"[19 0.0035]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>20</span>","value":"20"},{"type":"html","content":"<span class='clj-unkown'>0.0033</span>","value":"0.0033"}],"value":"[20 0.0033]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>21</span>","value":"21"},{"type":"html","content":"<span class='clj-unkown'>0.0013</span>","value":"0.0013"}],"value":"[21 0.0013]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>22</span>","value":"22"},{"type":"html","content":"<span class='clj-unkown'>0.0015</span>","value":"0.0015"}],"value":"[22 0.0015]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>23</span>","value":"23"},{"type":"html","content":"<span class='clj-unkown'>6.0E-4</span>","value":"6.0E-4"}],"value":"[23 6.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>24</span>","value":"24"},{"type":"html","content":"<span class='clj-unkown'>0.0016</span>","value":"0.0016"}],"value":"[24 0.0016]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>25</span>","value":"25"},{"type":"html","content":"<span class='clj-unkown'>7.0E-4</span>","value":"7.0E-4"}],"value":"[25 7.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>26</span>","value":"26"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[26 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>27</span>","value":"27"},{"type":"html","content":"<span class='clj-unkown'>4.0E-4</span>","value":"4.0E-4"}],"value":"[27 4.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>28</span>","value":"28"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[28 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>29</span>","value":"29"},{"type":"html","content":"<span class='clj-unkown'>4.0E-4</span>","value":"4.0E-4"}],"value":"[29 4.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>30</span>","value":"30"},{"type":"html","content":"<span class='clj-unkown'>4.0E-4</span>","value":"4.0E-4"}],"value":"[30 4.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>31</span>","value":"31"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[31 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>32</span>","value":"32"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[32 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>33</span>","value":"33"},{"type":"html","content":"<span class='clj-unkown'>2.0E-4</span>","value":"2.0E-4"}],"value":"[33 2.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>34</span>","value":"34"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[34 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>38</span>","value":"38"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[38 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>39</span>","value":"39"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[39 1.0E-4]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>44</span>","value":"44"},{"type":"html","content":"<span class='clj-unkown'>1.0E-4</span>","value":"1.0E-4"}],"value":"[44 1.0E-4]"}],"value":"{0 0.2018, 1 0.1582, 2 0.1246, 3 0.1049, 4 0.0807, 5 0.0667, 6 0.0532, 7 0.0401, 8 0.0322, 9 0.029, 10 0.0241, 11 0.0176, 12 0.0117, 13 0.0095, 14 0.0096, 15 0.0085, 16 0.0056, 17 0.0044, 18 0.0026, 19 0.0035, 20 0.0033, 21 0.0013, 22 0.0015, 23 6.0E-4, 24 0.0016, 25 7.0E-4, 26 2.0E-4, 27 4.0E-4, 28 2.0E-4, 29 4.0E-4, 30 4.0E-4, 31 1.0E-4, 32 2.0E-4, 33 2.0E-4, 34 1.0E-4, 38 1.0E-4, 39 1.0E-4, 44 1.0E-4}"}
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
