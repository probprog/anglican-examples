;; gorilla-repl.fileformat = 1

;; @@
(ns indian-gpa
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
(def american-gpa (fn [] (if (sample (flip 0.9)) 
                    (* 4 (sample (beta 7 3))) 
                    (* 4 (if (flip 0.8) 1.0 0.0)))))

(def indian-gpa (fn [] (if (sample (flip 0.99)) 
                    (* 10 (sample (beta 5 5))) 
                    (* 10 (if (flip 0.5) 1.0 0.0)))))

(defn student-gpa [] (if (sample (flip 0.5))
                             (american-gpa)
                             (indian-gpa)))

(defn dirac
  "Dirac distribution"
  [x]
    (reify
      distribution
      (sample [this] x)
      (observe [this value] (if (= x value) 0.0 (- (/ 1.0 0.0))))))

(with-primitive-procedures [student-gpa american-gpa indian-gpa dirac] 
    (defquery which-nationality-with-dirac [observed-gpa]
    (let [nationality (sample (categorical [["USA" 0.6] ["India" 0.4] ]))  
          student_gpa (if (= nationality "USA") 
                          (american-gpa)
                          (indian-gpa))]
          (observe (dirac student_gpa) observed-gpa)
          (predict :nationality nationality))))
(def N 1000)
(def sampler (doquery :smc which-nationality-with-dirac [4.0] :number-of-particles 1000))
(def samples (drop 100 (take (+ N 100) sampler)))
(def num-usa (count (filter #(= % "USA") (map :nationality (map get-predicts samples)))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;indian-gpa/num-usa</span>","value":"#'indian-gpa/num-usa"}
;; <=

;; @@
["USA" (float (/ num-usa N)) "India" (float (/ (- N num-usa) N)) "N" N]
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;USA&quot;</span>","value":"\"USA\""},{"type":"html","content":"<span class='clj-unkown'>1.0</span>","value":"1.0"},{"type":"html","content":"<span class='clj-string'>&quot;India&quot;</span>","value":"\"India\""},{"type":"html","content":"<span class='clj-unkown'>0.0</span>","value":"0.0"},{"type":"html","content":"<span class='clj-string'>&quot;N&quot;</span>","value":"\"N\""},{"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"}],"value":"[\"USA\" 1.0 \"India\" 0.0 \"N\" 1000]"}
;; <=
