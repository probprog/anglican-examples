;; gorilla-repl.fileformat = 1

;; **
;;; # Funny Distributions
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns funnydistributions
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
(def infinite-loop #(loop [] (recur)))
(defn funny-dist [] (if (sample (flip 0.5))
                      1 
                      (if (sample (flip 0.5))
                        (funny-dist)
                        (infinite-loop))))

(with-primitive-procedures [funny-dist infinite-loop] 
  (defquery induced-dist (predict :x (funny-dist))))
         
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;funnydistributions/induced-dist</span>","value":"#'funnydistributions/induced-dist"}
;; <=

;; @@
(take 4 (doquery :pcascade induced-dist nil :number-of-threads 100))
;; @@

;; @@

;; @@
