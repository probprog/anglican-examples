;; gorilla-repl.fileformat = 1

;; **
;;; # Gorilla REPL
;;; 
;;; Welcome to gorilla :-)
;;; 
;;; Shift + enter evaluates code. Hit ctrl+g twice in quick succession or click the menu icon (upper-right corner) for more commands ...
;;; 
;;; It's a good habit to run each worksheet in its own namespace: feel free to use the declaration we've provided below if you'd like.
;; **

;; @@
(ns complexityreduction
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
;(def s (sample (gamma 1 1)))
;(def d (DISTRIBUTION [arg1 & args] (let [a (sample (normal 5 s))]
;                    (observe (normal a 2) arg1)
;                    (predict [a (* a a) 'rt])
;                ) ))
;(sample (d))
;(sample (d 5.7))
;(observe (d 5.7) 10) ; only if hongseok makes this happen/enumerate query going...
;(support (d )
         
(def s (sample (gamma 1 1)))
; conditioning via fn wrapping
(def d (fn [s] (DISTRIBUTION 
                 (let [a (SAMPLE (normal 5 s))]
                    (OBSERVE (normal a 2) 67)
                    {:a (exp a)
                     :b [a (* a a) 'rt]}))))
               
;; Type: () -> List (Entry Keyword (Real|Real*Real*Symbol))        
(doseq [s data] ((def d (DISTRIBUTION)
                 (SAMPLE d))))
(SAMPLE (d 5.7))
(OBSERVE (d 5.7) 10) ; only if hongseok makes this happen/enumerate query going...
(support (d ))

(repeatedly #(SAMPLE (d 5.7)))

         
;; @@
