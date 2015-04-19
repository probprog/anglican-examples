;; gorilla-repl.fileformat = 1

;; **
;;; # Birthday
;;; 
;;; Shift + enter evaluates code. Click the menu icon for more commands ...
;;; 
;;; First we set up the Anglican and Clojure namespaces.  Also Gorilla plotting.
;; **

;; @@
(ns pdia
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
;;; Answering a question like “Approximately, what’s the probability that in a room filled with 23 people at least one pair of people have the same birthday?” is straightforward. All one needs to do is generate the underlying phenomena and then pose a question that computes the quantity of interest from the generated data.
;;; 
;;; To find out the approximate probability of a pair or more sharing the same birthday we need a model that generates birthdays. A simple and reasonably accurate model encodes the assumption that birthdays are generated independently for all people uniformly over the set of days in a year. The hardest part of answering the question is writing the function to compute the answer to the question of interest.
;; **

;; @@
(defn equal-pair-in-sequence [x] 
  (if (empty? x) false
    (if (not (empty? (filter #(= (first x) %) (rest x))))
      true
      (equal-pair-in-sequence (rest x)))))


(defn duplicates? [xs]
  (loop [[x & xs :as coll] xs, seen #{}]
    (if (empty? coll) false
      (or (contains? seen x) (recur xs (conj seen x))))))


;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/duplicates?</span>","value":"#'pdia/duplicates?"}
;; <=

;; @@
(with-primitive-procedures [equal-pair-in-sequence duplicates?]
  (defquery equal-birthday-in-a-group-of N 
    "birthday: takes number of people in room"
     (let [birthday-distribution (uniform-discrete 1 366)
           birthdays (repeatedly N (fn [] (sample birthday-distribution)))]
           (predict :b birthdays)
           (predict :eq (equal-pair-in-sequence birthdays)))))
                           
          
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/equal-birthday-in-a-group-of</span>","value":"#'pdia/equal-birthday-in-a-group-of"}
;; <=

;; @@
(def N 23)
(def sampler (doquery :smc equal-birthday-in-a-group-of N :number-of-particles 1))
(def t (map #(if % 1 0) (map :eq (map get-predicts sampler))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/t</span>","value":"#'pdia/t"}
;; <=

;; @@
(plot/histogram (take 1000 t))
(def samples (take 1000 (map :eq (map get-predicts sampler))))
(float (/ (count (filter true? samples)) (count samples)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>0.488</span>","value":"0.488"}
;; <=

;; **
;;; Looks like around N = 23 the probability of finding two people with exactly the same birthday is arond 50/50.  Just for the fun of it let's use inference machinery to ask what number of people would be needed to make this probability 90%.  
;; **

;; @@
(with-primitive-procedures [doquery get-predicts]
  (defquery num-people-in-room-needed-to-have-X-percent-chance-of-overlapping-birthday X 
    "num-people-in-room-needed-to-have-X-percent-chance-of-overlapping-birthday: "
     (let [num-people-in-room (sample (uniform-discrete 23 50))
           outcomes (doquery :smc equal-birthday-in-a-group-of num-people-in-room)
           predicts (map get-predicts (take 1000 outcomes))
		   probability (float (/ (count (filter true? predicts)) (count predicts)))]
       (observe (beta (* 1000 X) (* 1000 (- 1 X))) probability)
       (predict :N num-people-in-room))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/num-people-in-room-needed-to-have-X-percent-chance-of-overlapping-birthday</span>","value":"#'pdia/num-people-in-room-needed-to-have-X-percent-chance-of-overlapping-birthday"}
;; <=

;; @@
(def sampler2 (doquery :almh num-people-in-room-needed-to-have-X-percent-chance-of-overlapping-birthday 0.9))
(def u (map :N (map get-predicts sampler2)))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/u</span>","value":"#'pdia/u"}
;; <=

;; @@
(time (plot/histogram (take 5000 u)))
;; @@

;; **
;;; Visually it would appear that this occurs around 38-40 people.  We can algorithmically find the mode for potential subsequent use:
;; **

;; @@
(def bucket-size 1)
(->> u
     (take 5000)
     (group-by #(quot % bucket-size))
     (sort-by (comp - count second))
     ffirst
     (* bucket-size))

;; @@

;; @@

;; @@
