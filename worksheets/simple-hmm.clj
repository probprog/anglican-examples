;; gorilla-repl.fileformat = 1

;; **
;;; # Simple HMM Model
;;; In this tutorial we will create a simple hidden Markov model (HMM) and demonstrate the use of the mem construct. First to import the necessary libraries,
;; **

;; @@
(ns pencil-factory
  (:require [gorilla-plot.core :as plot]
            [clojure.core.matrix :as m])
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
;;; Now we will import the initial distribution and transition matrix from text files,
;; **

;; @@
(def initial-state-dist (into [] (map read-string (split (slurp "worksheets/simple-hmm-init.txt") #"\s+"))))
(def count-states (count initial-state-dist))
(def transition-matrix (m/reshape (into [] (map read-string (split (slurp "worksheets/simple-hmm-transitions.txt") #"\s+"))) [3 3]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pencil-factory/transition-matrix</span>","value":"#'pencil-factory/transition-matrix"}
;; <=

;; **
;;; It's useful to be able to import parameters like this from external data rather than have them hardcoded.
;;; 
;;; Now to the the model for the initial state and the transition model.
;;; 
;;; Now to define the emission model. We will give it as a function taking the value of its parent latent variable, thus providing an important layer of abstraction. That is, our query will be able to take any arbitrary emission model.
;; **

;; @@
(def sd 1)	; standard deviation of the emission model
(def means [-1 1 0])	; means of the emission model for different states

(defm emission-model 
  [s]
  (normal (get means s) sd))

(defm initial-model
  []
  (discrete initial-state-dist))

(defm transition-model
  [s]
  (discrete (get transition-matrix s)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pencil-factory/transition-model</span>","value":"#'pencil-factory/transition-model"}
;; <=

;; **
;;; Note that we define the model with defm rather than defn. This is a special m! construct that allows definition of functions that work inside of defquery without the with-primitive-procedures wrapper. Running the query,
;; **

;; @@
(def observations [[0.3 -1.5 1 0.2] [0.9 1.5 0 0.3]])

(defquery simple-hmm
  [initial-model transition-model emission-model observations]
  "a simple hidden markov model"
  (let [initial-state (sample (initial-model))
        next-state (fn [prev-state] (sample (transition-model prev-state)))
        get-state (mem (fn get-state [state] (if (<= state 0) initial-state (next-state (get-state (- state 1))))))
        count-obs (count observations)	; Number of observations for each state
        count-states (count (first observations)) ; Number of latent state variables
        state-idx (range 0 count-states)
        states (map get-state state-idx)]	
    (map (fn [obs] (map (fn [x y] (observe (emission-model y) x)) obs states)) observations) ; Observe multiple samples
    (predict :s states)
    ))

(take 3 (map :s (map get-predicts (doquery :smc simple-hmm [initial-model transition-model emission-model observations] :number-of-particles 1000))))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"(2 2 2 2)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"(2 2 2 2)"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"},{"type":"html","content":"<span class='clj-long'>2</span>","value":"2"}],"value":"(2 2 2 2)"}],"value":"((2 2 2 2) (2 2 2 2) (2 2 2 2))"}
;; <=
