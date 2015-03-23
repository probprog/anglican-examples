;; gorilla-repl.fileformat = 1

;; **
;;; # Probabilistic Deterministic Infinite Automata
;;; 
;;; Shift + enter evaluates code. Click the menu icon for more commands ...
;;; 
;;; First we set up Anglican and Clojure namespaces.  We also use Gorilla plotting.
;; **

;; @@
(ns pdia
  (:require [gorilla-plot.core :as plot])
  (:use [mrepl core]
        [embang runtime emit]
        [anglib crp]
        [clojure.string :only (join)]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Now we define the class even process, a two-state probabilistic deterministic automaton that emits stochastic character sequences consisting of two different characters, here one \a (note that \a is the character a in Clojure), the other \b.  Notably the sequences contain only even subsequences of one of the characters, here \b.
;; **

;; @@
(defn even 
  ([] (even 0)) 
  ([state]
   (if (= state 0)
     (let [ret (if (sample (flip 0.4)) \a \b)]
       (if (= ret \a)
             ; if in 0 and a is emitted then stay in 0
             (cons \a (lazy-seq (even 0)))    
             ; otherwise switch states 
          	 (cons \b (lazy-seq (even 1)))))

     (do
       ; if in 1 emit a 
       (cons \b (lazy-seq (even 0)))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/even</span>","value":"#'pdia/even"}
;; <=

;; **
;;; We can look at what the even process looks like by "simulating" from it some N number of observations, note the number of \b's in sequence
;; **

;; @@
(take 100 (even))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\b</span>","value":"\\b"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"},{"type":"html","content":"<span class='clj-char'>\\a</span>","value":"\\a"}],"value":"(\\b \\b \\a \\b \\b \\b \\b \\a \\b \\b \\b \\b \\a \\a \\b \\b \\b \\b \\a \\a \\b \\b \\b \\b \\b \\b \\a \\b \\b \\a \\a \\a \\b \\b \\b \\b \\b \\b \\b \\b \\b \\b \\b \\b \\b \\b \\a \\b \\b \\b \\b \\a \\b \\b \\b \\b \\a \\b \\b \\b \\b \\b \\b \\a \\a \\b \\b \\b \\b \\b \\b \\a \\a \\b \\b \\a \\a \\a \\b \\b \\b \\b \\b \\b \\a \\b \\b \\b \\b \\a \\b \\b \\b \\b \\a \\a \\b \\b \\a \\a)"}
;; <=

;; **
;;; 
;; **

;; **
;;; Since it's a bit difficult to see by eye in long sequences, let's build up some processing code, mostly to detect whether the number of consecutive b's in sequences emitted by the automata we're going to learn is even. We'll use these functions again later.
;; **

;; @@
(def run-length-counts
  (map #(list (first %) (count %)) 
        (partition-by identity (even))))
(def b-runs (filter #(= \b (first %)) run-length-counts))
; (def even-b-runs (map #(mod (second %) 2) (butlast b-runs)))
; don't count the last one because it could get cut off by 
;(def fraction-odd (/ (reduce + even-b-runs) (count (butlast b-runs))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/b-runs</span>","value":"#'pdia/b-runs"}
;; <=

;; **
;;; Now that we have a data source (even), we can try to learn the structure of the automata.
;; **

;; @@
(with-primitive-procedures [even join]
  (defquery pdia data-source
    "Probabilistic Deterministic Infinite Automata"
    (let [data (take 100 data-source)
          vocabulary (distinct data)
          model-prior (dirichlet (repeat (count vocabulary) 1.))

          alpha 10.
          top-level (crp alpha)

          alpha0 10.
          symbol->state-generator
          (mem (fn [symbol] (DPmem alpha0 top-level)))

          state-symbol->next-state
          (mem (fn [state symbol] ((symbol->state-generator symbol))))

          state->observation-model
          (mem (fn [state] (sample model-prior)))

          observation-noise 
          (mem (fn [state]
                 (categorical (map vector
                                   vocabulary
                                   (state->observation-model state)))))
          sample-words
          (fn sample-words [state]
            (when (sample* (flip 0.9))
              (let [word (sample* (observation-noise state))]
                (conj
                  (sample-words (state-symbol->next-state state word))
                  word))))]

      (reduce (fn [state symbol]
                (observe (observation-noise state) symbol)
                (state-symbol->next-state state symbol))
              0 data)

      (predict 'words (join (sample-words 0))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/pdia</span>","value":"#'pdia/pdia"}
;; <=

;; @@
(def burn-in 5000)
(def number-of-samples 1000)
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/number-of-samples</span>","value":"#'pdia/number-of-samples"}
;; <=

;; @@
(def words (->> (doquery :lmh pdia (even))
                (drop burn-in)
                (take number-of-samples)
                (map get-predicts)
                (map #(get % 'words))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/words</span>","value":"#'pdia/words"}
;; <=

;; @@
(defn index-words
  [words]
  (loop [wordidx {}
        i 0
        words words
        indices []]
    (if-let [[word & words] words]
      (if (contains? wordidx word)
        (recur wordidx i words (conj indices (get wordidx word)))
        (recur (assoc wordidx word i) (inc i) words (conj indices i)))
      indices)))
(def indices (index-words words))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/indices</span>","value":"#'pdia/indices"}
;; <=

;; @@
 (plot/histogram indices)
 (take 10 words)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;bbbabb&quot;</span>","value":"\"bbbabb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbbbb&quot;</span>","value":"\"bbbbbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;bba&quot;</span>","value":"\"bba\""},{"type":"html","content":"<span class='clj-string'>&quot;bba&quot;</span>","value":"\"bba\""},{"type":"html","content":"<span class='clj-string'>&quot;ab&quot;</span>","value":"\"ab\""},{"type":"html","content":"<span class='clj-string'>&quot;ab&quot;</span>","value":"\"ab\""},{"type":"html","content":"<span class='clj-string'>&quot;ab&quot;</span>","value":"\"ab\""},{"type":"html","content":"<span class='clj-string'>&quot;ab&quot;</span>","value":"\"ab\""},{"type":"html","content":"<span class='clj-string'>&quot;ab&quot;</span>","value":"\"ab\""},{"type":"html","content":"<span class='clj-string'>&quot;ab&quot;</span>","value":"\"ab\""}],"value":"(\"bbbabb\" \"bbbbbbb\" \"bba\" \"bba\" \"ab\" \"ab\" \"ab\" \"ab\" \"ab\" \"ab\")"}
;; <=
