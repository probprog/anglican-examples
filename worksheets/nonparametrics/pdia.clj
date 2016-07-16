;; gorilla-repl.fileformat = 1

;; **
;;; # Probabilistic Deterministic Infinite Automata
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
;;; Now we define a true data generating process -- the even process, a two-state probabilistic deterministic automaton that emits stochastic character sequences consisting of two different characters, here one \a (note that \a is the character a in Clojure), the other \b.  Notably the sequences contain only even subsequences of one of the characters, here \b.
;; **

;; @@
(defn even 
  ([] (even 0)) 
  ([state]
   (if (= state 0)
     (let [ret (if (sample* (flip 0.6)) \a \b)]
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
;;; We can look at what the even process looks like by "simulating" from it some N number of observations, note the number of consecutive \b's in each of the sequences
;; **

;; @@
(reduce str (take 200 (even)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-string'>&quot;bbbbbbbbabbabbaaabbaaabbabbaaaaaabbaaabbabbaaabbbbbbabbaaabbbbbbbbbbabbbbbbbbbbabbaabbabbbbabbbbaaaaaaaaabbabbabbaaaabbabbaaaaabbaabbabbbbabbbbbbaabbabbabbbbabbabbbbbbaaabbabbbbaaabbbbaaabbbbbbabbaaaa&quot;</span>","value":"\"bbbbbbbbabbabbaaabbaaabbabbaaaaaabbaaabbabbaaabbbbbbabbaaabbbbbbbbbbabbbbbbbbbbabbaabbabbbbabbbbaaaaaaaaabbabbabbaaaabbabbaaaaabbaabbabbbbabbbbbbaabbabbabbbbabbabbbbbbaaabbabbbbaaabbbbaaabbbbbbabbaaaa\""}
;; <=

;; **
;;; Since it's a bit difficult to see by eye what's going on in long sequences, let's build up some processing code, mostly to detect whether the number of consecutive b's in sequences emitted by the automata we're going to learn is even. 
;; **

;; @@
(defn fraction-even [data]
  (let [es (->> data
              (partition-by identity)
              (butlast)
              (map #(list (first %) (count %)))
              (filter #(= \b (first %)))
              (map #(= 0 (mod (second %) 2))))
        fs (frequencies es)
        nt (if (not (nil? (get fs true)))   (get fs true) 0)
        nf (if (not (nil? (get fs false))) (get fs false) 0)]
    (if (> (+ nf nt) 0 ) 
      (/ nt (+ nf nt))
      nil)))
(fraction-even (take 100 (even)))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-long'>1</span>","value":"1"}
;; <=

;; **
;;; Given a data source (the even process in this case), we can try to do fully unsupervised automata structure learning.  The way this model "reports" on the inferred PDFA is by generating output from it starting at its start state which is known by construction.  By examining this predicted output and, in particular, it's statistical similarity to the input data sequence we can assess how well learning has worked.
;;; 
;;; We first frame the query.
;; **

;; @@
(with-primitive-procedures [join even]
  (defquery pdia N 
    "pdia: from training sequence of length N 
     learn a distribution over pdfa's"
    (let [data (take N (even))
          vocabulary (distinct data)
          model-prior (dirichlet (repeat (count vocabulary) 1.))

          alpha 1.
          top-level (crp alpha)

          alpha0 1.
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

      (join (sample-words 0)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/pdia</span>","value":"#'pdia/pdia"}
;; <=

;; **
;;; And then we perform inference
;; **

;; @@
(def t (->> (doquery :pimh pdia 1000 :number-of-particles 1000)
            (filter #(not (= Double/NEGATIVE_INFINITY (:log-weight %))))
            (take 1000)
            (map :result)
            (filter #(not (nil? (fraction-even %))))
            doall))
            
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/t</span>","value":"#'pdia/t"}
;; <=

;; **
;;; Where, again, we look at the interesting sequences generated by the inferred PDFA.  Interesting here means that the output has both \a's and \b's and isn't the empty string.  What we would like to see is that the learned automata produces strings that have a high fraction of even-length runs of \b's.  This we both can see visually.
;; **

;; @@
(take-nth 20 t)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;aaaaaaabbab&quot;</span>","value":"\"aaaaaaabbab\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbbaaaabbaaabbaaaaabbaaa&quot;</span>","value":"\"abbbbaaaabbaaabbaaaaabbaaa\""},{"type":"html","content":"<span class='clj-string'>&quot;abbabbaabbbbbb&quot;</span>","value":"\"abbabbaabbbbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbbbbbaabbb&quot;</span>","value":"\"bbbbbbbbaabbb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbaaaaabbabbbbabbaa&quot;</span>","value":"\"abbaaaaabbabbbbabbaa\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbaaaabbaaabbbba&quot;</span>","value":"\"bbbbaaaabbaaabbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;aaaabbaa&quot;</span>","value":"\"aaaabbaa\""},{"type":"html","content":"<span class='clj-string'>&quot;abbaabbab&quot;</span>","value":"\"abbaabbab\""},{"type":"html","content":"<span class='clj-string'>&quot;bba&quot;</span>","value":"\"bba\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbba&quot;</span>","value":"\"abbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;abba&quot;</span>","value":"\"abba\""},{"type":"html","content":"<span class='clj-string'>&quot;abbaabbbb&quot;</span>","value":"\"abbaabbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaabbaaaabbbbbbaa&quot;</span>","value":"\"bbaaabbaaaabbbbbbaa\""},{"type":"html","content":"<span class='clj-string'>&quot;abba&quot;</span>","value":"\"abba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbabbbbb&quot;</span>","value":"\"bbabbbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbbbbbbbaaabbab&quot;</span>","value":"\"bbbbbbbbbbaaabbab\""},{"type":"html","content":"<span class='clj-string'>&quot;aaaabbabbabbaabb&quot;</span>","value":"\"aaaabbabbabbaabb\""},{"type":"html","content":"<span class='clj-string'>&quot;aabbaaabbabbbbaaaaabbbb&quot;</span>","value":"\"aabbaaabbabbbbaaaaabbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbaaaaaaabbabbbbbbbbbbabbabba&quot;</span>","value":"\"abbaaaaaaabbabbbbbbbbbbabbabba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbabbaaaaaab&quot;</span>","value":"\"bbbbabbaaaaaab\""},{"type":"html","content":"<span class='clj-string'>&quot;aabbaaabbaaaaabbabbbbbbaabbbbb&quot;</span>","value":"\"aabbaaabbaaaaabbabbbbbbaabbbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbabbaabba&quot;</span>","value":"\"abbabbaabba\""},{"type":"html","content":"<span class='clj-string'>&quot;aabbabbbbaaa&quot;</span>","value":"\"aabbabbbbaaa\""},{"type":"html","content":"<span class='clj-string'>&quot;aabbbbbbbbaaabbbbaabbbbbbaa&quot;</span>","value":"\"aabbbbbbbbaaabbbbaabbbbbbaa\""},{"type":"html","content":"<span class='clj-string'>&quot;bbabbbbb&quot;</span>","value":"\"bbabbbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;aaabbbabb&quot;</span>","value":"\"aaabbbabb\""},{"type":"html","content":"<span class='clj-string'>&quot;aabbbbaaabbbbbbbbaabbaaabba&quot;</span>","value":"\"aabbbbaaabbbbbbbbaabbaaabba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbaaaaaaaab&quot;</span>","value":"\"bbbbaaaaaaaab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaa&quot;</span>","value":"\"bbaa\""}],"value":"(\"aaaaaaabbab\" \"abbbbaaaabbaaabbaaaaabbaaa\" \"abbabbaabbbbbb\" \"bbbbbbbbaabbb\" \"abbaaaaabbabbbbabbaa\" \"bbbbaaaabbaaabbbba\" \"aaaabbaa\" \"abbaabbab\" \"bba\" \"abbbba\" \"abba\" \"abbaabbbb\" \"bbaaabbaaaabbbbbbaa\" \"abba\" \"bbabbbbb\" \"bbbbbbbbbbaaabbab\" \"aaaabbabbabbaabb\" \"aabbaaabbabbbbaaaaabbbb\" \"abbaaaaaaabbabbbbbbbbbbabbabba\" \"bbbbabbaaaaaab\" \"aabbaaabbaaaaabbabbbbbbaabbbbb\" \"abbabbaabba\" \"aabbabbbbaaa\" \"aabbbbbbbbaaabbbbaabbbbbbaa\" \"bbabbbbb\" \"aaabbbabb\" \"aabbbbaaabbbbbbbbaabbaaabba\" \"bbbbaaaaaaaab\" \"bbaa\")"}
;; <=

;; **
;;; And can confirm numerically.
;; **

;; @@
(defn average [coll] 
  (/ (reduce + coll) (count coll)))

(float (average (filter #(not (nil? %)) (map fraction-even t))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>0.9771349</span>","value":"0.9771349"}
;; <=

;; **
;;; Further, we can look at what happens as a function of training input sequence length.  We would expect that as the automata gets a longer training sequence it is capable of understanding the structure of the generative automata better and will generate more statistically similar output.  And, in the single run below this is more-or-less the pattern we see.  
;;; 
;;; Multiple repeats of this experiment would smooth out the "noise" in the plot arising from the high variance of the estimators and the fundamental difficulty of the inference problem.
;; **

;; @@
(def fraction-even-as-function-of-training-data-length 
  (loop [N 2 purities {}]
    (if (> N 800)
      purities
      (let [samples (->> (doquery :pimh pdia N :number-of-particles 1000)
                         (filter #(not (= Double/NEGATIVE_INFINITY (:log-weight %))))
                         (take 1000)
                         (map :result)
                         (filter #(not (nil? (fraction-even %)))))
            fe (if (= 0 (count samples))
                 0
                 (float (average (map fraction-even samples))))]
        (println {:N N 
                  :fraction-even fe 
                  :illustrative-samples (take 5 samples)})
        (recur (int (floor (* N 1.8))) (conj purities {N fe}))))))
;; @@
;; ->
;;; {:N 2, :fraction-even 0, :illustrative-samples ()}
;;; {:N 3, :fraction-even 0, :illustrative-samples ()}
;;; {:N 5, :fraction-even 0.35032853, :illustrative-samples (abbabbbaabaaabb bbabaaaba bbbbbbaabba bbbaabbaba aaaaaabbbbbbaaa)}
;;; {:N 9, :fraction-even 0.41151485, :illustrative-samples (abbbabbbaaaabaaabbabbab babaaab bbbaaaaabababa aaaaababbb bababbababa)}
;;; {:N 16, :fraction-even 0.43661693, :illustrative-samples (aaaabba aabbabbaabbbbabbb abbabbababb bbbab bbaabb)}
;;; {:N 28, :fraction-even 0.37826568, :illustrative-samples (baaaaaaaaaabbbbbbaabb aabbba bbabbbbbba bbaaabbaaabbbbbbbbbb babb)}
;;; {:N 50, :fraction-even 0.46403491, :illustrative-samples (babaaaaaabbbbbbaaaa bbbba bba bbabbabbaaaab bbbbbbbbaaa)}
;;; {:N 90, :fraction-even 0.9214794, :illustrative-samples (aaaaaaaaaaaabbaab bbaaaaaaabbaaaabbaaaab aabbaaa bbabbaa bbbbaaaa)}
;;; {:N 162, :fraction-even 0.9801726, :illustrative-samples (bbaaaa aabbaaaaaaabb aaaaabba abbabb abbaabbaa)}
;;; {:N 291, :fraction-even 0.93708295, :illustrative-samples (bbaaabbbbbbaaaa bbabbaabbabb abbbbaa aaaabbabbb bba)}
;;; {:N 523, :fraction-even 0.96097875, :illustrative-samples (bbabbbbabbbbaaaabbbbbbaaaaabbabbabbaab aaaabbaaaaaaab aabbaa bbbbbbbbbbbbbbbba abba)}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/fraction-even-as-function-of-training-data-length</span>","value":"#'pdia/fraction-even-as-function-of-training-data-length"}
;; <=

;; @@
(plot/list-plot fraction-even-as-function-of-training-data-length 
                :joined false
                :x-title "Length of training sequence"
                :y-title "Fraction even in output")
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"0cc4edef-3b08-4116-bcf1-81b4446c1e39","values":[{"x":291,"y":0.9370829463005066},{"x":50,"y":0.46403491497039795},{"x":523,"y":0.9609787464141846},{"x":90,"y":0.9214794039726257},{"x":162,"y":0.9801725745201111},{"x":28,"y":0.3782656788825989},{"x":3,"y":0},{"x":2,"y":0},{"x":9,"y":0.41151484847068787},{"x":5,"y":0.3503285348415375},{"x":16,"y":0.4366169273853302}]}],"marks":[{"type":"symbol","from":{"data":"0cc4edef-3b08-4116-bcf1-81b4446c1e39"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"0cc4edef-3b08-4116-bcf1-81b4446c1e39","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"0cc4edef-3b08-4116-bcf1-81b4446c1e39","field":"data.y"}}],"axes":[{"type":"x","scale":"x","title":"Length of training sequence","titleOffset":30},{"type":"y","scale":"y","title":"Fraction even in output","titleOffset":45}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"0cc4edef-3b08-4116-bcf1-81b4446c1e39\", :values ({:x 291, :y 0.93708295} {:x 50, :y 0.46403491} {:x 523, :y 0.96097875} {:x 90, :y 0.9214794} {:x 162, :y 0.9801726} {:x 28, :y 0.37826568} {:x 3, :y 0} {:x 2, :y 0} {:x 9, :y 0.41151485} {:x 5, :y 0.35032853} {:x 16, :y 0.43661693})}], :marks [{:type \"symbol\", :from {:data \"0cc4edef-3b08-4116-bcf1-81b4446c1e39\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"0cc4edef-3b08-4116-bcf1-81b4446c1e39\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"0cc4edef-3b08-4116-bcf1-81b4446c1e39\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\", :title \"Length of training sequence\", :titleOffset 30} {:type \"y\", :scale \"y\", :title \"Fraction even in output\", :titleOffset 45}]}}"}
;; <=

;; @@

;; @@
