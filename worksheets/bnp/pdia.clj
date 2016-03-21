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
     (let [ret (if (sample (flip 0.6)) \a \b)]
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
;;; {"type":"html","content":"<span class='clj-string'>&quot;bbbbaaaaaaabbbbbbabbabbabbbbabbaabbbbbbbbbbbbbbabbaaaaaabbabbabbbbaabbbbaaaabbabbbbaaabbaabbbbabbbbaabbaaaabbabbbbbbbbaaaaaaaaaabbabbaaabbaabbaaaaabbaaabbbbbbbbbbbbaaabbabbaabbaaaaabbbbabbbbbbaaaaabbb&quot;</span>","value":"\"bbbbaaaaaaabbbbbbabbabbabbbbabbaabbbbbbbbbbbbbbabbaaaaaabbabbabbbbaabbbbaaaabbabbbbaaabbaabbbbabbbbaabbaaaabbabbbbbbbbaaaaaaaaaabbabbaaabbaabbaaaaabbaaabbbbbbbbbbbbaaabbabbaabbaaaaabbbbabbbbbbaaaaabbb\""}
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

      (predict :words (join (sample-words 0))))))
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
            (map get-predicts)
            (take 1000)
            (map :words)
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
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;bbabbbbaaaaaaabbaabbaaabbabbbbbbbbaaabbbbaaa&quot;</span>","value":"\"bbabbbbaaaaaaabbaabbaaabbabbbbbbbbaaabbbbaaa\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaabbaaab&quot;</span>","value":"\"bbaabbaaab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaabbabbbbabbaabbaaaaaaabb&quot;</span>","value":"\"bbaabbabbbbabbaabbaaaaaaabb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbabbbbaaaaaaaaaaaaabbaa&quot;</span>","value":"\"abbabbbbaaaaaaaaaaaaabbaa\""},{"type":"html","content":"<span class='clj-string'>&quot;aaaaabbbbbbaabbaabbabb&quot;</span>","value":"\"aaaaabbbbbbaabbaabbabb\""},{"type":"html","content":"<span class='clj-string'>&quot;aabbabbaabbaaabbbbaa&quot;</span>","value":"\"aabbabbaabbaaabbbbaa\""},{"type":"html","content":"<span class='clj-string'>&quot;bbabb&quot;</span>","value":"\"bbabb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbbba&quot;</span>","value":"\"bbbbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;aabbaa&quot;</span>","value":"\"aabbaa\""},{"type":"html","content":"<span class='clj-string'>&quot;aaaaaaabbabbb&quot;</span>","value":"\"aaaaaaabbabbb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbabbabbaaabbbbabbaaaaabbbbbbabbb&quot;</span>","value":"\"abbabbabbaaabbbbabbaaaaabbbbbbabbb\""},{"type":"html","content":"<span class='clj-string'>&quot;aabbabbaaabbabbabb&quot;</span>","value":"\"aabbabbaaabbabbabb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaaaaaabbbbaaabbaaabbbabbbabbbbaabb&quot;</span>","value":"\"bbaaaaaaabbbbaaabbaaabbbabbbabbbbaabb\""},{"type":"html","content":"<span class='clj-string'>&quot;aabbaaaabbaaaaabbabbbbabbbb&quot;</span>","value":"\"aabbaaaabbaaaaabbabbbbabbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaabb&quot;</span>","value":"\"bbaabb\""},{"type":"html","content":"<span class='clj-string'>&quot;aabbaaaabbaa&quot;</span>","value":"\"aabbaaaabbaa\""},{"type":"html","content":"<span class='clj-string'>&quot;bbabbaaabba&quot;</span>","value":"\"bbabbaaabba\""},{"type":"html","content":"<span class='clj-string'>&quot;aaabbaaaaabb&quot;</span>","value":"\"aaabbaaaaabb\""},{"type":"html","content":"<span class='clj-string'>&quot;aaabbabbabba&quot;</span>","value":"\"aaabbabbabba\""},{"type":"html","content":"<span class='clj-string'>&quot;aaabbabbabbaaabb&quot;</span>","value":"\"aaabbabbabbaaabb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbbbbbbabbaabbaabbabbaaabbbbabba&quot;</span>","value":"\"abbbbbbbbabbaabbaabbabbaaabbbbabba\""},{"type":"html","content":"<span class='clj-string'>&quot;aaaaaaaabbaaaaaaaaabbbbabbbbbbaaaabbbbabbbbb&quot;</span>","value":"\"aaaaaaaabbaaaaaaaaabbbbabbbbbbaaaabbbbabbbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbabbaaabbaabbbbaaaaaaaaaabb&quot;</span>","value":"\"abbabbaaabbaabbbbaaaaaaaaaabb\""},{"type":"html","content":"<span class='clj-string'>&quot;aaabbaabbbb&quot;</span>","value":"\"aaabbaabbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbabbaaaa&quot;</span>","value":"\"bbabbaaaa\""},{"type":"html","content":"<span class='clj-string'>&quot;aabbbbaabbb&quot;</span>","value":"\"aabbbbaabbb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbabbaabbab&quot;</span>","value":"\"abbabbaabbab\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbbaaaaabbbba&quot;</span>","value":"\"abbbbaaaaabbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbab&quot;</span>","value":"\"bbab\""}],"value":"(\"bbabbbbaaaaaaabbaabbaaabbabbbbbbbbaaabbbbaaa\" \"bbaabbaaab\" \"bbaabbabbbbabbaabbaaaaaaabb\" \"abbabbbbaaaaaaaaaaaaabbaa\" \"aaaaabbbbbbaabbaabbabb\" \"aabbabbaabbaaabbbbaa\" \"bbabb\" \"bbbbbba\" \"aabbaa\" \"aaaaaaabbabbb\" \"abbabbabbaaabbbbabbaaaaabbbbbbabbb\" \"aabbabbaaabbabbabb\" \"bbaaaaaaabbbbaaabbaaabbbabbbabbbbaabb\" \"aabbaaaabbaaaaabbabbbbabbbb\" \"bbaabb\" \"aabbaaaabbaa\" \"bbabbaaabba\" \"aaabbaaaaabb\" \"aaabbabbabba\" \"aaabbabbabbaaabb\" \"abbbbbbbbabbaabbaabbabbaaabbbbabba\" \"aaaaaaaabbaaaaaaaaabbbbabbbbbbaaaabbbbabbbbb\" \"abbabbaaabbaabbbbaaaaaaaaaabb\" \"aaabbaabbbb\" \"bbabbaaaa\" \"aabbbbaabbb\" \"abbabbaabbab\" \"abbbbaaaaabbbba\" \"bbab\")"}
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
;;; {"type":"html","content":"<span class='clj-unkown'>0.9844499</span>","value":"0.9844499"}
;; <=

;; **
;;; Further, we can look at what happens as a function of training input sequence length.  We would expect that as the automata gets a longer training sequence it is capable of understanding the structure of the generative automata better and will generate more statistically similar output.  And, in the single run below this is more-or-less the pattern we see.  
;;; 
;;; Multiple repeats of this experiment would smooth out the "noise" in the plot arising from the high variance of the estimators and the fundamental difficulty of the inference problem.
;; **

;; @@
(def fraction-even-as-function-of-training-data-length 
  (loop [N 2 purities {}]
    (if (> N 1500)
      purities
      (let [samples (->> (doquery :pimh pdia N :number-of-particles 1000)
                         (filter #(not (= Double/NEGATIVE_INFINITY (:log-weight %))))
                         (map get-predicts)
                         (take 1000)
                         (map :words)
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
;;; {:N 3, :fraction-even 0.3750248, :illustrative-samples (abbbbbabb bbbbbbabbabbaabbabbabbabbaabbabbabbabbbbabbbbabbabbbbabbbbbbabbaa bbabbabbabb bbbbbabbbbbbbbbbbbbb babbbbbabaaabaaababbbaab)}
;;; {:N 5, :fraction-even 0.31982574, :illustrative-samples (abbaabbaabbbbabababbbaabbbaababbbaaabbaaababaab aabab baabbaaabaaabb baabaa bbaaabbb)}
;;; {:N 9, :fraction-even 0.3905417, :illustrative-samples (ba bbbabaabbbabaabbbbba abbabaaabab bbbbbbabbbbb aaaaabaaaaaabbbbbaa)}
;;; {:N 16, :fraction-even 0.46423975, :illustrative-samples (aabbababbbbbbbbbaabbaabbabbbbbbbbbababbaabbbaababb bbaababbbab abbabbaa babaabbbaabbaa bbabab)}
;;; {:N 28, :fraction-even 0.52298564, :illustrative-samples (ababbbabaaaaba bbbbbbbaabb bbbbbaabbbbb aaabbba aaaaaaaaaaaaaaaaaaababbabbbb)}
;;; {:N 50, :fraction-even 0.8436249, :illustrative-samples (aaaaaba aabbaaaaabbaaaaabb bbabbaabbaaaa bbbbaaaaa aabbaaaaaaaaa)}
;;; {:N 90, :fraction-even 0.73328763, :illustrative-samples (bbbbaabbabbab abbabbaa abba baaabbaabbab baabbabbaab)}
;;; {:N 162, :fraction-even 0.63419074, :illustrative-samples (abbbbbbabbbba bbbaabbb bbbbbbaabba ba bbbbba)}
;;; {:N 291, :fraction-even 0.99008685, :illustrative-samples (bbaaaaababbbbaaaaaaaabbaabbbbabbbbabbabbb aaba abbabb abbabbabbbbaaa bbbbaaaaaa)}
;;; {:N 523, :fraction-even 0.99384326, :illustrative-samples (aabbaaaabbaaaaaa abbabbbbbbbbaaab bbbbbbbbbbaaaabb bbaaaabbb aaaabbbbaabbaaaaaaa)}
;;; {:N 941, :fraction-even 0.9932153, :illustrative-samples (aabbaa bbbbbbabbbbbbbbaaabbabbaa abbbbabbabbbbb bba abbaabbbbaab)}
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
;;; {"type":"vega","content":{"axes":[{"titleOffset":30,"title":"Length of training sequence","scale":"x","type":"x"},{"titleOffset":45,"title":"Fraction even in output","scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"eefa83b8-9bdd-4beb-94f3-95f108e38410","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"eefa83b8-9bdd-4beb-94f3-95f108e38410","field":"data.y"}}],"marks":[{"type":"symbol","from":{"data":"eefa83b8-9bdd-4beb-94f3-95f108e38410"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}],"data":[{"name":"eefa83b8-9bdd-4beb-94f3-95f108e38410","values":[{"x":291,"y":0.9900868535041809},{"x":50,"y":0.8436248898506165},{"x":523,"y":0.9938432574272156},{"x":90,"y":0.7332876324653625},{"x":162,"y":0.6341907382011414},{"x":28,"y":0.5229856371879578},{"x":941,"y":0.9932153224945068},{"x":3,"y":0.37502479553222656},{"x":2,"y":0},{"x":9,"y":0.3905417025089264},{"x":5,"y":0.3198257386684418},{"x":16,"y":0.4642397463321686}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:titleOffset 30, :title \"Length of training sequence\", :scale \"x\", :type \"x\"} {:titleOffset 45, :title \"Fraction even in output\", :scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"eefa83b8-9bdd-4beb-94f3-95f108e38410\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"eefa83b8-9bdd-4beb-94f3-95f108e38410\", :field \"data.y\"}}], :marks [{:type \"symbol\", :from {:data \"eefa83b8-9bdd-4beb-94f3-95f108e38410\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}}], :data [{:name \"eefa83b8-9bdd-4beb-94f3-95f108e38410\", :values ({:x 291, :y 0.99008685} {:x 50, :y 0.8436249} {:x 523, :y 0.99384326} {:x 90, :y 0.73328763} {:x 162, :y 0.63419074} {:x 28, :y 0.52298564} {:x 941, :y 0.9932153} {:x 3, :y 0.3750248} {:x 2, :y 0} {:x 9, :y 0.3905417} {:x 5, :y 0.31982574} {:x 16, :y 0.46423975})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"}
;; <=
