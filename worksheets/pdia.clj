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
;;; {"type":"html","content":"<span class='clj-string'>&quot;bbbbabbbbbbbbabbbbabbabbbbabbbbbbaaabbabbbbbbaabbaaabbaabbabbbbaaabbaaaaaaaaabbbbabbabbaabbaaaaabbaabbbbbbaaaabbbbaaabbaaaaaaaabbabbbbbbabbaaaabbbbbbaabbabbaaaaaaaaabbaabbbbabbaabbaaaabbbbabbabbbbaabb&quot;</span>","value":"\"bbbbabbbbbbbbabbbbabbabbbbabbbbbbaaabbabbbbbbaabbaaabbaabbabbbbaaabbaaaaaaaaabbbbabbabbaabbaaaaabbaabbbbbbaaaabbbbaaabbaaaaaaaabbabbbbbbabbaaaabbbbbbaabbabbaaaaaaaaabbaabbbbabbaabbaaaabbbbabbabbbbaabb\""}
;; <=

;; **
;;; 
;; **

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
(def t (->> (doquery :pimh pdia 1000 :number-of-particles 20)
            (filter #(not (= Double/NEGATIVE_INFINITY 
                             (:embang.state/log-weight %))))
            (map get-predicts)
            (take 2000)
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
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;ababbabbbbabbbb&quot;</span>","value":"\"ababbabbbbabbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;aabbbbab&quot;</span>","value":"\"aabbbbab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaa&quot;</span>","value":"\"bbaa\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbaaaaaaabbbbbba&quot;</span>","value":"\"bbbbaaaaaaabbbbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;abbabbabb&quot;</span>","value":"\"abbabbabb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbabbbbbba&quot;</span>","value":"\"bbabbbbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaaabbbbaaabba&quot;</span>","value":"\"bbaaaabbbbaaabba\""},{"type":"html","content":"<span class='clj-string'>&quot;bba&quot;</span>","value":"\"bba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbaabb&quot;</span>","value":"\"bbbbaabb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbbabbaabbbbaaaaabbabbbbbbabbbbb&quot;</span>","value":"\"abbbbabbaabbbbaaaaabbabbbbbbabbbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbbbbabb&quot;</span>","value":"\"abbbbbbabb\""},{"type":"html","content":"<span class='clj-string'>&quot;abaaabbbbbbaabbaabbbbaa&quot;</span>","value":"\"abaaabbbbbbaabbaabbbbaa\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaaabbaaabbbbabbaabbaab&quot;</span>","value":"\"bbaaaabbaaabbbbabbaabbaab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbabbaabbaaabbbb&quot;</span>","value":"\"bbabbaabbaaabbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaabbbbbbbbbb&quot;</span>","value":"\"bbaaabbbbbbbbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbabbabb&quot;</span>","value":"\"abbabbabb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbabbbbbba&quot;</span>","value":"\"bbabbbbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaaabbbbaaabba&quot;</span>","value":"\"bbaaaabbbbaaabba\""},{"type":"html","content":"<span class='clj-string'>&quot;bba&quot;</span>","value":"\"bba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbaabb&quot;</span>","value":"\"bbbbaabb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbbabbaabbbbaaaaabbabbbbbbabbbbb&quot;</span>","value":"\"abbbbabbaabbbbaaaaabbabbbbbbabbbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbbbbabb&quot;</span>","value":"\"abbbbbbabb\""},{"type":"html","content":"<span class='clj-string'>&quot;abaaabbbbbbaabbaabbbbaa&quot;</span>","value":"\"abaaabbbbbbaabbaabbbbaa\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaaabbaaabbbbabbaabbaab&quot;</span>","value":"\"bbaaaabbaaabbbbabbaabbaab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbabbaabbaaabbbb&quot;</span>","value":"\"bbabbaabbaaabbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaabbbbbbbbbb&quot;</span>","value":"\"bbaaabbbbbbbbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbabbabb&quot;</span>","value":"\"abbabbabb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbabbbbbba&quot;</span>","value":"\"bbabbbbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaaabbbbaaabba&quot;</span>","value":"\"bbaaaabbbbaaabba\""},{"type":"html","content":"<span class='clj-string'>&quot;bba&quot;</span>","value":"\"bba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbaabb&quot;</span>","value":"\"bbbbaabb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbbabbaabbbbaaaaabbabbbbbbabbbbb&quot;</span>","value":"\"abbbbabbaabbbbaaaaabbabbbbbbabbbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbbbbabb&quot;</span>","value":"\"abbbbbbabb\""},{"type":"html","content":"<span class='clj-string'>&quot;abaaabbbbbbaabbaabbbbaa&quot;</span>","value":"\"abaaabbbbbbaabbaabbbbaa\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaaabbaaabbbbabbaabbaab&quot;</span>","value":"\"bbaaaabbaaabbbbabbaabbaab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbabbaabbaaabbbb&quot;</span>","value":"\"bbabbaabbaaabbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaabbbbbbbbbb&quot;</span>","value":"\"bbaaabbbbbbbbbb\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbba&quot;</span>","value":"\"abbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaab&quot;</span>","value":"\"bbaab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaabbabbb&quot;</span>","value":"\"bbaaabbabbb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbbbaabbaabbbbaabbabbaabbbbabbbbbbab&quot;</span>","value":"\"bbbbbbaabbaabbbbaabbabbaabbbbabbbbbbab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbba&quot;</span>","value":"\"bbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;abbaabbaab&quot;</span>","value":"\"abbaabbaab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaabbabbaabbaaaaabbaabbabba&quot;</span>","value":"\"bbaabbabbaabbaaaaabbaabbabba\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbba&quot;</span>","value":"\"abbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaab&quot;</span>","value":"\"bbaab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaabbabbb&quot;</span>","value":"\"bbaaabbabbb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbbbaabbaabbbbaabbabbaabbbbabbbbbbab&quot;</span>","value":"\"bbbbbbaabbaabbbbaabbabbaabbbbabbbbbbab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbba&quot;</span>","value":"\"bbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;abbaabbaab&quot;</span>","value":"\"abbaabbaab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaabbabbaabbaaaaabbaabbabba&quot;</span>","value":"\"bbaabbabbaabbaaaaabbaabbabba\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbba&quot;</span>","value":"\"abbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaab&quot;</span>","value":"\"bbaab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaabbabbb&quot;</span>","value":"\"bbaaabbabbb\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbbbbaabbaabbbbaabbabbaabbbbabbbbbbab&quot;</span>","value":"\"bbbbbbaabbaabbbbaabbabbaabbbbabbbbbbab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbbba&quot;</span>","value":"\"bbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;abbaabbaab&quot;</span>","value":"\"abbaabbaab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaabbabbaabbaaaaabbaabbabba&quot;</span>","value":"\"bbaabbabbaabbaaaaabbaabbabba\""},{"type":"html","content":"<span class='clj-string'>&quot;abbbba&quot;</span>","value":"\"abbbba\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaab&quot;</span>","value":"\"bbaab\""},{"type":"html","content":"<span class='clj-string'>&quot;bbaaabbabbb&quot;</span>","value":"\"bbaaabbabbb\""}],"value":"(\"ababbabbbbabbbb\" \"aabbbbab\" \"bbaa\" \"bbbbaaaaaaabbbbbba\" \"abbabbabb\" \"bbabbbbbba\" \"bbaaaabbbbaaabba\" \"bba\" \"bbbbaabb\" \"abbbbabbaabbbbaaaaabbabbbbbbabbbbb\" \"abbbbbbabb\" \"abaaabbbbbbaabbaabbbbaa\" \"bbaaaabbaaabbbbabbaabbaab\" \"bbabbaabbaaabbbb\" \"bbaaabbbbbbbbbb\" \"abbabbabb\" \"bbabbbbbba\" \"bbaaaabbbbaaabba\" \"bba\" \"bbbbaabb\" \"abbbbabbaabbbbaaaaabbabbbbbbabbbbb\" \"abbbbbbabb\" \"abaaabbbbbbaabbaabbbbaa\" \"bbaaaabbaaabbbbabbaabbaab\" \"bbabbaabbaaabbbb\" \"bbaaabbbbbbbbbb\" \"abbabbabb\" \"bbabbbbbba\" \"bbaaaabbbbaaabba\" \"bba\" \"bbbbaabb\" \"abbbbabbaabbbbaaaaabbabbbbbbabbbbb\" \"abbbbbbabb\" \"abaaabbbbbbaabbaabbbbaa\" \"bbaaaabbaaabbbbabbaabbaab\" \"bbabbaabbaaabbbb\" \"bbaaabbbbbbbbbb\" \"abbbba\" \"bbaab\" \"bbaaabbabbb\" \"bbbbbbaabbaabbbbaabbabbaabbbbabbbbbbab\" \"bbbba\" \"abbaabbaab\" \"bbaabbabbaabbaaaaabbaabbabba\" \"abbbba\" \"bbaab\" \"bbaaabbabbb\" \"bbbbbbaabbaabbbbaabbabbaabbbbabbbbbbab\" \"bbbba\" \"abbaabbaab\" \"bbaabbabbaabbaaaaabbaabbabba\" \"abbbba\" \"bbaab\" \"bbaaabbabbb\" \"bbbbbbaabbaabbbbaabbabbaabbbbabbbbbbab\" \"bbbba\" \"abbaabbaab\" \"bbaabbabbaabbaaaaabbaabbabba\" \"abbbba\" \"bbaab\" \"bbaaabbabbb\")"}
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
;;; {"type":"html","content":"<span class='clj-unkown'>0.97359395</span>","value":"0.97359395"}
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
    (let [samples (->> (doquery :pimh pdia N :number-of-particles 50)
                       (filter #(not (= Double/NEGATIVE_INFINITY 
                         (:embang.state/log-weight %))))
                       (map get-predicts)
                       (take 3000)
                       (map :words)
                       (filter #(not (nil? (fraction-even %)))))
          fe (if (= 0 (count samples))
                0
                (float (average (map fraction-even samples))))
          _ (prn {:N N :fraction-even fe :illustrative-samples (take 5 samples)})]
       (recur (int (floor (* N 1.8))) (conj purities {N fe}))))))
;; @@
;; ->
;;; {:N 2, :fraction-even 0, :illustrative-samples ()}
;;; {:N 3, :fraction-even 0.3393704, :illustrative-samples (&quot;aaaaababaababaababaaaa&quot; &quot;aba&quot; &quot;bbbabbaaab&quot; &quot;abaabaaaaa&quot; &quot;abbabbabbbbbbabaabbbbbbabbbbaaa&quot;)}
;;; {:N 5, :fraction-even 0.38808852, :illustrative-samples (&quot;aaabaaabaaababbbabbbbbabbaaaaabaa&quot; &quot;bbbbabbababbbbabaababbbbbabaaabababbbbbbbbbbba&quot; &quot;bbaaabaaaaabbabaaaaab&quot; &quot;abbbbbbbbbbabbbbbbababbbbbb&quot; &quot;bbbbbabbbabbbbb&quot;)}
;;; {:N 9, :fraction-even 0.3919594, :illustrative-samples (&quot;aaabbabbbbaaaaaaaaa&quot; &quot;babbabbbbbbbb&quot; &quot;bbbabaabbabbbbbaabb&quot; &quot;abbabbbabbabababb&quot; &quot;abbbabbbb&quot;)}
;;; {:N 16, :fraction-even 0.30409732, :illustrative-samples (&quot;bbbabbbbaaaa&quot; &quot;baabaaaa&quot; &quot;bbbabbaba&quot; &quot;aabaababbbbaaba&quot; &quot;abbbbbaabbabaaabbababbbb&quot;)}
;;; {:N 28, :fraction-even 0.52499074, :illustrative-samples (&quot;bbababbbb&quot; &quot;aabbabababbbbabababba&quot; &quot;babbbbbbbbbaaababbbbbbbb&quot; &quot;aabbbbababbbbbbbabbbab&quot; &quot;bbbaababbabbaabaabbbbbbaababab&quot;)}
;;; {:N 50, :fraction-even 0.94503117, :illustrative-samples (&quot;ba&quot; &quot;bbabaabbbaaaaababbbbb&quot; &quot;bbba&quot; &quot;babaabbaabbaaaab&quot; &quot;aabbbbbabbb&quot;)}
;;; {:N 90, :fraction-even 0.5532275, :illustrative-samples (&quot;bbbbaaaababbaabbbbababbabbab&quot; &quot;bbbbbbbbabaabbbbbbabb&quot; &quot;bbbbbab&quot; &quot;bbbbbbaaaaabaaab&quot; &quot;aabba&quot;)}
;;; {:N 162, :fraction-even 0.73575795, :illustrative-samples (&quot;ba&quot; &quot;babb&quot; &quot;aabbaaabbabbbbbabb&quot; &quot;aaaaabaaaa&quot; &quot;aabaabbaaabbbaa&quot;)}
;;; {:N 291, :fraction-even 0.8624851, :illustrative-samples (&quot;bbaaabbaaa&quot; &quot;bbaab&quot; &quot;aaaaabbbbaaaa&quot; &quot;aaaaaabbaaaa&quot; &quot;bbbbaaaab&quot;)}
;;; {:N 523, :fraction-even 0.66175246, :illustrative-samples (&quot;bbbbab&quot; &quot;baab&quot; &quot;bbaabbaaaaabbb&quot; &quot;baaaaaaa&quot; &quot;bbaaabaabaaaaabaa&quot;)}
;;; {:N 941, :fraction-even 0.938066, :illustrative-samples (&quot;aba&quot; &quot;abbaabbabb&quot; &quot;bbaaabaab&quot; &quot;aabbbbabbaab&quot; &quot;bbabaaaabbbbbb&quot;)}
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pdia/fraction-even-as-function-of-training-data-length</span>","value":"#'pdia/fraction-even-as-function-of-training-data-length"}
;; <=

;; @@
(plot/list-plot fraction-even-as-function-of-training-data-length :joined false)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"dc04c350-859b-4ced-bc0c-6ae221edae3e","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"dc04c350-859b-4ced-bc0c-6ae221edae3e","field":"data.y"}}],"marks":[{"type":"symbol","from":{"data":"dc04c350-859b-4ced-bc0c-6ae221edae3e"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"fill":{"value":"steelblue"},"fillOpacity":{"value":1}},"update":{"shape":"circle","size":{"value":70},"stroke":{"value":"transparent"}},"hover":{"size":{"value":210},"stroke":{"value":"white"}}}}],"data":[{"name":"dc04c350-859b-4ced-bc0c-6ae221edae3e","values":[{"x":291,"y":0.8624851107597351},{"x":50,"y":0.9450311660766602},{"x":523,"y":0.661752462387085},{"x":90,"y":0.5532274842262268},{"x":162,"y":0.7357579469680786},{"x":28,"y":0.5249907374382019},{"x":941,"y":0.9380660057067871},{"x":3,"y":0.33937039971351624},{"x":2,"y":0},{"x":9,"y":0.39195939898490906},{"x":5,"y":0.38808852434158325},{"x":16,"y":0.30409732460975647}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"dc04c350-859b-4ced-bc0c-6ae221edae3e\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"dc04c350-859b-4ced-bc0c-6ae221edae3e\", :field \"data.y\"}}], :marks [{:type \"symbol\", :from {:data \"dc04c350-859b-4ced-bc0c-6ae221edae3e\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 1}}, :update {:shape \"circle\", :size {:value 70}, :stroke {:value \"transparent\"}}, :hover {:size {:value 210}, :stroke {:value \"white\"}}}}], :data [{:name \"dc04c350-859b-4ced-bc0c-6ae221edae3e\", :values ({:x 291, :y 0.8624851} {:x 50, :y 0.94503117} {:x 523, :y 0.66175246} {:x 90, :y 0.5532275} {:x 162, :y 0.73575795} {:x 28, :y 0.52499074} {:x 941, :y 0.938066} {:x 3, :y 0.3393704} {:x 2, :y 0} {:x 9, :y 0.3919594} {:x 5, :y 0.38808852} {:x 16, :y 0.30409732})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@

;; @@
