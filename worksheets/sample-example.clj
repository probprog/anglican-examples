;; gorilla-repl.fileformat = 1

;; **
;;; # Sample!
;;; 
;;; Maybe you've written a query, and you'd like to wrap it within a distribution and just call `sample`. We can do this!
;; **

;; @@
(ns sample-example
  (:require [gorilla-plot.core :as plot]
            [embang
             [emit :refer [query]]
             [inference :refer [infer warmup]]
             [core :refer [load-algorithm]]
             [state :as state]])
  (:use [mrepl core]
        [embang emit runtime]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; We use "defquery" in the usual way, to define some probability distribution.
;; **

;; @@
(defquery my-query [data]
  (let [p (sample (beta 1 1))
        likelihood (fn [x] (observe (flip p) x))]
    (map likelihood data)
    (predict :p p)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sample-example/my-query</span>","value":"#'sample-example/my-query"}
;; <=

;; **
;;; Here's some hypothetical data:
;; **

;; @@
(def data [true true false true true])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sample-example/data</span>","value":"#'sample-example/data"}
;; <=

;; **
;;; When we run the query, the general behaviour is to construct a (lazy) sequence of weighted samples. Here are the first 6:
;; **

;; @@
(def results (->> (doquery :smc my-query [data] :number-of-particles 3)
                  (take 6)))


(map get-predicts results)
(map get-log-weight results)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>-3.7439096283919806</span>","value":"-3.7439096283919806"},{"type":"html","content":"<span class='clj-double'>-3.7439096283919806</span>","value":"-3.7439096283919806"},{"type":"html","content":"<span class='clj-double'>-3.7439096283919806</span>","value":"-3.7439096283919806"},{"type":"html","content":"<span class='clj-double'>-4.102520955636695</span>","value":"-4.102520955636695"},{"type":"html","content":"<span class='clj-double'>-4.102520955636695</span>","value":"-4.102520955636695"},{"type":"html","content":"<span class='clj-double'>-4.102520955636695</span>","value":"-4.102520955636695"}],"value":"(-3.7439096283919806 -3.7439096283919806 -3.7439096283919806 -4.102520955636695 -4.102520955636695 -4.102520955636695)"}
;; <=

;; **
;;; Alternatively, we could wrap the query inside a distribution object and just call "sample". The idea will be to create a distribution object that closes over the state of the sampler. Every time we see a new (weighted) sample from the stream, we emit it (or emit our previous sample) according to a Metropolis-Hastings ratio.
;;; 
;;; For unweighted samples, this reduces to "emit the next sample deterministically".
;;; 
;;; For SMC with 1 particle per sweep, this reduces to a standard independent MH sampler.
;; **

;; @@
;; We maintain at all times an atom with two elements: 
;;
;; (1) the current state
;; (2) a lazy sequence whose `rest` represents all future samples

(defn mh-init [[state proposal]]
  "Initialze the sampler"
  [(first proposal) proposal])

(defn mh-transition [[state proposal]]
  "Atomically update the sampler state"
  (let [old-log-weight (get-log-weight state)
        candidate (first proposal)
        new-log-weight (get-log-weight candidate)]
    (if (> (- new-log-weight old-log-weight) (log (rand)))
      [candidate (rest proposal)]
      [state (rest proposal)])))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sample-example/mh-transition</span>","value":"#'sample-example/mh-transition"}
;; <=

;; **
;;; We can use these to draw samples indefinitely. We call this new form `sample!` instead of `sample` in order to make it clear that sampling in this manner changes state.
;; **

;; @@
(defn sample! [sampler]
  (get-predicts (first (swap! sampler mh-transition))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;sample-example/sample!</span>","value":"#'sample-example/sample!"}
;; <=

;; **
;;; Let's try it out on our query above.
;; **

;; @@
(def my-sampler (let [producer (atom [nil (doquery :smc my-query [data] :number-of-particles 3)])]
                  (swap! producer mh-init)
                  producer))


(sample! my-sampler)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:p</span>","value":":p"},{"type":"html","content":"<span class='clj-double'>0.8362967483699322</span>","value":"0.8362967483699322"}],"value":"[:p 0.8362967483699322]"}],"value":"{:p 0.8362967483699322}"}
;; <=

;; @@
;; Draw 20 more samples

(clojure.pprint/pprint 
  (repeatedly 20 #(sample! my-sampler)))
;; @@
;; ->
;;; ({:p 0.8362967483699322}
;;;  {:p 0.8362967483699322}
;;;  {:p 0.40374255762435496}
;;;  {:p 0.5041535652708262}
;;;  {:p 0.436164558166638}
;;;  {:p 0.7381025159265846}
;;;  {:p 0.5375720320735127}
;;;  {:p 0.7381025159265846}
;;;  {:p 0.7467270297929645}
;;;  {:p 0.7467270297929645}
;;;  {:p 0.7467270297929645}
;;;  {:p 0.5687454447615892}
;;;  {:p 0.5687454447615892}
;;;  {:p 0.5687454447615892}
;;;  {:p 0.9738936955109239}
;;;  {:p 0.9738936955109239}
;;;  {:p 0.9738936955109239}
;;;  {:p 0.499451253330335}
;;;  {:p 0.499451253330335}
;;;  {:p 0.40285826358012855})
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; We could probably define a nice macro for wrapping the sampler definition up above.
;;; 
;;; One final warning: please do not try to print the sampler itself.
;; **

;; @@
;; BAD: will try to evaluate the infinite sequence of future samples.

; my-sampler 
;; @@

;; @@

;; @@
