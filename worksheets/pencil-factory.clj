;; gorilla-repl.fileformat = 1

;; **
;;; # The "Pencil Factory": a simple beta-binomial example
;;; 
;;; Probabilistic generative models can be written concisely as probabilistic programs. In this
;;; short exercise we will outline the basic structure of a probabilistic program, and show an
;;; example of automatic posterior inference.
;;; 
;;; First we import the necessary libraries to use Anglican,
;; **

;; @@
(ns pencil-factory
  (:require [gorilla-plot.core :as plot]
            [anglican.stat :as stat])
  (:use clojure.repl
        [anglican core runtime emit [state :only [get-predicts]]]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; A classical example problem deals with estimating the failure rate of a process, as in the
;;; following example:
;;; 
;;; >Suppose that you are thinking about purchasing a factory that makes pencils.
;;; Your accountants have determined that you can make a profit (i.e.
;;; you should transact the purchase) if the percentage of defective pencils
;;; manufactured by the factory is less than 30%.
;;; In your prior experience, you learned that, on average, pencil factories produce
;;; defective pencils at a rate of 50%.
;;; To make your judgement about the efficiency of this factory you test pencils
;;; one at a time in sequence as they emerge from the factory to see if they are
;;; defective.
;;; 
;;; We let @@y_1,\ldots,y_N@@, with @@y_n\in\{0,1\}@@ be a set of defective/ not defective observations. A
;;; very simple approach would be to model each observation yn as an independent Bernoulli
;;; trial, with some unknown success rate @@p@@. We place a prior distribution on @@p@@, the shape of
;;; which represents the strength of our conviction that pencil factories produce 50% defective
;;; pencils. A traditional choice of prior might be a uniform distribution on the interval @@[0, 1]@@,
;;; the maximum entropy distribution for @@p@@ which has an expected value of @@0.5@@. In this case,
;;; our full model for the pencil factory data is
;;; 
;;; $$p\sim \mathrm{Uniform}[0,1]$$
;;; $$y_n\sim \mathrm{Bernoulli}(p).$$
;;; 
;;; Suppose the very first pencil that comes off the conveyor belt is defective. We can write this
;;; model as a probabilistic program, complete with observing our defective pencil, as
;; **

;; @@
(defquery run-pencil-factory
    "a simple pencil factory"
     (let [p (sample (uniform-continuous 0 1))]
           (observe (flip p) false)
            p))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pencil-factory/run-pencil-factory</span>","value":"#'pencil-factory/run-pencil-factory"}
;; <=

;; **
;;; Now we run the query and plot a histogram of the posterior distribution over p given that we observe one defective pencil.
;; **

;; @@
(def sampler (doquery :smc run-pencil-factory nil :number-of-particles 100))
(def samples (take 10000 (map :result sampler)))
(plot/histogram samples)


[(float (mean samples))
 (float (variance samples))]
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>0.33149165</span>","value":"0.33149165"},{"type":"html","content":"<span class='clj-unkown'>0.056539405</span>","value":"0.056539405"}],"value":"[0.33149165 0.056539405]"}
;; <=

;; **
;;; Recall that a uniform-continuous distribution over the interval [0, 1] is identical to a Beta distribution with pseudocounts a = b = 1. After observing K successes from N trials, we can compute the posterior expectation and variance of p analytically:
;;; 
;;; >E[p] = (a + K) / (a + b + N)
;;; 
;;; >Var[p] = (a + K) * (b + N - K) / (a + b + N)^2 / (a + b + N + 1)
;;; 
;;; We will compare these with the values estimated by averaging over samples from the probabilistic program above (i.e. for a = b = 1, N = 1, K = 0).
;;; 
;; **

;; @@
(defn exp-beta
  "expectation of beta distribution"
  [a b]
  (/ a (+ a b)))

(defn var-beta
  "variance of beta distribution"
  [a b]
  (/ (* a b) (* (Math/pow (+ a b) 2) (+ a b 1))))

(defn exp-beta-pos
  "posterior expectation of beta distribution having observed K successes from N trials"
  [a b N K]
  (exp-beta (+ a K) (- (+ b N) K)))

(defn var-beta-pos
  "posterior variance of beta distribution having observed K successes from N trials"
  [a b N K]
  (var-beta (+ a K) (- (+ b N) K)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pencil-factory/var-beta-pos</span>","value":"#'pencil-factory/var-beta-pos"}
;; <=

;; @@
(let [a 1
      b 1
      n 1
      k 0]
  [(float (exp-beta-pos a b n k))
   (float (var-beta-pos a b n k))])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>0.33333334</span>","value":"0.33333334"},{"type":"html","content":"<span class='clj-unkown'>0.055555556</span>","value":"0.055555556"}],"value":"[0.33333334 0.055555556]"}
;; <=

;; **
;;; We see these values closely agree with the ones found empirically.
;;; 
;;; Now we will create a new query that lets us input multiple observations and use a more flexible prior for p,
;; **

;; @@
(defquery run-pencil-factory [a b n k]
    "a simple pencil factory"
     (let [p (sample (beta a b))]
           (observe (binomial n p) k)
           p))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pencil-factory/run-pencil-factory</span>","value":"#'pencil-factory/run-pencil-factory"}
;; <=

;; **
;;; This redefines the previous query, but lets us condition on i.i.d Bernoulli samples. Now, introducting 10 true and 3 false pseudocounts, and observing 7 false observations we get,
;; **

;; @@
(def a 10)
(def b 3)
(def n 7)
(def k 0)

(def sampler 
  (doquery :smc 
           run-pencil-factory 
           [a b n k] 
           :number-of-particles 1000))
(def samples (take 10000 (map :result sampler)))
(plot/histogram samples)


[(float (mean samples))
 (float (variance samples))]

[(float (exp-beta-pos a b n k ))
 (float (var-beta-pos a b n k))]
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>0.5</span>","value":"0.5"},{"type":"html","content":"<span class='clj-unkown'>0.011904762</span>","value":"0.011904762"}],"value":"[0.5 0.011904762]"}
;; <=

;; **
;;; which as we expect gives p approximately equal to 0.5.
;;; 
;;; Crucially, when writing probabilistic programs we are not limited to simple conjugate models like the one we have defined above. Instead of the beta prior on p, we could introduce a new prior for which we can no longer compute the posterior distribution by hand. For example, suppose we were to place a truncated exponential prior on p, with z ~ exp(2) and p | z = min(z, 1),
;; **

;; @@
(defquery run-pencil-factory [n k]
    "a simple pencil factory"
     (let [z (sample (exponential 2))
           p (min z 1)]
       (observe (binomial n p) k)
       p))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;pencil-factory/run-pencil-factory</span>","value":"#'pencil-factory/run-pencil-factory"}
;; <=

;; @@
(def n 10)
(def k 3)

(def sampler (doquery :smc run-pencil-factory [n k] :number-of-particles 1000))
(def samples (take 10000 (map :result sampler)))
(plot/histogram samples)


[(float (mean samples))
 (float (variance samples))]
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>0.30122435</span>","value":"0.30122435"},{"type":"html","content":"<span class='clj-unkown'>0.015498705</span>","value":"0.015498705"}],"value":"[0.30122435 0.015498705]"}
;; <=
