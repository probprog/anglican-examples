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
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [anglican core runtime emit]))
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
;;; the maximum-entropy distribution for @@p@@ on this closed interval. In this case,
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/run-pencil-factory</span>","value":"#'deli/run-pencil-factory"}
;; <=

;; **
;;; Now we run the query and plot a histogram of the posterior distribution over p given that we observe one defective pencil.
;; **

;; @@
(def sampler (doquery :smc run-pencil-factory nil :number-of-particles 10000))
(def samples (take 10000 (map :result sampler)))
(plot/histogram samples)


[(float (mean samples))
 (float (variance samples))]
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sampler</span>","value":"#'deli/sampler"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/samples</span>","value":"#'deli/samples"}],"value":"[#'deli/sampler,#'deli/samples]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"45eddd50-4505-49fd-97ed-634b4acfb1a4","values":[{"x":0.00017464197640193468,"y":0},{"x":0.06596146473710704,"y":1428},{"x":0.13174828749781214,"y":1143},{"x":0.19753511025851725,"y":1129},{"x":0.26332193301922235,"y":955},{"x":0.3291087557799275,"y":876},{"x":0.3948955785406326,"y":843},{"x":0.46068240130133775,"y":708},{"x":0.5264692240620429,"y":723},{"x":0.592256046822748,"y":610},{"x":0.6580428695834531,"y":441},{"x":0.7238296923441583,"y":394},{"x":0.7896165151048634,"y":321},{"x":0.8554033378655685,"y":229},{"x":0.9211901606262737,"y":138},{"x":0.9869769833869788,"y":62},{"x":1.0527638061476838,"y":0}]}],"marks":[{"type":"line","from":{"data":"45eddd50-4505-49fd-97ed-634b4acfb1a4"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"45eddd50-4505-49fd-97ed-634b4acfb1a4","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"45eddd50-4505-49fd-97ed-634b4acfb1a4","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"45eddd50-4505-49fd-97ed-634b4acfb1a4\", :values ({:x 1.7464197640193468E-4, :y 0} {:x 0.06596146473710704, :y 1428.0} {:x 0.13174828749781214, :y 1143.0} {:x 0.19753511025851725, :y 1129.0} {:x 0.26332193301922235, :y 955.0} {:x 0.3291087557799275, :y 876.0} {:x 0.3948955785406326, :y 843.0} {:x 0.46068240130133775, :y 708.0} {:x 0.5264692240620429, :y 723.0} {:x 0.592256046822748, :y 610.0} {:x 0.6580428695834531, :y 441.0} {:x 0.7238296923441583, :y 394.0} {:x 0.7896165151048634, :y 321.0} {:x 0.8554033378655685, :y 229.0} {:x 0.9211901606262737, :y 138.0} {:x 0.9869769833869788, :y 62.0} {:x 1.0527638061476838, :y 0})}], :marks [{:type \"line\", :from {:data \"45eddd50-4505-49fd-97ed-634b4acfb1a4\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"45eddd50-4505-49fd-97ed-634b4acfb1a4\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"45eddd50-4505-49fd-97ed-634b4acfb1a4\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[[#'deli/sampler,#'deli/samples],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"45eddd50-4505-49fd-97ed-634b4acfb1a4\", :values ({:x 1.7464197640193468E-4, :y 0} {:x 0.06596146473710704, :y 1428.0} {:x 0.13174828749781214, :y 1143.0} {:x 0.19753511025851725, :y 1129.0} {:x 0.26332193301922235, :y 955.0} {:x 0.3291087557799275, :y 876.0} {:x 0.3948955785406326, :y 843.0} {:x 0.46068240130133775, :y 708.0} {:x 0.5264692240620429, :y 723.0} {:x 0.592256046822748, :y 610.0} {:x 0.6580428695834531, :y 441.0} {:x 0.7238296923441583, :y 394.0} {:x 0.7896165151048634, :y 321.0} {:x 0.8554033378655685, :y 229.0} {:x 0.9211901606262737, :y 138.0} {:x 0.9869769833869788, :y 62.0} {:x 1.0527638061476838, :y 0})}], :marks [{:type \"line\", :from {:data \"45eddd50-4505-49fd-97ed-634b4acfb1a4\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"45eddd50-4505-49fd-97ed-634b4acfb1a4\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"45eddd50-4505-49fd-97ed-634b4acfb1a4\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>0.3283981</span>","value":"0.3283981"},{"type":"html","content":"<span class='clj-unkown'>0.056103077</span>","value":"0.056103077"}],"value":"[0.3283981 0.056103077]"}],"value":"[[[#'deli/sampler,#'deli/samples],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"45eddd50-4505-49fd-97ed-634b4acfb1a4\", :values ({:x 1.7464197640193468E-4, :y 0} {:x 0.06596146473710704, :y 1428.0} {:x 0.13174828749781214, :y 1143.0} {:x 0.19753511025851725, :y 1129.0} {:x 0.26332193301922235, :y 955.0} {:x 0.3291087557799275, :y 876.0} {:x 0.3948955785406326, :y 843.0} {:x 0.46068240130133775, :y 708.0} {:x 0.5264692240620429, :y 723.0} {:x 0.592256046822748, :y 610.0} {:x 0.6580428695834531, :y 441.0} {:x 0.7238296923441583, :y 394.0} {:x 0.7896165151048634, :y 321.0} {:x 0.8554033378655685, :y 229.0} {:x 0.9211901606262737, :y 138.0} {:x 0.9869769833869788, :y 62.0} {:x 1.0527638061476838, :y 0})}], :marks [{:type \"line\", :from {:data \"45eddd50-4505-49fd-97ed-634b4acfb1a4\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"45eddd50-4505-49fd-97ed-634b4acfb1a4\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"45eddd50-4505-49fd-97ed-634b4acfb1a4\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}],[0.3283981 0.056103077]]"}
;; <=

;; **
;;; Recall that a uniform-continuous distribution over the interval @@\[0, 1\]@@ is identical to a Beta distribution with pseudocounts @@a = b = 1@@. After observing K successes from N trials, we can compute the posterior expectation and variance of p analytically:
;;; 
;;; >@@E[p] = \frac{a + K}{a + b + N}@@
;;; 
;;; >@@Var[p] = \frac{(a + K) (b + N - K)}{(a + b + N)^2(a + b + N + 1)}@@
;;; 
;;; We will compare these with the values estimated by averaging over samples from the probabilistic program above (i.e. for @@a = b = 1, N = 1, K = 0@@).
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
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/exp-beta</span>","value":"#'deli/exp-beta"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/var-beta</span>","value":"#'deli/var-beta"}],"value":"[#'deli/exp-beta,#'deli/var-beta]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/exp-beta-pos</span>","value":"#'deli/exp-beta-pos"}],"value":"[[#'deli/exp-beta,#'deli/var-beta],#'deli/exp-beta-pos]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/var-beta-pos</span>","value":"#'deli/var-beta-pos"}],"value":"[[[#'deli/exp-beta,#'deli/var-beta],#'deli/exp-beta-pos],#'deli/var-beta-pos]"}
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/run-pencil-factory</span>","value":"#'deli/run-pencil-factory"}
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
           :number-of-particles 10000))
(def samples (take 10000 (map :result sampler)))
(plot/histogram samples)


[(float (mean samples))
 (float (variance samples))]

[(float (exp-beta-pos a b n k ))
 (float (var-beta-pos a b n k))]
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/a</span>","value":"#'deli/a"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/b</span>","value":"#'deli/b"}],"value":"[#'deli/a,#'deli/b]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/n</span>","value":"#'deli/n"}],"value":"[[#'deli/a,#'deli/b],#'deli/n]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/k</span>","value":"#'deli/k"}],"value":"[[[#'deli/a,#'deli/b],#'deli/n],#'deli/k]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sampler</span>","value":"#'deli/sampler"}],"value":"[[[[#'deli/a,#'deli/b],#'deli/n],#'deli/k],#'deli/sampler]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/samples</span>","value":"#'deli/samples"}],"value":"[[[[[#'deli/a,#'deli/b],#'deli/n],#'deli/k],#'deli/sampler],#'deli/samples]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"98b13cc2-c516-4f03-91da-98fa03a1ea38","values":[{"x":0.28123574539541285,"y":0},{"x":0.3213439157096138,"y":445},{"x":0.36145208602381473,"y":415},{"x":0.4015602563380157,"y":872},{"x":0.4416684266522166,"y":1406},{"x":0.48177659696641756,"y":1195},{"x":0.5218847672806185,"y":1532},{"x":0.5619929375948195,"y":1319},{"x":0.6021011079090205,"y":981},{"x":0.6422092782232215,"y":809},{"x":0.6823174485374225,"y":539},{"x":0.7224256188516235,"y":291},{"x":0.7625337891658245,"y":141},{"x":0.8026419594800255,"y":40},{"x":0.8427501297942265,"y":12},{"x":0.8828583001084275,"y":3},{"x":0.9229664704226285,"y":0}]}],"marks":[{"type":"line","from":{"data":"98b13cc2-c516-4f03-91da-98fa03a1ea38"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"98b13cc2-c516-4f03-91da-98fa03a1ea38","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"98b13cc2-c516-4f03-91da-98fa03a1ea38","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"98b13cc2-c516-4f03-91da-98fa03a1ea38\", :values ({:x 0.28123574539541285, :y 0} {:x 0.3213439157096138, :y 445.0} {:x 0.36145208602381473, :y 415.0} {:x 0.4015602563380157, :y 872.0} {:x 0.4416684266522166, :y 1406.0} {:x 0.48177659696641756, :y 1195.0} {:x 0.5218847672806185, :y 1532.0} {:x 0.5619929375948195, :y 1319.0} {:x 0.6021011079090205, :y 981.0} {:x 0.6422092782232215, :y 809.0} {:x 0.6823174485374225, :y 539.0} {:x 0.7224256188516235, :y 291.0} {:x 0.7625337891658245, :y 141.0} {:x 0.8026419594800255, :y 40.0} {:x 0.8427501297942265, :y 12.0} {:x 0.8828583001084275, :y 3.0} {:x 0.9229664704226285, :y 0})}], :marks [{:type \"line\", :from {:data \"98b13cc2-c516-4f03-91da-98fa03a1ea38\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"98b13cc2-c516-4f03-91da-98fa03a1ea38\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"98b13cc2-c516-4f03-91da-98fa03a1ea38\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[[[[[[#'deli/a,#'deli/b],#'deli/n],#'deli/k],#'deli/sampler],#'deli/samples],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"98b13cc2-c516-4f03-91da-98fa03a1ea38\", :values ({:x 0.28123574539541285, :y 0} {:x 0.3213439157096138, :y 445.0} {:x 0.36145208602381473, :y 415.0} {:x 0.4015602563380157, :y 872.0} {:x 0.4416684266522166, :y 1406.0} {:x 0.48177659696641756, :y 1195.0} {:x 0.5218847672806185, :y 1532.0} {:x 0.5619929375948195, :y 1319.0} {:x 0.6021011079090205, :y 981.0} {:x 0.6422092782232215, :y 809.0} {:x 0.6823174485374225, :y 539.0} {:x 0.7224256188516235, :y 291.0} {:x 0.7625337891658245, :y 141.0} {:x 0.8026419594800255, :y 40.0} {:x 0.8427501297942265, :y 12.0} {:x 0.8828583001084275, :y 3.0} {:x 0.9229664704226285, :y 0})}], :marks [{:type \"line\", :from {:data \"98b13cc2-c516-4f03-91da-98fa03a1ea38\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"98b13cc2-c516-4f03-91da-98fa03a1ea38\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"98b13cc2-c516-4f03-91da-98fa03a1ea38\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>0.50155777</span>","value":"0.50155777"},{"type":"html","content":"<span class='clj-unkown'>0.011082438</span>","value":"0.011082438"}],"value":"[0.50155777 0.011082438]"}],"value":"[[[[[[[#'deli/a,#'deli/b],#'deli/n],#'deli/k],#'deli/sampler],#'deli/samples],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"98b13cc2-c516-4f03-91da-98fa03a1ea38\", :values ({:x 0.28123574539541285, :y 0} {:x 0.3213439157096138, :y 445.0} {:x 0.36145208602381473, :y 415.0} {:x 0.4015602563380157, :y 872.0} {:x 0.4416684266522166, :y 1406.0} {:x 0.48177659696641756, :y 1195.0} {:x 0.5218847672806185, :y 1532.0} {:x 0.5619929375948195, :y 1319.0} {:x 0.6021011079090205, :y 981.0} {:x 0.6422092782232215, :y 809.0} {:x 0.6823174485374225, :y 539.0} {:x 0.7224256188516235, :y 291.0} {:x 0.7625337891658245, :y 141.0} {:x 0.8026419594800255, :y 40.0} {:x 0.8427501297942265, :y 12.0} {:x 0.8828583001084275, :y 3.0} {:x 0.9229664704226285, :y 0})}], :marks [{:type \"line\", :from {:data \"98b13cc2-c516-4f03-91da-98fa03a1ea38\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"98b13cc2-c516-4f03-91da-98fa03a1ea38\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"98b13cc2-c516-4f03-91da-98fa03a1ea38\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}],[0.50155777 0.011082438]]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>0.5</span>","value":"0.5"},{"type":"html","content":"<span class='clj-unkown'>0.011904762</span>","value":"0.011904762"}],"value":"[0.5 0.011904762]"}],"value":"[[[[[[[[#'deli/a,#'deli/b],#'deli/n],#'deli/k],#'deli/sampler],#'deli/samples],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"98b13cc2-c516-4f03-91da-98fa03a1ea38\", :values ({:x 0.28123574539541285, :y 0} {:x 0.3213439157096138, :y 445.0} {:x 0.36145208602381473, :y 415.0} {:x 0.4015602563380157, :y 872.0} {:x 0.4416684266522166, :y 1406.0} {:x 0.48177659696641756, :y 1195.0} {:x 0.5218847672806185, :y 1532.0} {:x 0.5619929375948195, :y 1319.0} {:x 0.6021011079090205, :y 981.0} {:x 0.6422092782232215, :y 809.0} {:x 0.6823174485374225, :y 539.0} {:x 0.7224256188516235, :y 291.0} {:x 0.7625337891658245, :y 141.0} {:x 0.8026419594800255, :y 40.0} {:x 0.8427501297942265, :y 12.0} {:x 0.8828583001084275, :y 3.0} {:x 0.9229664704226285, :y 0})}], :marks [{:type \"line\", :from {:data \"98b13cc2-c516-4f03-91da-98fa03a1ea38\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"98b13cc2-c516-4f03-91da-98fa03a1ea38\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"98b13cc2-c516-4f03-91da-98fa03a1ea38\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}],[0.50155777 0.011082438]],[0.5 0.011904762]]"}
;; <=

;; **
;;; which as we expect gives p approximately equal to 0.5.
;;; 
;;; Crucially, when writing probabilistic programs we are not limited to simple conjugate models like the one we have defined above. Instead of the beta prior on @@p@@, we could introduce a new prior for which we can no longer compute the posterior distribution by hand. For example, suppose we were to place a truncated exponential prior on @@p@@, with @@z \sim \exp(2)@@ and @@p | z = \min(z, 1)@@,
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
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;deli/run-pencil-factory</span>","value":"#'deli/run-pencil-factory"}
;; <=

;; @@
(def n 10)
(def k 3)

(def sampler (doquery :smc run-pencil-factory [n k] :number-of-particles 10000))
(def samples (take 10000 (map :result sampler)))
(plot/histogram samples)


[(float (mean samples))
 (float (variance samples))]
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;deli/n</span>","value":"#'deli/n"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/k</span>","value":"#'deli/k"}],"value":"[#'deli/n,#'deli/k]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/sampler</span>","value":"#'deli/sampler"}],"value":"[[#'deli/n,#'deli/k],#'deli/sampler]"},{"type":"html","content":"<span class='clj-var'>#&#x27;deli/samples</span>","value":"#'deli/samples"}],"value":"[[[#'deli/n,#'deli/k],#'deli/sampler],#'deli/samples]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"99a9c0d5-196e-45a5-bda2-d12fa9246d01","values":[{"x":0.013712003823675989,"y":0},{"x":0.0669795379157345,"y":81},{"x":0.120247072007793,"y":431},{"x":0.1735146060998515,"y":1020},{"x":0.22678214019191,"y":1524},{"x":0.28004967428396854,"y":1679},{"x":0.3333172083760271,"y":1654},{"x":0.3865847424680856,"y":1241},{"x":0.43985227656014414,"y":962},{"x":0.4931198106522027,"y":636},{"x":0.5463873447442612,"y":387},{"x":0.5996548788363197,"y":233},{"x":0.6529224129283783,"y":102},{"x":0.7061899470204368,"y":37},{"x":0.7594574811124953,"y":11},{"x":0.8127250152045539,"y":2},{"x":0.8659925492966124,"y":0}]}],"marks":[{"type":"line","from":{"data":"99a9c0d5-196e-45a5-bda2-d12fa9246d01"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"99a9c0d5-196e-45a5-bda2-d12fa9246d01","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"99a9c0d5-196e-45a5-bda2-d12fa9246d01","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"99a9c0d5-196e-45a5-bda2-d12fa9246d01\", :values ({:x 0.013712003823675989, :y 0} {:x 0.0669795379157345, :y 81.0} {:x 0.120247072007793, :y 431.0} {:x 0.1735146060998515, :y 1020.0} {:x 0.22678214019191, :y 1524.0} {:x 0.28004967428396854, :y 1679.0} {:x 0.3333172083760271, :y 1654.0} {:x 0.3865847424680856, :y 1241.0} {:x 0.43985227656014414, :y 962.0} {:x 0.4931198106522027, :y 636.0} {:x 0.5463873447442612, :y 387.0} {:x 0.5996548788363197, :y 233.0} {:x 0.6529224129283783, :y 102.0} {:x 0.7061899470204368, :y 37.0} {:x 0.7594574811124953, :y 11.0} {:x 0.8127250152045539, :y 2.0} {:x 0.8659925492966124, :y 0})}], :marks [{:type \"line\", :from {:data \"99a9c0d5-196e-45a5-bda2-d12fa9246d01\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"99a9c0d5-196e-45a5-bda2-d12fa9246d01\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"99a9c0d5-196e-45a5-bda2-d12fa9246d01\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[[[[#'deli/n,#'deli/k],#'deli/sampler],#'deli/samples],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"99a9c0d5-196e-45a5-bda2-d12fa9246d01\", :values ({:x 0.013712003823675989, :y 0} {:x 0.0669795379157345, :y 81.0} {:x 0.120247072007793, :y 431.0} {:x 0.1735146060998515, :y 1020.0} {:x 0.22678214019191, :y 1524.0} {:x 0.28004967428396854, :y 1679.0} {:x 0.3333172083760271, :y 1654.0} {:x 0.3865847424680856, :y 1241.0} {:x 0.43985227656014414, :y 962.0} {:x 0.4931198106522027, :y 636.0} {:x 0.5463873447442612, :y 387.0} {:x 0.5996548788363197, :y 233.0} {:x 0.6529224129283783, :y 102.0} {:x 0.7061899470204368, :y 37.0} {:x 0.7594574811124953, :y 11.0} {:x 0.8127250152045539, :y 2.0} {:x 0.8659925492966124, :y 0})}], :marks [{:type \"line\", :from {:data \"99a9c0d5-196e-45a5-bda2-d12fa9246d01\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"99a9c0d5-196e-45a5-bda2-d12fa9246d01\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"99a9c0d5-196e-45a5-bda2-d12fa9246d01\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-unkown'>0.30080745</span>","value":"0.30080745"},{"type":"html","content":"<span class='clj-unkown'>0.015359213</span>","value":"0.015359213"}],"value":"[0.30080745 0.015359213]"}],"value":"[[[[[#'deli/n,#'deli/k],#'deli/sampler],#'deli/samples],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"99a9c0d5-196e-45a5-bda2-d12fa9246d01\", :values ({:x 0.013712003823675989, :y 0} {:x 0.0669795379157345, :y 81.0} {:x 0.120247072007793, :y 431.0} {:x 0.1735146060998515, :y 1020.0} {:x 0.22678214019191, :y 1524.0} {:x 0.28004967428396854, :y 1679.0} {:x 0.3333172083760271, :y 1654.0} {:x 0.3865847424680856, :y 1241.0} {:x 0.43985227656014414, :y 962.0} {:x 0.4931198106522027, :y 636.0} {:x 0.5463873447442612, :y 387.0} {:x 0.5996548788363197, :y 233.0} {:x 0.6529224129283783, :y 102.0} {:x 0.7061899470204368, :y 37.0} {:x 0.7594574811124953, :y 11.0} {:x 0.8127250152045539, :y 2.0} {:x 0.8659925492966124, :y 0})}], :marks [{:type \"line\", :from {:data \"99a9c0d5-196e-45a5-bda2-d12fa9246d01\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"99a9c0d5-196e-45a5-bda2-d12fa9246d01\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"99a9c0d5-196e-45a5-bda2-d12fa9246d01\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}],[0.30080745 0.015359213]]"}
;; <=

;; @@

;; @@
