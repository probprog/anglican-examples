;; gorilla-repl.fileformat = 1

;; **
;;; # GDAs/GMMs for classification
;; **

;; @@
(ns classification
  (:use [anglican core runtime emit stat])
  (:require [gorilla-plot.core :as plot])
  (:require [clojure.core.matrix :as mat]))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; In this worksheet, we present a unified view of how to use Anglican for basic classification and clustering tasks. The aim here is to present why these tasks are so similar from a generative probabilistic perspective, and exploring how we can use Anglican to solve these problems in a fully-Bayesian manner.
;;; 
;;; Fix a set of possible classifications @@ \{0, 1, ..., C\} @@, and a space of possible features @@ \mathbb{R}^D @@. We apply the following model for classification:
;;; 
;;; $$ p(x, c | \theta) = p(x | c, \theta) p(c | \theta) = N(x | \mu\_c, \Sigma\_c) \pi\_c $$
;;; 
;;; Here, @@ c \in \{0, ..., C\} @@ is a particular classification, and @@ x \in \mathbb{R}^D @@ is a particular set of features. @@ \theta @@ is a set of parameters for the classification procedure: it consists of prior classification probabilities @@ \pi @@, feature means @@ \mu\_c @@, and feature covariances @@ \Sigma\_c @@.
;;; 
;;; If the correct classifications of each data point are known - i.e. we have a dataset @@ D = (x\_1, c\_1, ..., x\_N, c\_N) @@ - then is generally called a classification task, and this model can be called a multi-class **Gaussian discriminant analysis** (GDA). If the correct classifications of each data point are *not* known, and we only have a list of observed features @@ D = (x\_1, ..., x\_N) @@, then this is generally called a *clustering* task, and this model is called a **Gaussian mixture model** (GMM).
;;; 
;;; In this worksheet, we'll pursue an approach that can account for both data points with specified labels @@ (x, c) @@, and data points with unspecified labels. We account for the latter by summing over all possible classifications (which is clearly inefficient if there is a non-small number of possible classifications). In Anglican, we model this procedure as a distribution, called GDA, the parameters of which are a set of prior classification probabilities for each class @@ \pi\_c @@, and a list of feature distributions for each class (which, in the model above, correspond to the multi-variate normal distributions @@ N(x | \mu\_c, \Sigma\_c) @@.)
;;; 
;;; Sampling from this distribution is straightforward. Simply sample a possible classification @@ c \sim \pi\_c @@ from the categorical prior over classifications, and then sample a set of features from the corresponding feature distribution.
;;; 
;;; Observing on this distribution is more complex, because we must account for the case in which the classification is specified, and the case in which it isn't. This is handled straightforwardly in the below distribution using Anglican's log-sum-exp function:
;;; 
;; **

;; @@

(defdist gda [pi feature-dists]

  [class-dist (discrete pi)
   C (count pi)]

  (sample* [this]
           (let [my-class (sample* class-dist)
                 my-features (sample* (nth feature-dists my-class))]
             [my-class my-features]))

  (observe* [this [my-features my-class]]
            (if (not (nil? my-class))
              (+
                (observe* class-dist my-class)
                (observe* (nth feature-dists my-class) my-features))

              (reduce log-sum-exp
                      (map

                        #(+'
                           (observe* class-dist %)
                           (observe* (nth feature-dists %) my-features))

                        (range C))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#multifn[print-method 0x6efdf98c]</span>","value":"#multifn[print-method 0x6efdf98c]"}
;; <=

;; **
;;; The below functions are simply helper functions that we will use to classify individual data points using the above model. The function `classification-log-probabilities` takes in a GDA instance as constructed above, and a specific set of features, and returns a list of the unnormalized log-probabilities that this set of features belongs to each class. The function `classification-probabilities` simply calls the above function, and then exponentiates and normalizes the results, to obtain normal probabilities.
;;; 
;;; Finally, if it is not a distribution over classifications but rather a single projected classification that is desired, the function `classify-feature` calculates the log-probability of each classification being the correct one, and then simply returns the classification with highest probability, using the above `index-of-maximum` helper function.
;; **

;; @@

(defn classification-log-probabilities [gda-dist feature]
  (map
    (fn [c] (observe* gda-dist [feature c]))
    (range (:C gda-dist))))

(defn classification-probabilities [gda-dist feature]
  (let [unnormalized (classification-log-probabilities gda-dist feature)
        sum (reduce log-sum-exp unnormalized)
        normalized (map #(- % sum) unnormalized)]
    (map #(Math/exp %) normalized)))



(defn index-of-maximum [lst]
  (second
    (apply max-key first
           (map
             (fn [x y] [x y])
             lst
             (range)))))

(defn classify-feature [gda-dist feature]
  (let [unnormalized (classification-log-probabilities gda-dist feature)]
    (index-of-maximum unnormalized)))


;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;classification/classification-log-probabilities</span>","value":"#'classification/classification-log-probabilities"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/classification-probabilities</span>","value":"#'classification/classification-probabilities"}],"value":"[#'classification/classification-log-probabilities,#'classification/classification-probabilities]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/index-of-maximum</span>","value":"#'classification/index-of-maximum"}],"value":"[[#'classification/classification-log-probabilities,#'classification/classification-probabilities],#'classification/index-of-maximum]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/classify-feature</span>","value":"#'classification/classify-feature"}],"value":"[[[#'classification/classification-log-probabilities,#'classification/classification-probabilities],#'classification/index-of-maximum],#'classification/classify-feature]"}
;; <=

;; **
;;; The following simple example shows how to use the above GDA distribution, and helper functions, to create a GDA distribution with specified feature distributions and prior classification probabilities. Then, it uses that GDA to classify the feature set @@x = (5, 5)@@, computing the probabilities of each classification:
;; **

;; @@
(def feature-dist-1 (mvn [0 0] [[4 3] [3 4]]))
(def feature-dist-2 (mvn [0 10] [[10 -3] [-3 5]]))
(def feature-dist-3 (mvn [10 10] [[6 0] [0 8]]))

(def pi [1 1 1])

(def my-gda (gda pi (list feature-dist-1 feature-dist-2 feature-dist-3)))

(def x [5 5])


(def probs (map #(Math/round (* 100 %)) (classification-probabilities my-gda x)))

(plot/bar-chart '(0 1 2) probs)
(reduce str (map #(str (Math/round (* 100 %)) "% ") (classification-probabilities my-gda x)))

(str "The feature " x " has classification " (classify-feature my-gda x))

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;classification/feature-dist-1</span>","value":"#'classification/feature-dist-1"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/feature-dist-2</span>","value":"#'classification/feature-dist-2"}],"value":"[#'classification/feature-dist-1,#'classification/feature-dist-2]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/feature-dist-3</span>","value":"#'classification/feature-dist-3"}],"value":"[[#'classification/feature-dist-1,#'classification/feature-dist-2],#'classification/feature-dist-3]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/pi</span>","value":"#'classification/pi"}],"value":"[[[#'classification/feature-dist-1,#'classification/feature-dist-2],#'classification/feature-dist-3],#'classification/pi]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/my-gda</span>","value":"#'classification/my-gda"}],"value":"[[[[#'classification/feature-dist-1,#'classification/feature-dist-2],#'classification/feature-dist-3],#'classification/pi],#'classification/my-gda]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/x</span>","value":"#'classification/x"}],"value":"[[[[[#'classification/feature-dist-1,#'classification/feature-dist-2],#'classification/feature-dist-3],#'classification/pi],#'classification/my-gda],#'classification/x]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/probs</span>","value":"#'classification/probs"}],"value":"[[[[[[#'classification/feature-dist-1,#'classification/feature-dist-2],#'classification/feature-dist-3],#'classification/pi],#'classification/my-gda],#'classification/x],#'classification/probs]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"8e056bf0-b1d0-405a-a7b2-4f9b76592c00","values":[{"x":0,"y":43},{"x":1,"y":41},{"x":2,"y":15}]}],"marks":[{"type":"rect","from":{"data":"8e056bf0-b1d0-405a-a7b2-4f9b76592c00"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"8e056bf0-b1d0-405a-a7b2-4f9b76592c00","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"8e056bf0-b1d0-405a-a7b2-4f9b76592c00","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\", :values ({:x 0, :y 43} {:x 1, :y 41} {:x 2, :y 15})}], :marks [{:type \"rect\", :from {:data \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[[[[[[[#'classification/feature-dist-1,#'classification/feature-dist-2],#'classification/feature-dist-3],#'classification/pi],#'classification/my-gda],#'classification/x],#'classification/probs],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\", :values ({:x 0, :y 43} {:x 1, :y 41} {:x 2, :y 15})}], :marks [{:type \"rect\", :from {:data \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"html","content":"<span class='clj-string'>&quot;43% 41% 15% &quot;</span>","value":"\"43% 41% 15% \""}],"value":"[[[[[[[[#'classification/feature-dist-1,#'classification/feature-dist-2],#'classification/feature-dist-3],#'classification/pi],#'classification/my-gda],#'classification/x],#'classification/probs],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\", :values ({:x 0, :y 43} {:x 1, :y 41} {:x 2, :y 15})}], :marks [{:type \"rect\", :from {:data \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}],\"43% 41% 15% \"]"},{"type":"html","content":"<span class='clj-string'>&quot;The feature [5 5] has classification 0&quot;</span>","value":"\"The feature [5 5] has classification 0\""}],"value":"[[[[[[[[[#'classification/feature-dist-1,#'classification/feature-dist-2],#'classification/feature-dist-3],#'classification/pi],#'classification/my-gda],#'classification/x],#'classification/probs],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\", :values ({:x 0, :y 43} {:x 1, :y 41} {:x 2, :y 15})}], :marks [{:type \"rect\", :from {:data \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"8e056bf0-b1d0-405a-a7b2-4f9b76592c00\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}],\"43% 41% 15% \"],\"The feature [5 5] has classification 0\"]"}
;; <=

;; **
;;; The above procedure, of course, only works if we *know* the correct classification probabilities `pi` and the correct feature distributions for each classification. In the below query, we place priors on these parameters, infer them from given data, and then output the posterior predictive for the classification of the data point `x`:
;; **

;; @@
;; HYPERPARAMETERS (for priors):

(def nu [1 1 1])
(def alpha [1 1])


;; PRIORS:

(def mu-priors (mvn nu (mat/identity-matrix 3)))
(def sigma (mat/identity-matrix 3))
(def pi-prior (dirichlet alpha))


(defquery gda-trainer [data x]
  (declare :primitive mat/zero-matrix mat/to-vector mat/identity-matrix gda classification-probabilities)
  (let [N (count data)

        ;; sample parameters

        mu1 (sample mu-priors)
        mu2 (sample mu-priors)
        pi (sample pi-prior)

        ;; construct GDA distribution
        
        feature-dists (list (mvn mu1 sigma) (mvn mu2 sigma))
        gda-dist (gda pi feature-dists)]

    
    ;; condition on data
    (loop [n 0]
      (if (< n N)
        (do
          (observe gda-dist (nth data n))
          (recur (+ n 1)))))
    
    ;; now project the type of x
    (classification-probabilities gda-dist x)))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;classification/nu</span>","value":"#'classification/nu"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/alpha</span>","value":"#'classification/alpha"}],"value":"[#'classification/nu,#'classification/alpha]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/mu-priors</span>","value":"#'classification/mu-priors"}],"value":"[[#'classification/nu,#'classification/alpha],#'classification/mu-priors]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/sigma</span>","value":"#'classification/sigma"}],"value":"[[[#'classification/nu,#'classification/alpha],#'classification/mu-priors],#'classification/sigma]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/pi-prior</span>","value":"#'classification/pi-prior"}],"value":"[[[[#'classification/nu,#'classification/alpha],#'classification/mu-priors],#'classification/sigma],#'classification/pi-prior]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/gda-trainer</span>","value":"#'classification/gda-trainer"}],"value":"[[[[[#'classification/nu,#'classification/alpha],#'classification/mu-priors],#'classification/sigma],#'classification/pi-prior],#'classification/gda-trainer]"}
;; <=

;; **
;;; Specifically, we will infer the parameters of the GDA distribution from the following data. Notice that in this data, as we noted previously, some of the features have a known classification. However, some of the features have no known classification, which means that our probabilistic program will have to sum over all possible classifications in order to learn the parameters. In particular, notice that there are no classifications that are known to be type 0 - which provides an interesting challenge for the inference procedure.
;;; 
;;; Given this data, we are interested in approximating the posterior classification of the feature set `x`:
;; **

;; @@
(def dataset [
               [[3 3 3] 1]
               [[1 1 1]]
               [[0 0 0]]
               [[5 5 5] 1]
               [[4 4 4] 1]
               [[-1 -1 -1]]
               [[-2 0 0]]

               ])

(def x [1.5 1 1])
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;classification/dataset</span>","value":"#'classification/dataset"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/x</span>","value":"#'classification/x"}],"value":"[#'classification/dataset,#'classification/x]"}
;; <=

;; **
;;; As it turns out, learning the parameters for this type of classification problem is actually quite a complex inference problem. So we approximate the true posterior distribution using Anglican's IPMCMC inference algorithm, and 100,000 simulations, which will take some time to run:
;; **

;; @@
(def S 100000)
(def particles 100)
(def nodes 10)

(def samples (take S (drop S 
                           (doquery :ipmcmc gda-trainer [dataset x]
                                    :number-of-particles particles :number-of-nodes nodes))))

(def weights (map :log-weight samples))
(def projections (map :result samples))

(take-last 10 projections)

;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;classification/S</span>","value":"#'classification/S"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/particles</span>","value":"#'classification/particles"}],"value":"[#'classification/S,#'classification/particles]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/nodes</span>","value":"#'classification/nodes"}],"value":"[[#'classification/S,#'classification/particles],#'classification/nodes]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/samples</span>","value":"#'classification/samples"}],"value":"[[[#'classification/S,#'classification/particles],#'classification/nodes],#'classification/samples]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/weights</span>","value":"#'classification/weights"}],"value":"[[[[#'classification/S,#'classification/particles],#'classification/nodes],#'classification/samples],#'classification/weights]"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/projections</span>","value":"#'classification/projections"}],"value":"[[[[[#'classification/S,#'classification/particles],#'classification/nodes],#'classification/samples],#'classification/weights],#'classification/projections]"},{"type":"list-like","open":"<span class='clj-list'>(</span>","close":"<span class='clj-list'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.538848314823797</span>","value":"0.538848314823797"},{"type":"html","content":"<span class='clj-double'>0.46115168517620253</span>","value":"0.46115168517620253"}],"value":"(0.538848314823797 0.46115168517620253)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.538848314823797</span>","value":"0.538848314823797"},{"type":"html","content":"<span class='clj-double'>0.46115168517620253</span>","value":"0.46115168517620253"}],"value":"(0.538848314823797 0.46115168517620253)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.538848314823797</span>","value":"0.538848314823797"},{"type":"html","content":"<span class='clj-double'>0.46115168517620253</span>","value":"0.46115168517620253"}],"value":"(0.538848314823797 0.46115168517620253)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.538848314823797</span>","value":"0.538848314823797"},{"type":"html","content":"<span class='clj-double'>0.46115168517620253</span>","value":"0.46115168517620253"}],"value":"(0.538848314823797 0.46115168517620253)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.538848314823797</span>","value":"0.538848314823797"},{"type":"html","content":"<span class='clj-double'>0.46115168517620253</span>","value":"0.46115168517620253"}],"value":"(0.538848314823797 0.46115168517620253)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.9837451961913958</span>","value":"0.9837451961913958"},{"type":"html","content":"<span class='clj-double'>0.016254803808604182</span>","value":"0.016254803808604182"}],"value":"(0.9837451961913958 0.016254803808604182)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.9837451961913958</span>","value":"0.9837451961913958"},{"type":"html","content":"<span class='clj-double'>0.016254803808604182</span>","value":"0.016254803808604182"}],"value":"(0.9837451961913958 0.016254803808604182)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.9837451961913958</span>","value":"0.9837451961913958"},{"type":"html","content":"<span class='clj-double'>0.016254803808604182</span>","value":"0.016254803808604182"}],"value":"(0.9837451961913958 0.016254803808604182)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.9837451961913958</span>","value":"0.9837451961913958"},{"type":"html","content":"<span class='clj-double'>0.016254803808604182</span>","value":"0.016254803808604182"}],"value":"(0.9837451961913958 0.016254803808604182)"},{"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-double'>0.9837451961913958</span>","value":"0.9837451961913958"},{"type":"html","content":"<span class='clj-double'>0.016254803808604182</span>","value":"0.016254803808604182"}],"value":"(0.9837451961913958 0.016254803808604182)"}],"value":"((0.538848314823797 0.46115168517620253) (0.538848314823797 0.46115168517620253) (0.538848314823797 0.46115168517620253) (0.538848314823797 0.46115168517620253) (0.538848314823797 0.46115168517620253) (0.9837451961913958 0.016254803808604182) (0.9837451961913958 0.016254803808604182) (0.9837451961913958 0.016254803808604182) (0.9837451961913958 0.016254803808604182) (0.9837451961913958 0.016254803808604182))"}],"value":"[[[[[[#'classification/S,#'classification/particles],#'classification/nodes],#'classification/samples],#'classification/weights],#'classification/projections],((0.538848314823797 0.46115168517620253) (0.538848314823797 0.46115168517620253) (0.538848314823797 0.46115168517620253) (0.538848314823797 0.46115168517620253) (0.538848314823797 0.46115168517620253) (0.9837451961913958 0.016254803808604182) (0.9837451961913958 0.016254803808604182) (0.9837451961913958 0.016254803808604182) (0.9837451961913958 0.016254803808604182) (0.9837451961913958 0.016254803808604182))]"}
;; <=

;; **
;;; We summarize our results by calculating the mean probability of each classification, as above:
;; **

;; @@
(defn calculate-mean [series]
  (empirical-mean
    (partition 2
               (interleave series weights))))

;; this will work better
(def percentages (map #(Math/round (* 100 %)) (calculate-mean projections)))

(plot/bar-chart (range (count percentages)) percentages)

(reduce str (map #(str % "% ") percentages))
;; @@
;; =>
;;; {"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"list-like","open":"","close":"","separator":"</pre><pre>","items":[{"type":"html","content":"<span class='clj-var'>#&#x27;classification/calculate-mean</span>","value":"#'classification/calculate-mean"},{"type":"html","content":"<span class='clj-var'>#&#x27;classification/percentages</span>","value":"#'classification/percentages"}],"value":"[#'classification/calculate-mean,#'classification/percentages]"},{"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"efa6ea51-bfd9-4de2-b25b-c66db150f9ad","values":[{"x":0,"y":89},{"x":1,"y":11}]}],"marks":[{"type":"rect","from":{"data":"efa6ea51-bfd9-4de2-b25b-c66db150f9ad"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"efa6ea51-bfd9-4de2-b25b-c66db150f9ad","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"efa6ea51-bfd9-4de2-b25b-c66db150f9ad","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"efa6ea51-bfd9-4de2-b25b-c66db150f9ad\", :values ({:x 0, :y 89} {:x 1, :y 11})}], :marks [{:type \"rect\", :from {:data \"efa6ea51-bfd9-4de2-b25b-c66db150f9ad\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"efa6ea51-bfd9-4de2-b25b-c66db150f9ad\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"efa6ea51-bfd9-4de2-b25b-c66db150f9ad\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}],"value":"[[#'classification/calculate-mean,#'classification/percentages],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"efa6ea51-bfd9-4de2-b25b-c66db150f9ad\", :values ({:x 0, :y 89} {:x 1, :y 11})}], :marks [{:type \"rect\", :from {:data \"efa6ea51-bfd9-4de2-b25b-c66db150f9ad\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"efa6ea51-bfd9-4de2-b25b-c66db150f9ad\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"efa6ea51-bfd9-4de2-b25b-c66db150f9ad\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}]"},{"type":"html","content":"<span class='clj-string'>&quot;89% 11% &quot;</span>","value":"\"89% 11% \""}],"value":"[[[#'classification/calculate-mean,#'classification/percentages],#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"efa6ea51-bfd9-4de2-b25b-c66db150f9ad\", :values ({:x 0, :y 89} {:x 1, :y 11})}], :marks [{:type \"rect\", :from {:data \"efa6ea51-bfd9-4de2-b25b-c66db150f9ad\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"efa6ea51-bfd9-4de2-b25b-c66db150f9ad\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"efa6ea51-bfd9-4de2-b25b-c66db150f9ad\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}],\"89% 11% \"]"}
;; <=

;; **
;;; While the results are to some degree up to interpretation, there does seem to be a strong consensus that the data point `x` is of classification 0 rather than classification 1 - which means that our inference algorithm has managed to infer approximate feature means for each classification.
;; **
