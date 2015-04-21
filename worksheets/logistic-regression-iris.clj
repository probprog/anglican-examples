;; gorilla-repl.fileformat = 1

;; **
;;; # Maximum likelihood estimation
;; **

;; @@
(ns maximum-likelihood-dagstuhl
  (:refer-clojure :exclude [= * + - / == < > <= >= zero? number? pos? neg?])
  (:require [gorilla-plot.core :as plot]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [clojure.core.matrix :as m]
            [ad.core :refer :all]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; We're going to train a logistic regression model to distinguish between two varieties of Iris flowers.
;;; 
;;; We will use the "iris" dataset, available at the UCI machine learning repository of datasets.
;;; The dataset contains 4 measurements for 150 different Iris flowers, of three different types ("Setosa", "Versicolor", and "Virginica").
;;; For more information on the "iris" dataset, see the [dataset description page](https://archive.ics.uci.edu/ml/datasets/Iris).
;;; 
;;; We begin by reading the dataset from disk, and separating into a 80/20 split for training and testing.
;;; There are 50 examples of each Iris type; we will focus only on determining between two varieties, Setosa and Versicolor.
;; **

;; @@
(def iris-data
  (loop [X []
         y []
         data (shuffle (take 100 (with-open [file (io/reader "data/iris.csv")]
                         (doall (csv/read-csv file)))))]
    (def row (first data))
    (if (nil? row)
      [X y]
      (recur (conj X (map #(Float/parseFloat %) (take (dec (count row)) row)))
             (conj y (keyword (peek row)))
             (rest data)))))


(def train-points (int (* (count (get iris-data 0)) 0.8)))

(def X (m/matrix (take train-points (get iris-data 0))))
(def y (m/matrix (take train-points (get iris-data 1))))

(def X-test (m/matrix (drop train-points (get iris-data 0))))
(def y-test (m/matrix (drop train-points (get iris-data 1))))

(println "Number of training examples:" (count y))
(println "Number of test examples:" (count y-test))

(println "\nVarieties of Iris:" (into [] (into #{} y)))
;; @@
;; ->
;;; Number of training examples: 80
;;; Number of test examples: 20
;;; 
;;; Varieties of Iris: [:Iris-versicolor :Iris-setosa]
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Here @@X@@ is a @@80\times 4@@ matrix of real-valued features, and @@y@@ is a vector of keyword labels with the name of the flower, here either `:Iris-setosa` or `:Iris-versicolor` <!---, or `:Iris-virginica`-->. We are going to try to train a classifier to identify the _Setosa_ irises.
;;; 
;;; A logistic regression model supposes each label @@y_{n}@@ can be predicted by a logistic transformation of a linear model. For each email we describe the probability of it being spam as 
;;; 
;;; $$p(y_n=1|x_n, w) = \dfrac{1}{1 + \exp\\{ -w_0 - \sum_d w_d x_n\,\hspace{-.2em}_d \\}}.$$
;;; 
;;; The parameter vector @@w@@ is a 58-long vector of weights, where _@@w_0@@_
;;; is known as an intercept or bias term. For each email, we can define its _likelihood function_, which describes the probability of the email being either spam or not, given the parameters and the feature set, as
;;; 
;;; $$p(y_n|x_n,w) = p_n^{y_n} (1-p_n)^{1-y_n}.$$
;;; 
;;; Here we let @@p_n@@ denote the probability of email @@n@@ being spam, as defined above.
;;; We often find it convenient to work with the log of the likelihood function, rather than the likelihood function itself.
;;; The full-data log-likelihood function
;;; 
;;; $$\mathcal{L}(X,y,w) = \sum_{n=1}^N \log p(y_n|x_n,w)$$
;;; 
;;; is defined as a sum over the individual data-point log-likelihoods.
;;; These are both defined as clojure functions in the following cell.
;; **

;; @@
(defn log-likelihood
  [x-n y-n w]
  (let [p-n (/ 1.0 (+ 1.0 (exp (- (reduce + (map * (cons 1.0 x-n) w))))))]
    (log (if (= y-n :Iris-setosa) p-n (- 1 p-n)))))


(defn full-log-likelihood
  [X y w]
  (reduce + (map (fn [x-n y-n] (log-likelihood x-n y-n w)) X y)))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;maximum-likelihood-dagstuhl/full-log-likelihood</span>","value":"#'maximum-likelihood-dagstuhl/full-log-likelihood"}
;; <=

;; **
;;; ## Gradient-based optimization
;;; 
;;; If the log-likelihood function for a particular data point looks like
;;; 
;;; $$\log p(y_n|x_n,w) = y_n \log p_n + (1-y_n) \log (1-p_n),$$
;;; 
;;; then we can compute the gradient of the log-likelihood @@\nabla_w \log p(y_n|x_n,w)@@,
;;; a @@D@@-vector with each entry
;;; 
;;; $$(\nabla\_w \log p(y\_n|x\_n,w))\_d = (y\_n - p\_n) x\_{nd}$$
;;; 
;;; where (for convenience) we define @@x\_{n0} = 1.0@@ for all @@n@@.
;;; The weight _@@w_0@@_ 
;;; is variously known as an _intercept_ or _bias_ term.
;; **

;; @@
(defn grad-log-likelihood
  [x-n y-n w]
  (let [y-value (if (= y-n :Iris-setosa) 1. 0.)
        phi (cons 1.0 x-n)
        C (exp (- (reduce + (map * phi w))))
        p-n (/ 1.0 (+ 1.0 C))]
    (map #(* (- y-value p-n) %) phi))) 
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;maximum-likelihood-dagstuhl/grad-log-likelihood</span>","value":"#'maximum-likelihood-dagstuhl/grad-log-likelihood"}
;; <=

;; **
;;; Define functions for checking the quality of our estimated @@w@@, by predicting the variety of the flower in the held-out test set
;; **

;; @@
(defn predict [X w]
  (map (fn [x-n] 
         (/ 1.0 (+ 1.0 (exp (- (reduce + (map * (cons 1.0 x-n) w)))))))
       X))

(defn check-accuracy [X y w threshold]
  (let [p (predict X w)
        predictions (map #(if (> % threshold) :Iris-setosa :Iris-versicolor) p)
        correct (map #(if (= %1 %2) 1.0 0.0) predictions y)]
    (/ (reduce + correct) (count y))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;maximum-likelihood-dagstuhl/check-accuracy</span>","value":"#'maximum-likelihood-dagstuhl/check-accuracy"}
;; <=

;; **
;;; A very simple algorithm for optimizing @@w@@ is [stochastic gradient descent](http://en.wikipedia.org/wiki/Stochastic_gradient_descent)(SGD).
;;; 
;;; We set a _learning rate_ or step size which we denote @@\alpha@@, and at each iteration of the algorithm we update our estimate of @@w@@ with
;;; 
;;; $$w \leftarrow w + \alpha \nabla_w \log p(y|X,w)$$
;;; 
;;; using the gradients we defined above.
;;; At each update, computing the gradient using all data points @@\\{x_n, y_n\\}@@ may be costly,
;;; so instead we estimate noisy gradients from only a single data point, which we sample at random from the full dataset.
;; **

;; @@
(defn sgd [X y f' w-init num-iters stepsize]
  (loop [w w-init
         num-iters num-iters]
    (if (= num-iters 0)
      w
      (let [n (rand-int (count y))
            gradient (f' (get X n) (get y n) w)
            w (map - w (map #(* stepsize %) gradient))]
        (recur w (dec num-iters))))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;maximum-likelihood-dagstuhl/sgd</span>","value":"#'maximum-likelihood-dagstuhl/sgd"}
;; <=

;; **
;;; We can test the algorithm by running a few hundred iterations of stochastic gradient descent, and observing what happens to the full-data log-likelihood function (it should increase --- we are maximizing it!) and what happens to our accuracy on the held-out test-set using the current estimate of @@w@@.
;;; 
;;; We initialize @@w@@ to a random vector with values between -1 and 1.
;; **

;; @@
(def thresh 0.5)

(defn plot-sgd-output [f' increments w-init stepsize]
  (loop [w w-init
         incr increments
         ll [(full-log-likelihood X y w)]
         accuracy [(check-accuracy X-test y-test w thresh)]]
    (if (empty? incr)
      (do
        (println "training log-likelihood:" ll "\n")
        (println "test set accuracy:" accuracy "\n")
        (println "optimized weight vector:" w)
        [(plot/list-plot (map (fn [a b] [a b]) (cons 0.0 increments) ll) :joined true)
         (plot/list-plot (map (fn [a b] [a b]) (cons 0.0 increments) accuracy) :joined true)])
      (let [w (sgd X y f' w 10 stepsize)]
      (recur w 
             (rest incr)
             (conj ll (full-log-likelihood X y w)) 
             (conj accuracy (check-accuracy X-test y-test w thresh)))))))

(def w-init (repeatedly 5 #(- (* 2 (rand)) 1)))
(plot-sgd-output grad-log-likelihood [10 20 50 100 200 500 1000 2000] w-init 0.1)
;; @@
;; ->
;;; training log-likelihood: [-43.805250600425715 -41.26661759367229 -20.67338833044083 -16.40887679369888 -8.708298092754562 -7.155029569392148 -5.8314361817714975 -5.446368882735079 -5.131905996764995] 
;;; 
;;; test set accuracy: [0.65 0.65 1.0 0.95 1.0 1.0 1.0 1.0 1.0] 
;;; 
;;; optimized weight vector: (-1.013744332797086 -0.9608821473405438 -0.13487491129201146 1.865723864624998 1.2978802054804992)
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"2fb58575-65fb-4c9f-be81-45f587d2b166","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"2fb58575-65fb-4c9f-be81-45f587d2b166","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"2fb58575-65fb-4c9f-be81-45f587d2b166"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"2fb58575-65fb-4c9f-be81-45f587d2b166","values":[{"x":0.0,"y":-43.805250600425715},{"x":10,"y":-41.26661759367229},{"x":20,"y":-20.67338833044083},{"x":50,"y":-16.40887679369888},{"x":100,"y":-8.708298092754562},{"x":200,"y":-7.155029569392148},{"x":500,"y":-5.8314361817714975},{"x":1000,"y":-5.446368882735079},{"x":2000,"y":-5.131905996764995}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"2fb58575-65fb-4c9f-be81-45f587d2b166\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"2fb58575-65fb-4c9f-be81-45f587d2b166\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"2fb58575-65fb-4c9f-be81-45f587d2b166\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"2fb58575-65fb-4c9f-be81-45f587d2b166\", :values ({:x 0.0, :y -43.805250600425715} {:x 10, :y -41.26661759367229} {:x 20, :y -20.67338833044083} {:x 50, :y -16.40887679369888} {:x 100, :y -8.708298092754562} {:x 200, :y -7.155029569392148} {:x 500, :y -5.8314361817714975} {:x 1000, :y -5.446368882735079} {:x 2000, :y -5.131905996764995})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"},{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"9ff9525e-aeaf-4686-b93e-ae62355eae90","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"9ff9525e-aeaf-4686-b93e-ae62355eae90","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"9ff9525e-aeaf-4686-b93e-ae62355eae90"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"9ff9525e-aeaf-4686-b93e-ae62355eae90","values":[{"x":0.0,"y":0.65},{"x":10,"y":0.65},{"x":20,"y":1.0},{"x":50,"y":0.95},{"x":100,"y":1.0},{"x":200,"y":1.0},{"x":500,"y":1.0},{"x":1000,"y":1.0},{"x":2000,"y":1.0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"9ff9525e-aeaf-4686-b93e-ae62355eae90\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"9ff9525e-aeaf-4686-b93e-ae62355eae90\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"9ff9525e-aeaf-4686-b93e-ae62355eae90\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"9ff9525e-aeaf-4686-b93e-ae62355eae90\", :values ({:x 0.0, :y 0.65} {:x 10, :y 0.65} {:x 20, :y 1.0} {:x 50, :y 0.95} {:x 100, :y 1.0} {:x 200, :y 1.0} {:x 500, :y 1.0} {:x 1000, :y 1.0} {:x 2000, :y 1.0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"2fb58575-65fb-4c9f-be81-45f587d2b166\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"2fb58575-65fb-4c9f-be81-45f587d2b166\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"2fb58575-65fb-4c9f-be81-45f587d2b166\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"2fb58575-65fb-4c9f-be81-45f587d2b166\", :values ({:x 0.0, :y -43.805250600425715} {:x 10, :y -41.26661759367229} {:x 20, :y -20.67338833044083} {:x 50, :y -16.40887679369888} {:x 100, :y -8.708298092754562} {:x 200, :y -7.155029569392148} {:x 500, :y -5.8314361817714975} {:x 1000, :y -5.446368882735079} {:x 2000, :y -5.131905996764995})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}} #gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"9ff9525e-aeaf-4686-b93e-ae62355eae90\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"9ff9525e-aeaf-4686-b93e-ae62355eae90\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"9ff9525e-aeaf-4686-b93e-ae62355eae90\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"9ff9525e-aeaf-4686-b93e-ae62355eae90\", :values ({:x 0.0, :y 0.65} {:x 10, :y 0.65} {:x 20, :y 1.0} {:x 50, :y 0.95} {:x 100, :y 1.0} {:x 200, :y 1.0} {:x 500, :y 1.0} {:x 1000, :y 1.0} {:x 2000, :y 1.0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"}
;; <=

;; **
;;; ## Autodiff
;;; 
;;; Instead of explicitly computing gradients, we can use automatic differentiation. [Automatic differentiation](http://en.wikipedia.org/wiki/Automatic_differentiation) is a non-standard program execution, using dual numbers to compute derivatives.
;;; 
;;; For details on the implementation of automatic differentiation, see either [clj-auto-diff](https://github.com/log0ymxm/clj-auto-diff) or [R6RS-AD](https://github.com/qobi/R6RS-AD).
;;; 
;;; The autodiff utility can perform this on the likelihood function itself (there is no need to manually take logarithms to put the function in a form amenable to human differentiation). 
;;; We simply define the likelihood function, and then create a gradient function `autodiff-of-likelihood`, which computes the gradient by directly applying the `gradient-vector-F` operator to our likelihood function.
;; **

;; @@
(defn likelihood
  [x-n y-n w]
  (let [y-n (if (= y-n :Iris-setosa) 1.0 0.0)
        p-n (/ 1.0 (+ 1.0 (exp (- (reduce + (map * (cons 1.0 x-n) w))))))]
	(* (expt p-n y-n) (expt (- 1 p-n) (- 1 y-n)))))


(defn autodiff-of-likelihood
  [x_n y_n w]
  ((gradient-vector-F (partial likelihood x_n y_n)) w))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;maximum-likelihood-dagstuhl/autodiff-of-likelihood</span>","value":"#'maximum-likelihood-dagstuhl/autodiff-of-likelihood"}
;; <=

;; **
;;; We can repeat the stochastic gradient optimization procedure we ran above, using our gradient found via automatic differentiation.
;; **

;; @@
(plot-sgd-output autodiff-of-likelihood [10 20 50 100 200 500 1000 2000] w-init 0.1)
;; @@
;; ->
;;; training log-likelihood: [-43.805250600425715 -24.230848260137144 -25.922839780169 -21.72885124699073 -27.96163017251892 -10.835997439412935 -8.688697816942621 -8.352268319575789 -7.478968458373297] 
;;; 
;;; test set accuracy: [0.65 1.0 0.95 0.85 0.9 1.0 1.0 1.0 1.0] 
;;; 
;;; optimized weight vector: (-0.9723497900052372 -0.8323175426615955 0.10806985840770471 1.6729327392857976 1.2627495564453468)
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"3c4c3e8f-88e0-4bab-8c26-513ec946dcd7","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"3c4c3e8f-88e0-4bab-8c26-513ec946dcd7","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"3c4c3e8f-88e0-4bab-8c26-513ec946dcd7"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"3c4c3e8f-88e0-4bab-8c26-513ec946dcd7","values":[{"x":0.0,"y":-43.805250600425715},{"x":10,"y":-24.230848260137144},{"x":20,"y":-25.922839780169},{"x":50,"y":-21.72885124699073},{"x":100,"y":-27.96163017251892},{"x":200,"y":-10.835997439412935},{"x":500,"y":-8.688697816942621},{"x":1000,"y":-8.352268319575789},{"x":2000,"y":-7.478968458373297}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"3c4c3e8f-88e0-4bab-8c26-513ec946dcd7\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"3c4c3e8f-88e0-4bab-8c26-513ec946dcd7\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"3c4c3e8f-88e0-4bab-8c26-513ec946dcd7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"3c4c3e8f-88e0-4bab-8c26-513ec946dcd7\", :values ({:x 0.0, :y -43.805250600425715} {:x 10, :y -24.230848260137144} {:x 20, :y -25.922839780169} {:x 50, :y -21.72885124699073} {:x 100, :y -27.96163017251892} {:x 200, :y -10.835997439412935} {:x 500, :y -8.688697816942621} {:x 1000, :y -8.352268319575789} {:x 2000, :y -7.478968458373297})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"},{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"ae96678c-945d-4a97-bb6f-c972d1593f8d","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"ae96678c-945d-4a97-bb6f-c972d1593f8d","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"ae96678c-945d-4a97-bb6f-c972d1593f8d"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"ae96678c-945d-4a97-bb6f-c972d1593f8d","values":[{"x":0.0,"y":0.65},{"x":10,"y":1.0},{"x":20,"y":0.95},{"x":50,"y":0.85},{"x":100,"y":0.9},{"x":200,"y":1.0},{"x":500,"y":1.0},{"x":1000,"y":1.0},{"x":2000,"y":1.0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"ae96678c-945d-4a97-bb6f-c972d1593f8d\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"ae96678c-945d-4a97-bb6f-c972d1593f8d\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"ae96678c-945d-4a97-bb6f-c972d1593f8d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"ae96678c-945d-4a97-bb6f-c972d1593f8d\", :values ({:x 0.0, :y 0.65} {:x 10, :y 1.0} {:x 20, :y 0.95} {:x 50, :y 0.85} {:x 100, :y 0.9} {:x 200, :y 1.0} {:x 500, :y 1.0} {:x 1000, :y 1.0} {:x 2000, :y 1.0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"3c4c3e8f-88e0-4bab-8c26-513ec946dcd7\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"3c4c3e8f-88e0-4bab-8c26-513ec946dcd7\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"3c4c3e8f-88e0-4bab-8c26-513ec946dcd7\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"3c4c3e8f-88e0-4bab-8c26-513ec946dcd7\", :values ({:x 0.0, :y -43.805250600425715} {:x 10, :y -24.230848260137144} {:x 20, :y -25.922839780169} {:x 50, :y -21.72885124699073} {:x 100, :y -27.96163017251892} {:x 200, :y -10.835997439412935} {:x 500, :y -8.688697816942621} {:x 1000, :y -8.352268319575789} {:x 2000, :y -7.478968458373297})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}} #gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"ae96678c-945d-4a97-bb6f-c972d1593f8d\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"ae96678c-945d-4a97-bb6f-c972d1593f8d\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"ae96678c-945d-4a97-bb6f-c972d1593f8d\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"ae96678c-945d-4a97-bb6f-c972d1593f8d\", :values ({:x 0.0, :y 0.65} {:x 10, :y 1.0} {:x 20, :y 0.95} {:x 50, :y 0.85} {:x 100, :y 0.9} {:x 200, :y 1.0} {:x 500, :y 1.0} {:x 1000, :y 1.0} {:x 2000, :y 1.0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"}
;; <=

;; @@

;; @@
