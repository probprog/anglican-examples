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
;;; A logistic regression model supposes each label @@y_{n}@@ can be predicted by a logistic transformation of a linear model. For each flower we describe the probability of it being a Setosa iris as 
;;; 
;;; $$p(y_n=1|x_n, w) = \dfrac{1}{1 + \exp\\{ -w_0 - \sum_d w_d x_n\,\hspace{-.2em}_d \\}}.$$
;;; 
;;; The parameter vector @@w@@ is a 58-long vector of weights, where _@@w_0@@_
;;; is known as an intercept or bias term. For each flower, we can define its _likelihood function_, which describes the probability of it being a Setosa or not, given the parameters and the measurements, as
;;; 
;;; $$p(y_n|x_n,w) = p_n^{y_n} (1-p_n)^{1-y_n}.$$
;;; 
;;; Here we let @@p_n@@ denote the probability of flower @@n@@ being a Setosa iris.
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
;;; We initialize @@w@@ to a vector of zeros.
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
        [(plot/list-plot (map (fn [a b] [a b]) (cons 0.0 increments) ll) 
                         :joined true :plot-size 350)
         (plot/list-plot (map (fn [a b] [a b]) (cons 0.0 increments) accuracy)
                         :joined true :plot-size 350 :plot-range [:all [0. 1.]])])
      (let [w (sgd X y f' w 10 stepsize)]
      (recur w 
             (rest incr)
             (conj ll (full-log-likelihood X y w)) 
             (conj accuracy (check-accuracy X-test y-test w thresh)))))))

(def w-init (repeat 5 0.))
(plot-sgd-output grad-log-likelihood [10 20 50 100 200 500 1000 2000 5000] w-init 0.1)
;; @@
;; ->
;;; training log-likelihood: [-55.451774444795646 -48.42755892605937 -22.094087614105025 -12.448247377736763 -9.697027842544236 -11.511230107494676 -7.041045472125804 -6.228665668052875 -5.374640357639805 -6.204894748476858] 
;;; 
;;; test set accuracy: [0.45 0.45 0.95 1.0 1.0 1.0 1.0 1.0 1.0 1.0] 
;;; 
;;; optimized weight vector: (-0.20056145911253218 -0.37832404735404895 -1.1398542204571018 1.6234236250762848 0.7155414406375851)
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"534f652b-0b38-4688-aabe-f1c5aaa8ca53","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"534f652b-0b38-4688-aabe-f1c5aaa8ca53","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"534f652b-0b38-4688-aabe-f1c5aaa8ca53"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"534f652b-0b38-4688-aabe-f1c5aaa8ca53","values":[{"x":0.0,"y":-55.451774444795646},{"x":10,"y":-48.42755892605937},{"x":20,"y":-22.094087614105025},{"x":50,"y":-12.448247377736763},{"x":100,"y":-9.697027842544236},{"x":200,"y":-11.511230107494676},{"x":500,"y":-7.041045472125804},{"x":1000,"y":-6.228665668052875},{"x":2000,"y":-5.374640357639805},{"x":5000,"y":-6.204894748476858}]}],"width":350,"height":216.31643676757812,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"534f652b-0b38-4688-aabe-f1c5aaa8ca53\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"534f652b-0b38-4688-aabe-f1c5aaa8ca53\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"534f652b-0b38-4688-aabe-f1c5aaa8ca53\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"534f652b-0b38-4688-aabe-f1c5aaa8ca53\", :values ({:x 0.0, :y -55.451774444795646} {:x 10, :y -48.42755892605937} {:x 20, :y -22.094087614105025} {:x 50, :y -12.448247377736763} {:x 100, :y -9.697027842544236} {:x 200, :y -11.511230107494676} {:x 500, :y -7.041045472125804} {:x 1000, :y -6.228665668052875} {:x 2000, :y -5.374640357639805} {:x 5000, :y -6.204894748476858})}], :width 350, :height 216.31644, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"},{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"0633f96d-7e7d-4c46-9141-250d9837794f","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0.0,1.0]}],"marks":[{"type":"line","from":{"data":"0633f96d-7e7d-4c46-9141-250d9837794f"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"0633f96d-7e7d-4c46-9141-250d9837794f","values":[{"x":0.0,"y":0.45},{"x":10,"y":0.45},{"x":20,"y":0.95},{"x":50,"y":1.0},{"x":100,"y":1.0},{"x":200,"y":1.0},{"x":500,"y":1.0},{"x":1000,"y":1.0},{"x":2000,"y":1.0},{"x":5000,"y":1.0}]}],"width":350,"height":216.31643676757812,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"0633f96d-7e7d-4c46-9141-250d9837794f\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0.0 1.0]}], :marks [{:type \"line\", :from {:data \"0633f96d-7e7d-4c46-9141-250d9837794f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"0633f96d-7e7d-4c46-9141-250d9837794f\", :values ({:x 0.0, :y 0.45} {:x 10, :y 0.45} {:x 20, :y 0.95} {:x 50, :y 1.0} {:x 100, :y 1.0} {:x 200, :y 1.0} {:x 500, :y 1.0} {:x 1000, :y 1.0} {:x 2000, :y 1.0} {:x 5000, :y 1.0})}], :width 350, :height 216.31644, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"534f652b-0b38-4688-aabe-f1c5aaa8ca53\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"534f652b-0b38-4688-aabe-f1c5aaa8ca53\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"534f652b-0b38-4688-aabe-f1c5aaa8ca53\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"534f652b-0b38-4688-aabe-f1c5aaa8ca53\", :values ({:x 0.0, :y -55.451774444795646} {:x 10, :y -48.42755892605937} {:x 20, :y -22.094087614105025} {:x 50, :y -12.448247377736763} {:x 100, :y -9.697027842544236} {:x 200, :y -11.511230107494676} {:x 500, :y -7.041045472125804} {:x 1000, :y -6.228665668052875} {:x 2000, :y -5.374640357639805} {:x 5000, :y -6.204894748476858})}], :width 350, :height 216.31644, :padding {:bottom 20, :top 10, :right 10, :left 50}}} #gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"0633f96d-7e7d-4c46-9141-250d9837794f\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0.0 1.0]}], :marks [{:type \"line\", :from {:data \"0633f96d-7e7d-4c46-9141-250d9837794f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"0633f96d-7e7d-4c46-9141-250d9837794f\", :values ({:x 0.0, :y 0.45} {:x 10, :y 0.45} {:x 20, :y 0.95} {:x 50, :y 1.0} {:x 100, :y 1.0} {:x 200, :y 1.0} {:x 500, :y 1.0} {:x 1000, :y 1.0} {:x 2000, :y 1.0} {:x 5000, :y 1.0})}], :width 350, :height 216.31644, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"}
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
(plot-sgd-output autodiff-of-likelihood [10 20 50 100 200 500 1000 2000 5000] w-init 0.1)
;; @@
;; ->
;;; training log-likelihood: [-55.451774444795646 -55.95229670395588 -41.119401709306636 -69.95282492047558 -31.379143704275286 -37.44181917159158 -19.456230157444356 -13.691876761275632 -12.572766334431174 -8.833214731404999] 
;;; 
;;; test set accuracy: [0.45 0.45 0.45 0.55 0.6 0.65 1.0 1.0 1.0 1.0] 
;;; 
;;; optimized weight vector: (-0.1577149840902522 -0.2049230280845834 -0.7985620578619811 1.3223998304712685 0.5882438494426248)
;;; 
;; <-
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"2c9c0d88-94a0-4da8-9e4a-004b2d57fb98","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"2c9c0d88-94a0-4da8-9e4a-004b2d57fb98","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"2c9c0d88-94a0-4da8-9e4a-004b2d57fb98"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"2c9c0d88-94a0-4da8-9e4a-004b2d57fb98","values":[{"x":0.0,"y":-55.451774444795646},{"x":10,"y":-55.95229670395588},{"x":20,"y":-41.119401709306636},{"x":50,"y":-69.95282492047558},{"x":100,"y":-31.379143704275286},{"x":200,"y":-37.44181917159158},{"x":500,"y":-19.456230157444356},{"x":1000,"y":-13.691876761275632},{"x":2000,"y":-12.572766334431174},{"x":5000,"y":-8.833214731404999}]}],"width":350,"height":216.31643676757812,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"2c9c0d88-94a0-4da8-9e4a-004b2d57fb98\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"2c9c0d88-94a0-4da8-9e4a-004b2d57fb98\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"2c9c0d88-94a0-4da8-9e4a-004b2d57fb98\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"2c9c0d88-94a0-4da8-9e4a-004b2d57fb98\", :values ({:x 0.0, :y -55.451774444795646} {:x 10, :y -55.95229670395588} {:x 20, :y -41.119401709306636} {:x 50, :y -69.95282492047558} {:x 100, :y -31.379143704275286} {:x 200, :y -37.44181917159158} {:x 500, :y -19.456230157444356} {:x 1000, :y -13.691876761275632} {:x 2000, :y -12.572766334431174} {:x 5000, :y -8.833214731404999})}], :width 350, :height 216.31644, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"},{"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"4c792c32-46fb-40eb-81c4-f2d1f8ed5f9f","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0.0,1.0]}],"marks":[{"type":"line","from":{"data":"4c792c32-46fb-40eb-81c4-f2d1f8ed5f9f"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"stroke":{"value":"#FF29D2"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"4c792c32-46fb-40eb-81c4-f2d1f8ed5f9f","values":[{"x":0.0,"y":0.45},{"x":10,"y":0.45},{"x":20,"y":0.45},{"x":50,"y":0.55},{"x":100,"y":0.6},{"x":200,"y":0.65},{"x":500,"y":1.0},{"x":1000,"y":1.0},{"x":2000,"y":1.0},{"x":5000,"y":1.0}]}],"width":350,"height":216.31643676757812,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"4c792c32-46fb-40eb-81c4-f2d1f8ed5f9f\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0.0 1.0]}], :marks [{:type \"line\", :from {:data \"4c792c32-46fb-40eb-81c4-f2d1f8ed5f9f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"4c792c32-46fb-40eb-81c4-f2d1f8ed5f9f\", :values ({:x 0.0, :y 0.45} {:x 10, :y 0.45} {:x 20, :y 0.45} {:x 50, :y 0.55} {:x 100, :y 0.6} {:x 200, :y 0.65} {:x 500, :y 1.0} {:x 1000, :y 1.0} {:x 2000, :y 1.0} {:x 5000, :y 1.0})}], :width 350, :height 216.31644, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}],"value":"[#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"2c9c0d88-94a0-4da8-9e4a-004b2d57fb98\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"2c9c0d88-94a0-4da8-9e4a-004b2d57fb98\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"2c9c0d88-94a0-4da8-9e4a-004b2d57fb98\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"2c9c0d88-94a0-4da8-9e4a-004b2d57fb98\", :values ({:x 0.0, :y -55.451774444795646} {:x 10, :y -55.95229670395588} {:x 20, :y -41.119401709306636} {:x 50, :y -69.95282492047558} {:x 100, :y -31.379143704275286} {:x 200, :y -37.44181917159158} {:x 500, :y -19.456230157444356} {:x 1000, :y -13.691876761275632} {:x 2000, :y -12.572766334431174} {:x 5000, :y -8.833214731404999})}], :width 350, :height 216.31644, :padding {:bottom 20, :top 10, :right 10, :left 50}}} #gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"4c792c32-46fb-40eb-81c4-f2d1f8ed5f9f\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0.0 1.0]}], :marks [{:type \"line\", :from {:data \"4c792c32-46fb-40eb-81c4-f2d1f8ed5f9f\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :stroke {:value \"#FF29D2\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"4c792c32-46fb-40eb-81c4-f2d1f8ed5f9f\", :values ({:x 0.0, :y 0.45} {:x 10, :y 0.45} {:x 20, :y 0.45} {:x 50, :y 0.55} {:x 100, :y 0.6} {:x 200, :y 0.65} {:x 500, :y 1.0} {:x 1000, :y 1.0} {:x 2000, :y 1.0} {:x 5000, :y 1.0})}], :width 350, :height 216.31644, :padding {:bottom 20, :top 10, :right 10, :left 50}}}]"}
;; <=
