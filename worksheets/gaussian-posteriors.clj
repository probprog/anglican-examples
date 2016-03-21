;; gorilla-repl.fileformat = 1

;; **
;;; # Posterior estimation with Gaussians
;;; 
;;; Authors:
;;; 
;;;  - Brooks Paige [brooks@robots.ox.ac.uk](mailto:brooks@robots.ox.ac.uk)
;;;  - Frank Wood [fwood@robots.ox.ac.uk](mailto:fwood@robots.ox.ac.uk)
;;;  
;;; This worksheet is based on an exercise from a [practical](https://bitbucket.org/probprog/mlss2015/) at MLSS 2015 in Tübingen.
;; **

;; @@
(ns gaussian-estimation
  (:require [anglican importance pgibbs]
            [anglican.stat :as stat]
            [gorilla-plot.core :as plot])
  (:use [anglican core emit runtime]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; We can use Anglican for posterior estimation in Bayesian models.
;;; 
;;; Suppose we are trying to estimate the mean of a Gaussian distribution, given some observed data @@y\_i@@.
;;; We'll assume that the variance is known, and focus on learning the posterior distribution of the mean @@\mu@@.
;;; We put a Gaussian prior on @@\mu@@, yielding a model
;;; 
;;; $$\begin{align}
;;; \sigma^2 &= 2 \\\\
;;; \mu &\sim \mathrm{Normal}(1, \sqrt 5) \\\\
;;; y\_i|\mu &\sim \mathrm{Normal}(\mu, \sigma).
;;; \end{align}$$
;;; 
;;; Now suppose we observe two data points, @@y\_1 = 9@@ and @@y\_2 = 8@@.
;;; This will be passed as an input to the query as a vector with two elements.
;; **

;; @@
(def dataset [9 8])
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gaussian-estimation/dataset</span>","value":"#'gaussian-estimation/dataset"}
;; <=

;; **
;;; We can write this model as a simple Anglican program:
;; **

;; @@
(defquery gaussian-model [data]
  (let [mu (sample (normal 1 (sqrt 5)))
        sigma (sqrt 2)]
    (doall (map (fn [x] (observe (normal mu sigma) x)) data))
    (predict :mu mu)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gaussian-estimation/gaussian-model</span>","value":"#'gaussian-estimation/gaussian-model"}
;; <=

;; **
;;; The following code creates a distribution object which represents the posterior distribution, conditioned on the supplied `dataset`, using the particle Gibbs inference algorithm.
;; **

;; @@
(def posterior 
  ((conditional gaussian-model :pgibbs :number-of-particles 1000) dataset))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gaussian-estimation/posterior</span>","value":"#'gaussian-estimation/posterior"}
;; <=

;; **
;;; The following line now draws 20,000 samples from the posterior distribution.
;; **

;; @@
(def posterior-samples (repeatedly 20000 #(sample posterior)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gaussian-estimation/posterior-samples</span>","value":"#'gaussian-estimation/posterior-samples"}
;; <=

;; **
;;; We can plot a histogram of these samples to see the posterior.
;;; 
;;; Here we have chosen the [conjugate prior](http://en.wikipedia.org/wiki/Conjugate_prior) for @@\mu@@, making this a rare model in that we can actually compute the posterior distribution analytically
;;; &mdash; when we run our sampler, we expect to find
;;; 
;;; $$\begin{align}
;;; \mu|y\_{1:2} &\sim \mathrm{Normal}(7.25, 0.9129).
;;; \end{align}$$
;;; 
;;; (We can also draw samples from the prior distribution @@\mathrm{Normal}(1,\sqrt 5)@@, to see how the posterior differs from the prior.)
;; **

;; @@
(def prior-samples (repeatedly 20000 #(sample (normal 1 (sqrt 5)))))


(println "Prior on mu (blue) and posterior (green)")
(plot/compose
 (plot/histogram prior-samples
                 :normalize :probability-density :bins 40
                 :plot-range [[-10 10] [0 0.8]])
 (plot/histogram (map :mu posterior-samples)
                 :normalize :probability-density :bins 40
                 :color :green))
;; @@
;; ->
;;; Prior on mu (blue) and posterior (green)
;;; 
;; <-
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-10,10]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,0.8]}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"d1c50038-0dcb-4d1f-a119-3e5a4f1ac34a","values":[{"x":-10.0,"y":0},{"x":-9.5,"y":0.0},{"x":-9.0,"y":0.0},{"x":-8.5,"y":0.0},{"x":-8.0,"y":0.0},{"x":-7.5,"y":0.0},{"x":-7.0,"y":4.0E-4},{"x":-6.5,"y":2.0E-4},{"x":-6.0,"y":9.0E-4},{"x":-5.5,"y":0.0019},{"x":-5.0,"y":0.0043},{"x":-4.5,"y":0.0069},{"x":-4.0,"y":0.0095},{"x":-3.5,"y":0.0183},{"x":-3.0,"y":0.0299},{"x":-2.5,"y":0.0425},{"x":-2.0,"y":0.0589},{"x":-1.5,"y":0.0881},{"x":-1.0,"y":0.1148},{"x":-0.5,"y":0.1375},{"x":0.0,"y":0.1502},{"x":0.5,"y":0.1694},{"x":1.0,"y":0.1758},{"x":1.5,"y":0.1794},{"x":2.0,"y":0.1673},{"x":2.5,"y":0.1546},{"x":3.0,"y":0.1281},{"x":3.5,"y":0.1063},{"x":4.0,"y":0.0772},{"x":4.5,"y":0.0594},{"x":5.0,"y":0.0434},{"x":5.5,"y":0.0322},{"x":6.0,"y":0.0187},{"x":6.5,"y":0.0113},{"x":7.0,"y":0.0066},{"x":7.5,"y":0.0026},{"x":8.0,"y":9.0E-4},{"x":8.5,"y":0.0014},{"x":9.0,"y":7.0E-4},{"x":9.5,"y":2.0E-4},{"x":10.0,"y":1.0E-4},{"x":10.5,"y":1.0E-4},{"x":11.0,"y":0}]},{"name":"a68cbb3d-f70e-4172-9bea-5741896a7ff8","values":[{"x":3.906970567760844,"y":0},{"x":4.069890361266436,"y":6.137989611223462E-4},{"x":4.232810154772029,"y":0.0018413968833670387},{"x":4.395729948277621,"y":0.002148296363928212},{"x":4.5586497417832135,"y":0.0036827937667340774},{"x":4.721569535288806,"y":0.010127682858518713},{"x":4.884489328794398,"y":0.011048381300202232},{"x":5.047409122299991,"y":0.021482963639282118},{"x":5.210328915805583,"y":0.02670025480882206},{"x":5.373248709311175,"y":0.04265902779800306},{"x":5.536168502816768,"y":0.05984539870942876},{"x":5.69908829632236,"y":0.09667333637676953},{"x":5.8620080898279525,"y":0.10833551663809411},{"x":6.024927883333545,"y":0.15222214235834186},{"x":6.187847676839137,"y":0.12613568651064216},{"x":6.35076747034473,"y":0.2065433504176695},{"x":6.513687263850322,"y":0.3010683904305108},{"x":6.676607057355914,"y":0.3480240109563703},{"x":6.839526850861507,"y":0.5214222174734331},{"x":7.002446644367099,"y":0.5373809904626141},{"x":7.165366437872692,"y":0.25441966938521254},{"x":7.328286231378284,"y":0.3188685603030589},{"x":7.491206024883876,"y":0.5978401881331652},{"x":7.654125818389469,"y":0.283575120038524},{"x":7.817045611895061,"y":0.42751097642171415},{"x":7.9799654054006535,"y":0.8869394988217904},{"x":8.142885198906246,"y":0.06997308156794747},{"x":8.305804992411838,"y":0.10250442650743183},{"x":8.46872478591743,"y":0.28050612523291224},{"x":8.631644579423023,"y":0.07703176962085445},{"x":8.794564372928615,"y":0.09605953741564718},{"x":8.957484166434208,"y":0.04818321844810418},{"x":9.1204039599398,"y":0.0},{"x":9.283323753445393,"y":0.0},{"x":9.446243546950985,"y":0.0},{"x":9.609163340456577,"y":0.0},{"x":9.77208313396217,"y":0.0},{"x":9.935002927467762,"y":0.11048381300202233},{"x":10.097922720973354,"y":0.0},{"x":10.260842514478947,"y":0.0},{"x":10.42376230798454,"y":0.0},{"x":10.586682101490132,"y":0.006137989611223462},{"x":10.749601894995724,"y":0}]}],"marks":[{"type":"line","from":{"data":"d1c50038-0dcb-4d1f-a119-3e5a4f1ac34a"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"a68cbb3d-f70e-4172-9bea-5741896a7ff8"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"green"},"fillOpacity":{"value":0.4},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-10 10]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 0.8]}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"d1c50038-0dcb-4d1f-a119-3e5a4f1ac34a\", :values ({:x -10.0, :y 0} {:x -9.5, :y 0.0} {:x -9.0, :y 0.0} {:x -8.5, :y 0.0} {:x -8.0, :y 0.0} {:x -7.5, :y 0.0} {:x -7.0, :y 4.0E-4} {:x -6.5, :y 2.0E-4} {:x -6.0, :y 9.0E-4} {:x -5.5, :y 0.0019} {:x -5.0, :y 0.0043} {:x -4.5, :y 0.0069} {:x -4.0, :y 0.0095} {:x -3.5, :y 0.0183} {:x -3.0, :y 0.0299} {:x -2.5, :y 0.0425} {:x -2.0, :y 0.0589} {:x -1.5, :y 0.0881} {:x -1.0, :y 0.1148} {:x -0.5, :y 0.1375} {:x 0.0, :y 0.1502} {:x 0.5, :y 0.1694} {:x 1.0, :y 0.1758} {:x 1.5, :y 0.1794} {:x 2.0, :y 0.1673} {:x 2.5, :y 0.1546} {:x 3.0, :y 0.1281} {:x 3.5, :y 0.1063} {:x 4.0, :y 0.0772} {:x 4.5, :y 0.0594} {:x 5.0, :y 0.0434} {:x 5.5, :y 0.0322} {:x 6.0, :y 0.0187} {:x 6.5, :y 0.0113} {:x 7.0, :y 0.0066} {:x 7.5, :y 0.0026} {:x 8.0, :y 9.0E-4} {:x 8.5, :y 0.0014} {:x 9.0, :y 7.0E-4} {:x 9.5, :y 2.0E-4} {:x 10.0, :y 1.0E-4} {:x 10.5, :y 1.0E-4} {:x 11.0, :y 0})} {:name \"a68cbb3d-f70e-4172-9bea-5741896a7ff8\", :values ({:x 3.906970567760844, :y 0} {:x 4.069890361266436, :y 6.137989611223462E-4} {:x 4.232810154772029, :y 0.0018413968833670387} {:x 4.395729948277621, :y 0.002148296363928212} {:x 4.5586497417832135, :y 0.0036827937667340774} {:x 4.721569535288806, :y 0.010127682858518713} {:x 4.884489328794398, :y 0.011048381300202232} {:x 5.047409122299991, :y 0.021482963639282118} {:x 5.210328915805583, :y 0.02670025480882206} {:x 5.373248709311175, :y 0.04265902779800306} {:x 5.536168502816768, :y 0.05984539870942876} {:x 5.69908829632236, :y 0.09667333637676953} {:x 5.8620080898279525, :y 0.10833551663809411} {:x 6.024927883333545, :y 0.15222214235834186} {:x 6.187847676839137, :y 0.12613568651064216} {:x 6.35076747034473, :y 0.2065433504176695} {:x 6.513687263850322, :y 0.3010683904305108} {:x 6.676607057355914, :y 0.3480240109563703} {:x 6.839526850861507, :y 0.5214222174734331} {:x 7.002446644367099, :y 0.5373809904626141} {:x 7.165366437872692, :y 0.25441966938521254} {:x 7.328286231378284, :y 0.3188685603030589} {:x 7.491206024883876, :y 0.5978401881331652} {:x 7.654125818389469, :y 0.283575120038524} {:x 7.817045611895061, :y 0.42751097642171415} {:x 7.9799654054006535, :y 0.8869394988217904} {:x 8.142885198906246, :y 0.06997308156794747} {:x 8.305804992411838, :y 0.10250442650743183} {:x 8.46872478591743, :y 0.28050612523291224} {:x 8.631644579423023, :y 0.07703176962085445} {:x 8.794564372928615, :y 0.09605953741564718} {:x 8.957484166434208, :y 0.04818321844810418} {:x 9.1204039599398, :y 0.0} {:x 9.283323753445393, :y 0.0} {:x 9.446243546950985, :y 0.0} {:x 9.609163340456577, :y 0.0} {:x 9.77208313396217, :y 0.0} {:x 9.935002927467762, :y 0.11048381300202233} {:x 10.097922720973354, :y 0.0} {:x 10.260842514478947, :y 0.0} {:x 10.42376230798454, :y 0.0} {:x 10.586682101490132, :y 0.006137989611223462} {:x 10.749601894995724, :y 0})}), :marks ({:type \"line\", :from {:data \"d1c50038-0dcb-4d1f-a119-3e5a4f1ac34a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"a68cbb3d-f70e-4172-9bea-5741896a7ff8\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value :green}, :fillOpacity {:value 0.4}, :stroke {:value :green}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; **
;;; # The seven scientists
;;; 
;;; Here's an interesting variation on estimating the mean of a Gaussian. This example is due to [MacKay 2003, exercise 22.15] and [Lee & Wagenmaker 2013, section 4.2].
;;; 
;;; Suppose seven scientists all go and perform the same experiment, each collecting a measurement @@x\_i@@ for @@i = 1,\dots,7@@. 
;;; 
;;; These scientists are varyingly good at their job, and while we can assume each scientist would estimate @@x@@ correctly _on average_, some of them may have much more error in their measurements than others.
;;; 
;;; They come back with the following seven observations:
;; **

;; @@
(def measurements [-27.020 3.570 8.191 9.898 9.603 9.945 10.056])

(plot/bar-chart (range 1 8) measurements)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"120b87cc-8f9b-40c3-b8f9-91b0ef3f325e","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"120b87cc-8f9b-40c3-b8f9-91b0ef3f325e","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"120b87cc-8f9b-40c3-b8f9-91b0ef3f325e"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"120b87cc-8f9b-40c3-b8f9-91b0ef3f325e","values":[{"x":1,"y":-27.02},{"x":2,"y":3.57},{"x":3,"y":8.191},{"x":4,"y":9.898},{"x":5,"y":9.603},{"x":6,"y":9.945},{"x":7,"y":10.056}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"120b87cc-8f9b-40c3-b8f9-91b0ef3f325e\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"120b87cc-8f9b-40c3-b8f9-91b0ef3f325e\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"120b87cc-8f9b-40c3-b8f9-91b0ef3f325e\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"120b87cc-8f9b-40c3-b8f9-91b0ef3f325e\", :values ({:x 1, :y -27.02} {:x 2, :y 3.57} {:x 3, :y 8.191} {:x 4, :y 9.898} {:x 5, :y 9.603} {:x 6, :y 9.945} {:x 7, :y 10.056})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; Clearly scientist 1 does not know what he is doing (and 2 and 3 are probably a little suspect too)!
;;; 
;;; To model this situation, we place simple priors on the mean @@\mu@@ of the measurements, and the error standard deviation @@\sigma\_i@@ for each of the @@i@@ scientists.
;;; 
;;; As a starting point, consider placing uninformative priors on these parameters; a suggestion is
;;; $$\begin{align}
;;; \mu &\sim \mathrm{Normal}(0, 50) \\\\
;;; \sigma\_i &\sim \mathrm{Uniform}(0, 25)
;;; \end{align}$$
;;; 
;;; The uniform distribution over real numbers on the open interval @@(a, b)@@ can be constructed in Anglican with `(uniform-continuous a b)`.
;;; 
;;; We can ask two questions, here:
;;; 
;;; * Given these measurements, what is the posterior distribution of @@x@@?
;;; * What distribution over noise level @@\sigma_i@@ do we infer for each of these scientists' estimates?
;;; 
;;; We write the model in Anglican such that it has two `predict` statements, one for the value `:x` and one for the vector of seven noise levels `:noise`.
;; **

;; @@
(defquery scientists [measurements]
  (let [mu (sample (normal 0 50))
        noise-levels (repeatedly
                       (count measurements)
                       #(sample (uniform-continuous 0 25)))]
    (doall (map (fn [measurement noise-level]
                  (observe (normal mu noise-level) measurement))
         measurements noise-levels))
    (predict :x mu)
    (predict :noise noise-levels)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gaussian-estimation/scientists</span>","value":"#'gaussian-estimation/scientists"}
;; <=

;; **
;;; Given the measurements, we sample from the conditional distribution, and plot a histogram of the results.
;; **

;; @@
(def scientist-posterior ((conditional scientists :pgibbs :number-of-particles 1000) measurements))

(def scientist-samples (repeatedly 50000 #(sample scientist-posterior)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gaussian-estimation/scientist-samples</span>","value":"#'gaussian-estimation/scientist-samples"}
;; <=

;; @@
(println "Expected value of measured quantity:" (stat/mean (map :x scientist-samples)))

(plot/histogram (map :x scientist-samples)
                :normalize :probability
                :bins 20)
;; @@
;; ->
;;; Expected value of measured quantity: 8.907864837111525
;;; 
;; <-
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"32c16dab-be1f-472d-8100-5c837b6135f4","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"32c16dab-be1f-472d-8100-5c837b6135f4","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"32c16dab-be1f-472d-8100-5c837b6135f4"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"32c16dab-be1f-472d-8100-5c837b6135f4","values":[{"x":-11.399452500059837,"y":0},{"x":-9.653959617883036,"y":2.0E-5},{"x":-7.908466735706234,"y":2.0E-5},{"x":-6.162973853529433,"y":2.4E-4},{"x":-4.4174809713526315,"y":2.2E-4},{"x":-2.6719880891758296,"y":4.4E-4},{"x":-0.9264952069990278,"y":0.00148},{"x":0.8189976751777741,"y":0.00344},{"x":2.564490557354576,"y":0.0077},{"x":4.309983439531377,"y":0.01504},{"x":6.055476321708179,"y":0.04518},{"x":7.80096920388498,"y":0.1037},{"x":9.546462086061782,"y":0.43946},{"x":11.291954968238583,"y":0.3605},{"x":13.037447850415385,"y":0.01408},{"x":14.782940732592186,"y":0.0053},{"x":16.52843361476899,"y":0.0021},{"x":18.27392649694579,"y":8.6E-4},{"x":20.019419379122592,"y":1.8E-4},{"x":21.764912261299393,"y":0.0},{"x":23.510405143476195,"y":2.0E-5},{"x":25.255898025652996,"y":2.0E-5},{"x":27.001390907829798,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"32c16dab-be1f-472d-8100-5c837b6135f4\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"32c16dab-be1f-472d-8100-5c837b6135f4\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"32c16dab-be1f-472d-8100-5c837b6135f4\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"32c16dab-be1f-472d-8100-5c837b6135f4\", :values ({:x -11.399452500059837, :y 0} {:x -9.653959617883036, :y 2.0E-5} {:x -7.908466735706234, :y 2.0E-5} {:x -6.162973853529433, :y 2.4E-4} {:x -4.4174809713526315, :y 2.2E-4} {:x -2.6719880891758296, :y 4.4E-4} {:x -0.9264952069990278, :y 0.00148} {:x 0.8189976751777741, :y 0.00344} {:x 2.564490557354576, :y 0.0077} {:x 4.309983439531377, :y 0.01504} {:x 6.055476321708179, :y 0.04518} {:x 7.80096920388498, :y 0.1037} {:x 9.546462086061782, :y 0.43946} {:x 11.291954968238583, :y 0.3605} {:x 13.037447850415385, :y 0.01408} {:x 14.782940732592186, :y 0.0053} {:x 16.52843361476899, :y 0.0021} {:x 18.27392649694579, :y 8.6E-4} {:x 20.019419379122592, :y 1.8E-4} {:x 21.764912261299393, :y 0.0} {:x 23.510405143476195, :y 2.0E-5} {:x 25.255898025652996, :y 2.0E-5} {:x 27.001390907829798, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; @@
(def noise-estimate (stat/mean (map :noise scientist-samples)))

(plot/bar-chart (range 1 8) noise-estimate)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"de5683fb-6db0-496f-8503-46dbe9acccc0","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"de5683fb-6db0-496f-8503-46dbe9acccc0","field":"data.y"}}],"marks":[{"type":"rect","from":{"data":"de5683fb-6db0-496f-8503-46dbe9acccc0"},"properties":{"enter":{"y":{"scale":"y","field":"data.y"},"width":{"offset":-1,"scale":"x","band":true},"x":{"scale":"x","field":"data.x"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"data":[{"name":"de5683fb-6db0-496f-8503-46dbe9acccc0","values":[{"x":1,"y":20.395273154116353},{"x":2,"y":14.004508467438283},{"x":3,"y":9.496267332464807},{"x":4,"y":11.666271040967201},{"x":5,"y":6.491212403989281},{"x":6,"y":4.766566878252081},{"x":7,"y":8.930511086810847}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"de5683fb-6db0-496f-8503-46dbe9acccc0\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"de5683fb-6db0-496f-8503-46dbe9acccc0\", :field \"data.y\"}}], :marks [{:type \"rect\", :from {:data \"de5683fb-6db0-496f-8503-46dbe9acccc0\"}, :properties {:enter {:y {:scale \"y\", :field \"data.y\"}, :width {:offset -1, :scale \"x\", :band true}, :x {:scale \"x\", :field \"data.x\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :data [{:name \"de5683fb-6db0-496f-8503-46dbe9acccc0\", :values ({:x 1, :y 20.395273154116353} {:x 2, :y 14.004508467438283} {:x 3, :y 9.496267332464807} {:x 4, :y 11.666271040967201} {:x 5, :y 6.491212403989281} {:x 6, :y 4.766566878252081} {:x 7, :y 8.930511086810847})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; * Are these noise levels what you would expect?
;;; * How sensitive is this to the prior on @@\mu@@ and @@\sigma\_i@@?
;; **

;; **
;;; 
;; **
