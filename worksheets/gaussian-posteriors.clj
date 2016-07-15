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
  (:require [gorilla-plot.core :as plot]
            [clojure.core.matrix :as m]
            [anglican.stat :as s])
  (:use clojure.repl
        [anglican 
          core runtime emit 
          [state :only [get-predicts get-log-weight set-log-weight]]
          [inference :only [collect-by equalize]]]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; 
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
    mu))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gaussian-estimation/gaussian-model</span>","value":"#'gaussian-estimation/gaussian-model"}
;; <=

;; **
;;; The following code creates a distribution object which represents the posterior distribution, conditioned on the supplied `dataset`, using the sequential Monte Carlo inference algorithm.
;; **

;; @@
(def posterior 
  ((conditional gaussian-model :smc :number-of-particles 10) dataset))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gaussian-estimation/posterior</span>","value":"#'gaussian-estimation/posterior"}
;; <=

;; **
;;; The following line now draws 20,000 samples from the Clojure posterior distribution object.
;; **

;; @@
(def posterior-samples (repeatedly 20000 #(sample* posterior)))
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
(def prior-samples (repeatedly 20000 #(sample* (normal 1 (sqrt 5)))))


(println "Prior on mu (blue) and posterior (green)")
(plot/compose
 (plot/histogram prior-samples
                 :normalize :probability-density :bins 40
                 :plot-range [[-10 10] [0 0.8]])
 (plot/histogram posterior-samples
                 :normalize :probability-density :bins 40
                 :color :green))
;; @@
;; ->
;;; Prior on mu (blue) and posterior (green)
;;; 
;; <-
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-10,10]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,0.8]}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"a052f399-6e33-4617-afd4-79cf505e98e8","values":[{"x":-10.0,"y":0},{"x":-9.5,"y":0.0},{"x":-9.0,"y":0.0},{"x":-8.5,"y":0.0},{"x":-8.0,"y":1.0E-4},{"x":-7.5,"y":0.0},{"x":-7.0,"y":1.0E-4},{"x":-6.5,"y":4.0E-4},{"x":-6.0,"y":8.0E-4},{"x":-5.5,"y":0.0023},{"x":-5.0,"y":0.0038},{"x":-4.5,"y":0.0067},{"x":-4.0,"y":0.0113},{"x":-3.5,"y":0.0188},{"x":-3.0,"y":0.0299},{"x":-2.5,"y":0.0427},{"x":-2.0,"y":0.0615},{"x":-1.5,"y":0.0866},{"x":-1.0,"y":0.1116},{"x":-0.5,"y":0.1358},{"x":0.0,"y":0.1484},{"x":0.5,"y":0.1637},{"x":1.0,"y":0.176},{"x":1.5,"y":0.1741},{"x":2.0,"y":0.1655},{"x":2.5,"y":0.1501},{"x":3.0,"y":0.1316},{"x":3.5,"y":0.1044},{"x":4.0,"y":0.0869},{"x":4.5,"y":0.0609},{"x":5.0,"y":0.0471},{"x":5.5,"y":0.0344},{"x":6.0,"y":0.0202},{"x":6.5,"y":0.0115},{"x":7.0,"y":0.0045},{"x":7.5,"y":0.0042},{"x":8.0,"y":0.0022},{"x":8.5,"y":0.0011},{"x":9.0,"y":5.0E-4},{"x":9.5,"y":2.0E-4},{"x":10.0,"y":0.0},{"x":10.5,"y":1.0E-4},{"x":11.0,"y":0}]},{"name":"aebe5a0a-682d-499e-a48a-fe72526db289","values":[{"x":1.9270149503860623,"y":0},{"x":2.125389453738956,"y":2.520485201218307E-4},{"x":2.32376395709185,"y":0.0012602426006091535},{"x":2.5221384604447437,"y":0.003276630761583799},{"x":2.7205129637976375,"y":2.520485201218307E-4},{"x":2.9188874671505314,"y":2.520485201218307E-4},{"x":3.1172619705034252,"y":0.0},{"x":3.315636473856319,"y":0.0027725337213401376},{"x":3.514010977209213,"y":0.0012602426006091535},{"x":3.712385480562107,"y":0.012854474526213366},{"x":3.9107599839150007,"y":0.018147493448771812},{"x":4.1091344872678945,"y":0.026213046092670392},{"x":4.307508990620788,"y":0.0713297311944781},{"x":4.505883493973682,"y":0.06628876079204148},{"x":4.704257997326576,"y":0.08972927316337173},{"x":4.90263250067947,"y":0.1610590043578498},{"x":5.101007004032364,"y":0.19659784569502795},{"x":5.299381507385258,"y":0.19886628237612441},{"x":5.497756010738152,"y":0.25230056864195255},{"x":5.696130514091045,"y":0.2558292479236582},{"x":5.894505017443939,"y":0.354380219291294},{"x":6.092879520796833,"y":0.3753002464614059},{"x":6.291254024149727,"y":0.40932679667785304},{"x":6.489628527502621,"y":0.31027172826997357},{"x":6.688003030855515,"y":0.3400134536443496},{"x":6.886377534208409,"y":0.3347204347217912},{"x":7.0847520375613025,"y":0.47082663558757976},{"x":7.283126540914196,"y":0.2437309189578103},{"x":7.48150104426709,"y":0.10812881513226537},{"x":7.679875547619984,"y":0.20063062201697723},{"x":7.878250050972878,"y":0.19357326345356599},{"x":8.076624554325772,"y":0.03957161765912742},{"x":8.274999057678665,"y":0.09174566132434638},{"x":8.473373561031558,"y":0.07359816787557456},{"x":8.67174806438445,"y":0.02092002717011195},{"x":8.870122567737344,"y":0.031254016495107007},{"x":9.068497071090237,"y":0.0},{"x":9.26687157444313,"y":0.0},{"x":9.465246077796023,"y":0.0698174400737471},{"x":9.663620581148916,"y":0.0},{"x":9.861995084501809,"y":0.0},{"x":10.060369587854701,"y":0.014618814167066181},{"x":10.258744091207594,"y":0}]}],"marks":[{"type":"line","from":{"data":"a052f399-6e33-4617-afd4-79cf505e98e8"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"aebe5a0a-682d-499e-a48a-fe72526db289"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"green"},"fillOpacity":{"value":0.4},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-10 10]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 0.8]}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"a052f399-6e33-4617-afd4-79cf505e98e8\", :values ({:x -10.0, :y 0} {:x -9.5, :y 0.0} {:x -9.0, :y 0.0} {:x -8.5, :y 0.0} {:x -8.0, :y 1.0E-4} {:x -7.5, :y 0.0} {:x -7.0, :y 1.0E-4} {:x -6.5, :y 4.0E-4} {:x -6.0, :y 8.0E-4} {:x -5.5, :y 0.0023} {:x -5.0, :y 0.0038} {:x -4.5, :y 0.0067} {:x -4.0, :y 0.0113} {:x -3.5, :y 0.0188} {:x -3.0, :y 0.0299} {:x -2.5, :y 0.0427} {:x -2.0, :y 0.0615} {:x -1.5, :y 0.0866} {:x -1.0, :y 0.1116} {:x -0.5, :y 0.1358} {:x 0.0, :y 0.1484} {:x 0.5, :y 0.1637} {:x 1.0, :y 0.176} {:x 1.5, :y 0.1741} {:x 2.0, :y 0.1655} {:x 2.5, :y 0.1501} {:x 3.0, :y 0.1316} {:x 3.5, :y 0.1044} {:x 4.0, :y 0.0869} {:x 4.5, :y 0.0609} {:x 5.0, :y 0.0471} {:x 5.5, :y 0.0344} {:x 6.0, :y 0.0202} {:x 6.5, :y 0.0115} {:x 7.0, :y 0.0045} {:x 7.5, :y 0.0042} {:x 8.0, :y 0.0022} {:x 8.5, :y 0.0011} {:x 9.0, :y 5.0E-4} {:x 9.5, :y 2.0E-4} {:x 10.0, :y 0.0} {:x 10.5, :y 1.0E-4} {:x 11.0, :y 0})} {:name \"aebe5a0a-682d-499e-a48a-fe72526db289\", :values ({:x 1.9270149503860623, :y 0} {:x 2.125389453738956, :y 2.520485201218307E-4} {:x 2.32376395709185, :y 0.0012602426006091535} {:x 2.5221384604447437, :y 0.003276630761583799} {:x 2.7205129637976375, :y 2.520485201218307E-4} {:x 2.9188874671505314, :y 2.520485201218307E-4} {:x 3.1172619705034252, :y 0.0} {:x 3.315636473856319, :y 0.0027725337213401376} {:x 3.514010977209213, :y 0.0012602426006091535} {:x 3.712385480562107, :y 0.012854474526213366} {:x 3.9107599839150007, :y 0.018147493448771812} {:x 4.1091344872678945, :y 0.026213046092670392} {:x 4.307508990620788, :y 0.0713297311944781} {:x 4.505883493973682, :y 0.06628876079204148} {:x 4.704257997326576, :y 0.08972927316337173} {:x 4.90263250067947, :y 0.1610590043578498} {:x 5.101007004032364, :y 0.19659784569502795} {:x 5.299381507385258, :y 0.19886628237612441} {:x 5.497756010738152, :y 0.25230056864195255} {:x 5.696130514091045, :y 0.2558292479236582} {:x 5.894505017443939, :y 0.354380219291294} {:x 6.092879520796833, :y 0.3753002464614059} {:x 6.291254024149727, :y 0.40932679667785304} {:x 6.489628527502621, :y 0.31027172826997357} {:x 6.688003030855515, :y 0.3400134536443496} {:x 6.886377534208409, :y 0.3347204347217912} {:x 7.0847520375613025, :y 0.47082663558757976} {:x 7.283126540914196, :y 0.2437309189578103} {:x 7.48150104426709, :y 0.10812881513226537} {:x 7.679875547619984, :y 0.20063062201697723} {:x 7.878250050972878, :y 0.19357326345356599} {:x 8.076624554325772, :y 0.03957161765912742} {:x 8.274999057678665, :y 0.09174566132434638} {:x 8.473373561031558, :y 0.07359816787557456} {:x 8.67174806438445, :y 0.02092002717011195} {:x 8.870122567737344, :y 0.031254016495107007} {:x 9.068497071090237, :y 0.0} {:x 9.26687157444313, :y 0.0} {:x 9.465246077796023, :y 0.0698174400737471} {:x 9.663620581148916, :y 0.0} {:x 9.861995084501809, :y 0.0} {:x 10.060369587854701, :y 0.014618814167066181} {:x 10.258744091207594, :y 0})}), :marks ({:type \"line\", :from {:data \"a052f399-6e33-4617-afd4-79cf505e98e8\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"aebe5a0a-682d-499e-a48a-fe72526db289\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value :green}, :fillOpacity {:value 0.4}, :stroke {:value :green}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"084d40c8-44f8-4085-bc0f-da05b138882c","values":[{"x":1,"y":-27.02},{"x":2,"y":3.57},{"x":3,"y":8.191},{"x":4,"y":9.898},{"x":5,"y":9.603},{"x":6,"y":9.945},{"x":7,"y":10.056}]}],"marks":[{"type":"rect","from":{"data":"084d40c8-44f8-4085-bc0f-da05b138882c"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"084d40c8-44f8-4085-bc0f-da05b138882c","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"084d40c8-44f8-4085-bc0f-da05b138882c","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"084d40c8-44f8-4085-bc0f-da05b138882c\", :values ({:x 1, :y -27.02} {:x 2, :y 3.57} {:x 3, :y 8.191} {:x 4, :y 9.898} {:x 5, :y 9.603} {:x 6, :y 9.945} {:x 7, :y 10.056})}], :marks [{:type \"rect\", :from {:data \"084d40c8-44f8-4085-bc0f-da05b138882c\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"084d40c8-44f8-4085-bc0f-da05b138882c\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"084d40c8-44f8-4085-bc0f-da05b138882c\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
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
    {:mu mu
     :noise noise-levels}))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gaussian-estimation/scientists</span>","value":"#'gaussian-estimation/scientists"}
;; <=

;; **
;;; Given the measurements, we sample from the conditional distribution, and plot a histogram of the results.
;; **

;; @@
(def scientist-posterior ((conditional scientists :smc :number-of-particles 1000) measurements))

(def scientist-samples (repeatedly 50000 #(sample* scientist-posterior)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;gaussian-estimation/scientist-samples</span>","value":"#'gaussian-estimation/scientist-samples"}
;; <=

;; @@
(println "Expected value of measured quantity:" (s/mean (map :mu scientist-samples)))

(plot/histogram (map :mu scientist-samples)
                :normalize :probability
                :bins 20)
;; @@
;; ->
;;; Expected value of measured quantity: 8.526699470402207
;;; 
;; <-
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"c38a1d19-c75a-4bf0-80ed-936b9362a666","values":[{"x":-14.180478311407935,"y":0},{"x":-12.403846652505521,"y":4.0E-5},{"x":-10.627214993603108,"y":8.0E-5},{"x":-8.850583334700694,"y":8.0E-5},{"x":-7.07395167579828,"y":1.6E-4},{"x":-5.297320016895865,"y":4.4E-4},{"x":-3.5206883579934507,"y":6.2E-4},{"x":-1.7440566990910364,"y":0.00124},{"x":0.03257495981137781,"y":0.00486},{"x":1.809206618713792,"y":0.01004},{"x":3.5858382776162063,"y":0.04812},{"x":5.362469936518621,"y":0.05352},{"x":7.139101595421035,"y":0.12792},{"x":8.915733254323449,"y":0.19994},{"x":10.692364913225862,"y":0.41386},{"x":12.468996572128276,"y":0.116},{"x":14.24562823103069,"y":0.01516},{"x":16.022259889933103,"y":0.00642},{"x":17.798891548835517,"y":8.6E-4},{"x":19.57552320773793,"y":3.4E-4},{"x":21.352154866640344,"y":2.8E-4},{"x":23.128786525542758,"y":2.0E-5},{"x":24.90541818444517,"y":0}]}],"marks":[{"type":"line","from":{"data":"c38a1d19-c75a-4bf0-80ed-936b9362a666"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"c38a1d19-c75a-4bf0-80ed-936b9362a666","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"c38a1d19-c75a-4bf0-80ed-936b9362a666","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"c38a1d19-c75a-4bf0-80ed-936b9362a666\", :values ({:x -14.180478311407935, :y 0} {:x -12.403846652505521, :y 4.0E-5} {:x -10.627214993603108, :y 8.0E-5} {:x -8.850583334700694, :y 8.0E-5} {:x -7.07395167579828, :y 1.6E-4} {:x -5.297320016895865, :y 4.4E-4} {:x -3.5206883579934507, :y 6.2E-4} {:x -1.7440566990910364, :y 0.00124} {:x 0.03257495981137781, :y 0.00486} {:x 1.809206618713792, :y 0.01004} {:x 3.5858382776162063, :y 0.04812} {:x 5.362469936518621, :y 0.05352} {:x 7.139101595421035, :y 0.12792} {:x 8.915733254323449, :y 0.19994} {:x 10.692364913225862, :y 0.41386} {:x 12.468996572128276, :y 0.116} {:x 14.24562823103069, :y 0.01516} {:x 16.022259889933103, :y 0.00642} {:x 17.798891548835517, :y 8.6E-4} {:x 19.57552320773793, :y 3.4E-4} {:x 21.352154866640344, :y 2.8E-4} {:x 23.128786525542758, :y 2.0E-5} {:x 24.90541818444517, :y 0})}], :marks [{:type \"line\", :from {:data \"c38a1d19-c75a-4bf0-80ed-936b9362a666\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"c38a1d19-c75a-4bf0-80ed-936b9362a666\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"c38a1d19-c75a-4bf0-80ed-936b9362a666\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@
(def noise-estimate (s/mean (map :noise scientist-samples)))

(plot/bar-chart (range 1 8) noise-estimate)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"148d424e-94ee-4229-8e24-9055f7f9d897","values":[{"x":1,"y":20.66333776067405},{"x":2,"y":10.911647328076015},{"x":3,"y":9.963545794048871},{"x":4,"y":9.678368712693102},{"x":5,"y":9.889692351383127},{"x":6,"y":8.643118208548303},{"x":7,"y":9.724140666432035}]}],"marks":[{"type":"rect","from":{"data":"148d424e-94ee-4229-8e24-9055f7f9d897"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"148d424e-94ee-4229-8e24-9055f7f9d897","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"148d424e-94ee-4229-8e24-9055f7f9d897","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"148d424e-94ee-4229-8e24-9055f7f9d897\", :values ({:x 1, :y 20.66333776067405} {:x 2, :y 10.911647328076015} {:x 3, :y 9.963545794048871} {:x 4, :y 9.678368712693102} {:x 5, :y 9.889692351383127} {:x 6, :y 8.643118208548303} {:x 7, :y 9.724140666432035})}], :marks [{:type \"rect\", :from {:data \"148d424e-94ee-4229-8e24-9055f7f9d897\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"148d424e-94ee-4229-8e24-9055f7f9d897\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"148d424e-94ee-4229-8e24-9055f7f9d897\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; * Are these noise levels what you would expect?
;;; * How sensitive is this to the prior on @@\mu@@ and @@\sigma\_i@@?
;; **
