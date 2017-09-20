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
          core runtime emit smc
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":[-10,10]},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":[0,0.8]}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"13eba4e7-cdb1-4c01-a7b8-8f0f56524149","values":[{"x":-10.0,"y":0},{"x":-9.5,"y":0.0},{"x":-9.0,"y":0.0},{"x":-8.5,"y":0.0},{"x":-8.0,"y":0.0},{"x":-7.5,"y":0.0},{"x":-7.0,"y":1.0E-4},{"x":-6.5,"y":6.0E-4},{"x":-6.0,"y":9.0E-4},{"x":-5.5,"y":0.0017},{"x":-5.0,"y":0.0031},{"x":-4.5,"y":0.0063},{"x":-4.0,"y":0.0109},{"x":-3.5,"y":0.018},{"x":-3.0,"y":0.0294},{"x":-2.5,"y":0.0444},{"x":-2.0,"y":0.0587},{"x":-1.5,"y":0.085},{"x":-1.0,"y":0.1068},{"x":-0.5,"y":0.1262},{"x":0.0,"y":0.1487},{"x":0.5,"y":0.1708},{"x":1.0,"y":0.1832},{"x":1.5,"y":0.1754},{"x":2.0,"y":0.1678},{"x":2.5,"y":0.1526},{"x":3.0,"y":0.132},{"x":3.5,"y":0.1112},{"x":4.0,"y":0.0865},{"x":4.5,"y":0.0642},{"x":5.0,"y":0.0444},{"x":5.5,"y":0.0305},{"x":6.0,"y":0.0154},{"x":6.5,"y":0.0114},{"x":7.0,"y":0.0067},{"x":7.5,"y":0.0034},{"x":8.0,"y":0.0018},{"x":8.5,"y":0.0013},{"x":9.0,"y":3.0E-4},{"x":9.5,"y":2.0E-4},{"x":10.0,"y":1.0E-4},{"x":10.5,"y":0.0},{"x":11.0,"y":0}]},{"name":"bc97efbb-35ed-4784-a2bd-2b522ebe9921","values":[{"x":2.654109571493211,"y":0},{"x":2.8594516304340285,"y":4.869922923526423E-4},{"x":3.064793689374846,"y":0.0},{"x":3.2701357483156634,"y":2.4349614617632116E-4},{"x":3.475477807256481,"y":4.869922923526423E-4},{"x":3.6808198661972984,"y":0.00413943448499746},{"x":3.886161925138116,"y":0.01655773793998984},{"x":4.091503984078933,"y":0.05259516757408537},{"x":4.296846043019751,"y":0.0460207716273247},{"x":4.502188101960568,"y":0.10445984670964177},{"x":4.707530160901386,"y":0.10275537368640753},{"x":4.912872219842203,"y":0.13562735342021087},{"x":5.118214278783021,"y":0.19382293235635165},{"x":5.323556337723838,"y":0.30217871740481456},{"x":5.528898396664656,"y":0.29389984843481964},{"x":5.734240455605473,"y":0.2578624188007241},{"x":5.939582514546291,"y":0.45412031261883895},{"x":6.144924573487108,"y":0.2807510565412983},{"x":6.3502666324279256,"y":0.413212960061217},{"x":6.555608691368743,"y":0.3369986663080285},{"x":6.7609507503095605,"y":0.3438165584009655},{"x":6.966292809250378,"y":0.31362303627510163},{"x":7.1716348681911954,"y":0.24276565773779218},{"x":7.376976927132013,"y":0.2590798995316057},{"x":7.58231898607283,"y":0.31143157095951474},{"x":7.787661045013648,"y":0.11468668484904726},{"x":7.993003103954465,"y":0.09398951242405996},{"x":8.198345162895283,"y":0.05673460205908283},{"x":8.4036872218361,"y":0.021427660863516263},{"x":8.609029280776916,"y":0.01485326491675559},{"x":8.814371339717733,"y":0.02045367627881098},{"x":9.01971339865855,"y":0.013148791893521342},{"x":9.225055457599366,"y":0.05137768684320376},{"x":9.430397516540182,"y":0.009739845847052847},{"x":9.635739575480999,"y":0.0},{"x":9.841081634421815,"y":0.0},{"x":10.046423693362632,"y":0.0},{"x":10.251765752303449,"y":0.0},{"x":10.457107811244265,"y":0.0},{"x":10.662449870185082,"y":0.0},{"x":10.867791929125898,"y":0.0},{"x":11.073133988066715,"y":0.006574395946760671},{"x":11.278476047007532,"y":0}]}],"marks":[{"type":"line","from":{"data":"13eba4e7-cdb1-4c01-a7b8-8f0f56524149"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"bc97efbb-35ed-4784-a2bd-2b522ebe9921"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"green"},"fillOpacity":{"value":0.4},"stroke":{"value":"green"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain [-10 10]} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain [0 0.8]}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"13eba4e7-cdb1-4c01-a7b8-8f0f56524149\", :values ({:x -10.0, :y 0} {:x -9.5, :y 0.0} {:x -9.0, :y 0.0} {:x -8.5, :y 0.0} {:x -8.0, :y 0.0} {:x -7.5, :y 0.0} {:x -7.0, :y 1.0E-4} {:x -6.5, :y 6.0E-4} {:x -6.0, :y 9.0E-4} {:x -5.5, :y 0.0017} {:x -5.0, :y 0.0031} {:x -4.5, :y 0.0063} {:x -4.0, :y 0.0109} {:x -3.5, :y 0.018} {:x -3.0, :y 0.0294} {:x -2.5, :y 0.0444} {:x -2.0, :y 0.0587} {:x -1.5, :y 0.085} {:x -1.0, :y 0.1068} {:x -0.5, :y 0.1262} {:x 0.0, :y 0.1487} {:x 0.5, :y 0.1708} {:x 1.0, :y 0.1832} {:x 1.5, :y 0.1754} {:x 2.0, :y 0.1678} {:x 2.5, :y 0.1526} {:x 3.0, :y 0.132} {:x 3.5, :y 0.1112} {:x 4.0, :y 0.0865} {:x 4.5, :y 0.0642} {:x 5.0, :y 0.0444} {:x 5.5, :y 0.0305} {:x 6.0, :y 0.0154} {:x 6.5, :y 0.0114} {:x 7.0, :y 0.0067} {:x 7.5, :y 0.0034} {:x 8.0, :y 0.0018} {:x 8.5, :y 0.0013} {:x 9.0, :y 3.0E-4} {:x 9.5, :y 2.0E-4} {:x 10.0, :y 1.0E-4} {:x 10.5, :y 0.0} {:x 11.0, :y 0})} {:name \"bc97efbb-35ed-4784-a2bd-2b522ebe9921\", :values ({:x 2.654109571493211, :y 0} {:x 2.8594516304340285, :y 4.869922923526423E-4} {:x 3.064793689374846, :y 0.0} {:x 3.2701357483156634, :y 2.4349614617632116E-4} {:x 3.475477807256481, :y 4.869922923526423E-4} {:x 3.6808198661972984, :y 0.00413943448499746} {:x 3.886161925138116, :y 0.01655773793998984} {:x 4.091503984078933, :y 0.05259516757408537} {:x 4.296846043019751, :y 0.0460207716273247} {:x 4.502188101960568, :y 0.10445984670964177} {:x 4.707530160901386, :y 0.10275537368640753} {:x 4.912872219842203, :y 0.13562735342021087} {:x 5.118214278783021, :y 0.19382293235635165} {:x 5.323556337723838, :y 0.30217871740481456} {:x 5.528898396664656, :y 0.29389984843481964} {:x 5.734240455605473, :y 0.2578624188007241} {:x 5.939582514546291, :y 0.45412031261883895} {:x 6.144924573487108, :y 0.2807510565412983} {:x 6.3502666324279256, :y 0.413212960061217} {:x 6.555608691368743, :y 0.3369986663080285} {:x 6.7609507503095605, :y 0.3438165584009655} {:x 6.966292809250378, :y 0.31362303627510163} {:x 7.1716348681911954, :y 0.24276565773779218} {:x 7.376976927132013, :y 0.2590798995316057} {:x 7.58231898607283, :y 0.31143157095951474} {:x 7.787661045013648, :y 0.11468668484904726} {:x 7.993003103954465, :y 0.09398951242405996} {:x 8.198345162895283, :y 0.05673460205908283} {:x 8.4036872218361, :y 0.021427660863516263} {:x 8.609029280776916, :y 0.01485326491675559} {:x 8.814371339717733, :y 0.02045367627881098} {:x 9.01971339865855, :y 0.013148791893521342} {:x 9.225055457599366, :y 0.05137768684320376} {:x 9.430397516540182, :y 0.009739845847052847} {:x 9.635739575480999, :y 0.0} {:x 9.841081634421815, :y 0.0} {:x 10.046423693362632, :y 0.0} {:x 10.251765752303449, :y 0.0} {:x 10.457107811244265, :y 0.0} {:x 10.662449870185082, :y 0.0} {:x 10.867791929125898, :y 0.0} {:x 11.073133988066715, :y 0.006574395946760671} {:x 11.278476047007532, :y 0})}), :marks ({:type \"line\", :from {:data \"13eba4e7-cdb1-4c01-a7b8-8f0f56524149\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"bc97efbb-35ed-4784-a2bd-2b522ebe9921\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value :green}, :fillOpacity {:value 0.4}, :stroke {:value :green}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"0359fc07-af9b-46f1-bb28-ff65d997c6ff","values":[{"x":1,"y":-27.02},{"x":2,"y":3.57},{"x":3,"y":8.191},{"x":4,"y":9.898},{"x":5,"y":9.603},{"x":6,"y":9.945},{"x":7,"y":10.056}]}],"marks":[{"type":"rect","from":{"data":"0359fc07-af9b-46f1-bb28-ff65d997c6ff"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"0359fc07-af9b-46f1-bb28-ff65d997c6ff","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"0359fc07-af9b-46f1-bb28-ff65d997c6ff","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"0359fc07-af9b-46f1-bb28-ff65d997c6ff\", :values ({:x 1, :y -27.02} {:x 2, :y 3.57} {:x 3, :y 8.191} {:x 4, :y 9.898} {:x 5, :y 9.603} {:x 6, :y 9.945} {:x 7, :y 10.056})}], :marks [{:type \"rect\", :from {:data \"0359fc07-af9b-46f1-bb28-ff65d997c6ff\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"0359fc07-af9b-46f1-bb28-ff65d997c6ff\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"0359fc07-af9b-46f1-bb28-ff65d997c6ff\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
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
(println "Expected value of measured quantity:" (mean (map :mu scientist-samples)))

(plot/histogram (map :mu scientist-samples)
                :normalize :probability
                :bins 20)
;; @@
;; ->
;;; Expected value of measured quantity: 8.345073418571607
;;; 
;; <-
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"51f48e23-67de-4c37-9f55-eef286e85140","values":[{"x":-13.902133927579003,"y":0},{"x":-11.906743025115919,"y":4.0E-5},{"x":-9.911352122652834,"y":0.0},{"x":-7.915961220189749,"y":6.0E-5},{"x":-5.920570317726664,"y":2.6E-4},{"x":-3.925179415263579,"y":7.6E-4},{"x":-1.9297885128004941,"y":0.00216},{"x":0.0656023896625908,"y":0.00484},{"x":2.0609932921256755,"y":0.01362},{"x":4.056384194588761,"y":0.0482},{"x":6.051775097051846,"y":0.06954},{"x":8.047165999514931,"y":0.22496},{"x":10.042556901978015,"y":0.44194},{"x":12.0379478044411,"y":0.16038},{"x":14.033338706904184,"y":0.0257},{"x":16.028729609367268,"y":0.00558},{"x":18.024120511830354,"y":0.00138},{"x":20.01951141429344,"y":4.4E-4},{"x":22.014902316756526,"y":6.0E-5},{"x":24.010293219219612,"y":6.0E-5},{"x":26.005684121682698,"y":2.0E-5},{"x":28.001075024145784,"y":0}]}],"marks":[{"type":"line","from":{"data":"51f48e23-67de-4c37-9f55-eef286e85140"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"51f48e23-67de-4c37-9f55-eef286e85140","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"51f48e23-67de-4c37-9f55-eef286e85140","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"51f48e23-67de-4c37-9f55-eef286e85140\", :values ({:x -13.902133927579003, :y 0} {:x -11.906743025115919, :y 4.0E-5} {:x -9.911352122652834, :y 0.0} {:x -7.915961220189749, :y 6.0E-5} {:x -5.920570317726664, :y 2.6E-4} {:x -3.925179415263579, :y 7.6E-4} {:x -1.9297885128004941, :y 0.00216} {:x 0.0656023896625908, :y 0.00484} {:x 2.0609932921256755, :y 0.01362} {:x 4.056384194588761, :y 0.0482} {:x 6.051775097051846, :y 0.06954} {:x 8.047165999514931, :y 0.22496} {:x 10.042556901978015, :y 0.44194} {:x 12.0379478044411, :y 0.16038} {:x 14.033338706904184, :y 0.0257} {:x 16.028729609367268, :y 0.00558} {:x 18.024120511830354, :y 0.00138} {:x 20.01951141429344, :y 4.4E-4} {:x 22.014902316756526, :y 6.0E-5} {:x 24.010293219219612, :y 6.0E-5} {:x 26.005684121682698, :y 2.0E-5} {:x 28.001075024145784, :y 0})}], :marks [{:type \"line\", :from {:data \"51f48e23-67de-4c37-9f55-eef286e85140\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"51f48e23-67de-4c37-9f55-eef286e85140\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"51f48e23-67de-4c37-9f55-eef286e85140\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; @@
(def noise-estimate (mean (map :noise scientist-samples)))

(plot/bar-chart (range 1 8) noise-estimate)
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"8ac25d08-1fa0-477b-a85d-143961ed73aa","values":[{"x":1,"y":19.93578855738375},{"x":2,"y":11.139478246551846},{"x":3,"y":8.963933926071638},{"x":4,"y":9.766913396810523},{"x":5,"y":8.85042663163643},{"x":6,"y":10.010716870157818},{"x":7,"y":9.977039432793518}]}],"marks":[{"type":"rect","from":{"data":"8ac25d08-1fa0-477b-a85d-143961ed73aa"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"8ac25d08-1fa0-477b-a85d-143961ed73aa","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"8ac25d08-1fa0-477b-a85d-143961ed73aa","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"8ac25d08-1fa0-477b-a85d-143961ed73aa\", :values ({:x 1, :y 19.93578855738375} {:x 2, :y 11.139478246551846} {:x 3, :y 8.963933926071638} {:x 4, :y 9.766913396810523} {:x 5, :y 8.85042663163643} {:x 6, :y 10.010716870157818} {:x 7, :y 9.977039432793518})}], :marks [{:type \"rect\", :from {:data \"8ac25d08-1fa0-477b-a85d-143961ed73aa\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"8ac25d08-1fa0-477b-a85d-143961ed73aa\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"8ac25d08-1fa0-477b-a85d-143961ed73aa\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
;; <=

;; **
;;; * Are these noise levels what you would expect?
;;; * How sensitive is this to the prior on @@\mu@@ and @@\sigma\_i@@?
;; **
