;; gorilla-repl.fileformat = 1

;; **
;;; # The Indian GPA Problem
;;; 
;;; This example was inspired by [Stuart Russell](https://www.cs.berkeley.edu/~russell/) who pointed out that most probabilistic programming systems, _Anglican currently included_, produce the "wrong" answer to this problem.  In short, the problem is: if you observe that a student GPA is exactly @@4.0@@ in a mix of transcripts of students from the USA (GPA's from @@0.0@@ to @@4.0@@ and India (GPA's from @@0.0@@ to @@10.0@@) what is the probability that the student is from India?  This problem gets at the heart of measure theoretic problems arising from combining distribution and density, problems not easy to automatically avoid in a probabilistic programming system.  As we know from statistics, given the mixture distribution and given the fact that his/her GPA is _exactly_ @@4.0@@,
;;; the probability that the student is American must be @@1.0@@ (i.e. zero probability that the student is from India).  What this really highlights is the definition of _is_.
;;; Does observing a GPA of @@4.0@@ mean exactly and only @@4.0@@?
;; **

;; @@
(ns indian-gpa
  (:require [gorilla-plot.core :as plot])
  (:use clojure.repl
        [mrepl core]
        [embang runtime emit]
        [anglib crp]
        [clojure.string :only (join split blank?)]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## Partially discrete, partially continuous distributions
;;; 
;;; Let's model this anyway.  Anglican allows complex compositions of distributions conveniently.
;;; 
;;; For example, you can easily represent a partially discrete, partially continuous distribution.  Such a distribution could be used to represent the distribution over GPAs of American students, where their GPAs from the interval @@\left(0.0, 4.0\right)@@ are distributed via Beta distribution (parameters @@\alpha@@ and @@\beta@@),
;;; while some small fraction of students have _exactly_ 0.0 and 4.0 as their GPAs.
;;; These latter two points should have separate probability masses @@p_1@@ and @@p_2@@.
;;; 
;;; The density function of this distribution can be written as
;;; 
;;; $$ f\left(x\right) = p_1 \delta x + p_2 \delta \left( x - 4.0 \right) + \left( 1 - p_1 - p_2 \right) \frac{(^x/_4)^{\alpha-1}(1- (^x/_4) )^{\beta-1}} {Beta(\alpha,\beta)}\! $$
;;; 
;;; This distribution could be represented as the following simple probabilistic program (e.g. let @@p_1 = 0.02, p_2 = 0.08, \alpha = 7.0, \beta = 3.0@@ (to account for grade inflation).  We can also see what this looks like 
;; **

;; @@
(def american-gpa (fn [] (if (sample (flip 0.9)) 
                    (* 4 (sample (beta 7 3))) 
                    (* 4 (if (flip 0.8) 1.0 0.0)))))
(plot/histogram (repeatedly 10000 american-gpa) :bins 50)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"db6b4825-cfa5-43e8-a2fb-f2b518fa14f9","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"db6b4825-cfa5-43e8-a2fb-f2b518fa14f9","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"db6b4825-cfa5-43e8-a2fb-f2b518fa14f9"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"db6b4825-cfa5-43e8-a2fb-f2b518fa14f9","values":[{"x":0.6572976483128476,"y":0},{"x":0.7241516953465907,"y":2.0},{"x":0.7910057423803338,"y":1.0},{"x":0.8578597894140769,"y":0.0},{"x":0.92471383644782,"y":3.0},{"x":0.9915678834815631,"y":6.0},{"x":1.0584219305153062,"y":4.0},{"x":1.1252759775490493,"y":8.0},{"x":1.1921300245827924,"y":12.0},{"x":1.2589840716165355,"y":8.0},{"x":1.3258381186502786,"y":18.0},{"x":1.3926921656840217,"y":28.0},{"x":1.4595462127177647,"y":28.0},{"x":1.5264002597515078,"y":32.0},{"x":1.593254306785251,"y":60.0},{"x":1.660108353818994,"y":55.0},{"x":1.7269624008527371,"y":76.0},{"x":1.7938164478864802,"y":76.0},{"x":1.8606704949202233,"y":90.0},{"x":1.9275245419539664,"y":133.0},{"x":1.9943785889877095,"y":150.0},{"x":2.0612326360214523,"y":147.0},{"x":2.128086683055195,"y":181.0},{"x":2.194940730088938,"y":209.0},{"x":2.261794777122681,"y":227.0},{"x":2.328648824156424,"y":261.0},{"x":2.3955028711901667,"y":280.0},{"x":2.4623569182239096,"y":316.0},{"x":2.5292109652576524,"y":352.0},{"x":2.5960650122913953,"y":344.0},{"x":2.662919059325138,"y":366.0},{"x":2.729773106358881,"y":349.0},{"x":2.796627153392624,"y":362.0},{"x":2.863481200426367,"y":389.0},{"x":2.9303352474601096,"y":407.0},{"x":2.9971892944938525,"y":418.0},{"x":3.0640433415275954,"y":423.0},{"x":3.1308973885613383,"y":439.0},{"x":3.197751435595081,"y":402.0},{"x":3.264605482628824,"y":349.0},{"x":3.331459529662567,"y":346.0},{"x":3.3983135766963097,"y":327.0},{"x":3.4651676237300526,"y":342.0},{"x":3.5320216707637955,"y":254.0},{"x":3.5988757177975383,"y":228.0},{"x":3.665729764831281,"y":185.0},{"x":3.732583811865024,"y":151.0},{"x":3.799437858898767,"y":86.0},{"x":3.86629190593251,"y":42.0},{"x":3.9331459529662527,"y":22.0},{"x":3.9999999999999956,"y":4.0},{"x":4.066854047033739,"y":1002.0},{"x":4.133708094067482,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"db6b4825-cfa5-43e8-a2fb-f2b518fa14f9\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"db6b4825-cfa5-43e8-a2fb-f2b518fa14f9\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"db6b4825-cfa5-43e8-a2fb-f2b518fa14f9\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"db6b4825-cfa5-43e8-a2fb-f2b518fa14f9\", :values ({:x 0.6572976483128476, :y 0} {:x 0.7241516953465907, :y 2.0} {:x 0.7910057423803338, :y 1.0} {:x 0.8578597894140769, :y 0.0} {:x 0.92471383644782, :y 3.0} {:x 0.9915678834815631, :y 6.0} {:x 1.0584219305153062, :y 4.0} {:x 1.1252759775490493, :y 8.0} {:x 1.1921300245827924, :y 12.0} {:x 1.2589840716165355, :y 8.0} {:x 1.3258381186502786, :y 18.0} {:x 1.3926921656840217, :y 28.0} {:x 1.4595462127177647, :y 28.0} {:x 1.5264002597515078, :y 32.0} {:x 1.593254306785251, :y 60.0} {:x 1.660108353818994, :y 55.0} {:x 1.7269624008527371, :y 76.0} {:x 1.7938164478864802, :y 76.0} {:x 1.8606704949202233, :y 90.0} {:x 1.9275245419539664, :y 133.0} {:x 1.9943785889877095, :y 150.0} {:x 2.0612326360214523, :y 147.0} {:x 2.128086683055195, :y 181.0} {:x 2.194940730088938, :y 209.0} {:x 2.261794777122681, :y 227.0} {:x 2.328648824156424, :y 261.0} {:x 2.3955028711901667, :y 280.0} {:x 2.4623569182239096, :y 316.0} {:x 2.5292109652576524, :y 352.0} {:x 2.5960650122913953, :y 344.0} {:x 2.662919059325138, :y 366.0} {:x 2.729773106358881, :y 349.0} {:x 2.796627153392624, :y 362.0} {:x 2.863481200426367, :y 389.0} {:x 2.9303352474601096, :y 407.0} {:x 2.9971892944938525, :y 418.0} {:x 3.0640433415275954, :y 423.0} {:x 3.1308973885613383, :y 439.0} {:x 3.197751435595081, :y 402.0} {:x 3.264605482628824, :y 349.0} {:x 3.331459529662567, :y 346.0} {:x 3.3983135766963097, :y 327.0} {:x 3.4651676237300526, :y 342.0} {:x 3.5320216707637955, :y 254.0} {:x 3.5988757177975383, :y 228.0} {:x 3.665729764831281, :y 185.0} {:x 3.732583811865024, :y 151.0} {:x 3.799437858898767, :y 86.0} {:x 3.86629190593251, :y 42.0} {:x 3.9331459529662527, :y 22.0} {:x 3.9999999999999956, :y 4.0} {:x 4.066854047033739, :y 1002.0} {:x 4.133708094067482, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; In India, however, in most GPAs lie in the range @@[0.0, 10.0]@@, and for instance could be represented as follows:
;; **

;; @@
(def indian-gpa (fn [] (if (sample (flip 0.99)) 
                    (* 10 (sample (beta 5 5))) 
                    (* 10 (if (flip 0.5) 1.0 0.0)))))
(plot/histogram (repeatedly 10000 indian-gpa) :bins 50)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"d359d6bf-7963-4602-beb3-0913c9d29ec8","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"d359d6bf-7963-4602-beb3-0913c9d29ec8","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"d359d6bf-7963-4602-beb3-0913c9d29ec8"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"d359d6bf-7963-4602-beb3-0913c9d29ec8","values":[{"x":0.45286707377131674,"y":0},{"x":0.6438097322958904,"y":2.0},{"x":0.8347523908204642,"y":3.0},{"x":1.0256950493450379,"y":4.0},{"x":1.2166377078696116,"y":11.0},{"x":1.4075803663941853,"y":20.0},{"x":1.598523024918759,"y":29.0},{"x":1.7894656834433327,"y":46.0},{"x":1.9804083419679064,"y":75.0},{"x":2.17135100049248,"y":83.0},{"x":2.3622936590170536,"y":110.0},{"x":2.5532363175416273,"y":138.0},{"x":2.744178976066201,"y":173.0},{"x":2.9351216345907747,"y":172.0},{"x":3.1260642931153484,"y":226.0},{"x":3.317006951639922,"y":266.0},{"x":3.507949610164496,"y":299.0},{"x":3.6988922686890695,"y":345.0},{"x":3.889834927213643,"y":343.0},{"x":4.0807775857382165,"y":373.0},{"x":4.27172024426279,"y":426.0},{"x":4.462662902787363,"y":464.0},{"x":4.653605561311936,"y":444.0},{"x":4.8445482198365095,"y":468.0},{"x":5.035490878361083,"y":499.0},{"x":5.226433536885656,"y":474.0},{"x":5.417376195410229,"y":450.0},{"x":5.608318853934803,"y":424.0},{"x":5.799261512459376,"y":463.0},{"x":5.990204170983949,"y":397.0},{"x":6.181146829508522,"y":383.0},{"x":6.372089488033096,"y":340.0},{"x":6.563032146557669,"y":337.0},{"x":6.753974805082242,"y":297.0},{"x":6.944917463606815,"y":254.0},{"x":7.135860122131389,"y":216.0},{"x":7.326802780655962,"y":203.0},{"x":7.517745439180535,"y":165.0},{"x":7.708688097705108,"y":142.0},{"x":7.899630756229682,"y":95.0},{"x":8.090573414754255,"y":67.0},{"x":8.281516073278828,"y":65.0},{"x":8.472458731803401,"y":42.0},{"x":8.663401390327975,"y":27.0},{"x":8.854344048852548,"y":20.0},{"x":9.045286707377121,"y":11.0},{"x":9.236229365901695,"y":1.0},{"x":9.427172024426268,"y":3.0},{"x":9.618114682950841,"y":1.0},{"x":9.809057341475414,"y":0.0},{"x":9.999999999999988,"y":0.0},{"x":10.19094265852456,"y":104.0},{"x":10.381885317049134,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"d359d6bf-7963-4602-beb3-0913c9d29ec8\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"d359d6bf-7963-4602-beb3-0913c9d29ec8\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"d359d6bf-7963-4602-beb3-0913c9d29ec8\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"d359d6bf-7963-4602-beb3-0913c9d29ec8\", :values ({:x 0.45286707377131674, :y 0} {:x 0.6438097322958904, :y 2.0} {:x 0.8347523908204642, :y 3.0} {:x 1.0256950493450379, :y 4.0} {:x 1.2166377078696116, :y 11.0} {:x 1.4075803663941853, :y 20.0} {:x 1.598523024918759, :y 29.0} {:x 1.7894656834433327, :y 46.0} {:x 1.9804083419679064, :y 75.0} {:x 2.17135100049248, :y 83.0} {:x 2.3622936590170536, :y 110.0} {:x 2.5532363175416273, :y 138.0} {:x 2.744178976066201, :y 173.0} {:x 2.9351216345907747, :y 172.0} {:x 3.1260642931153484, :y 226.0} {:x 3.317006951639922, :y 266.0} {:x 3.507949610164496, :y 299.0} {:x 3.6988922686890695, :y 345.0} {:x 3.889834927213643, :y 343.0} {:x 4.0807775857382165, :y 373.0} {:x 4.27172024426279, :y 426.0} {:x 4.462662902787363, :y 464.0} {:x 4.653605561311936, :y 444.0} {:x 4.8445482198365095, :y 468.0} {:x 5.035490878361083, :y 499.0} {:x 5.226433536885656, :y 474.0} {:x 5.417376195410229, :y 450.0} {:x 5.608318853934803, :y 424.0} {:x 5.799261512459376, :y 463.0} {:x 5.990204170983949, :y 397.0} {:x 6.181146829508522, :y 383.0} {:x 6.372089488033096, :y 340.0} {:x 6.563032146557669, :y 337.0} {:x 6.753974805082242, :y 297.0} {:x 6.944917463606815, :y 254.0} {:x 7.135860122131389, :y 216.0} {:x 7.326802780655962, :y 203.0} {:x 7.517745439180535, :y 165.0} {:x 7.708688097705108, :y 142.0} {:x 7.899630756229682, :y 95.0} {:x 8.090573414754255, :y 67.0} {:x 8.281516073278828, :y 65.0} {:x 8.472458731803401, :y 42.0} {:x 8.663401390327975, :y 27.0} {:x 8.854344048852548, :y 20.0} {:x 9.045286707377121, :y 11.0} {:x 9.236229365901695, :y 1.0} {:x 9.427172024426268, :y 3.0} {:x 9.618114682950841, :y 1.0} {:x 9.809057341475414, :y 0.0} {:x 9.999999999999988, :y 0.0} {:x 10.19094265852456, :y 104.0} {:x 10.381885317049134, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; # Mixture of complex distributions
;;; 
;;; We can easily create the mixture of these two distributions (e.g., the student with the same probability @@0.5@@ either is from some US or Indian university):
;; **

;; @@
(defn student-gpa [] (if (sample (flip 0.5))
                             (american-gpa)
                             (indian-gpa)))
(plot/histogram (repeatedly 10000 student-gpa) :bins 50)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"30e21a2f-767b-4b4d-a6dc-f30ab0402649","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"30e21a2f-767b-4b4d-a6dc-f30ab0402649","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"30e21a2f-767b-4b4d-a6dc-f30ab0402649"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"30e21a2f-767b-4b4d-a6dc-f30ab0402649","values":[{"x":0.5917790860086096,"y":0},{"x":0.7799435042884375,"y":5.0},{"x":0.9681079225682654,"y":4.0},{"x":1.1562723408480933,"y":20.0},{"x":1.3444367591279212,"y":28.0},{"x":1.532601177407749,"y":64.0},{"x":1.720765595687577,"y":93.0},{"x":1.9089300139674048,"y":173.0},{"x":2.0970944322472325,"y":274.0},{"x":2.28525885052706,"y":350.0},{"x":2.473423268806888,"y":419.0},{"x":2.6615876870867154,"y":558.0},{"x":2.849752105366543,"y":674.0},{"x":3.0379165236463708,"y":658.0},{"x":3.2260809419261984,"y":706.0},{"x":3.414245360206026,"y":667.0},{"x":3.6024097784858538,"y":528.0},{"x":3.7905741967656814,"y":339.0},{"x":3.978738615045509,"y":224.0},{"x":4.166903033325337,"y":711.0},{"x":4.355067451605165,"y":191.0},{"x":4.543231869884993,"y":198.0},{"x":4.731396288164821,"y":238.0},{"x":4.919560706444649,"y":228.0},{"x":5.107725124724477,"y":236.0},{"x":5.295889543004305,"y":239.0},{"x":5.4840539612841335,"y":241.0},{"x":5.672218379563962,"y":241.0},{"x":5.86038279784379,"y":190.0},{"x":6.048547216123618,"y":176.0},{"x":6.236711634403446,"y":180.0},{"x":6.424876052683274,"y":184.0},{"x":6.613040470963102,"y":142.0},{"x":6.80120488924293,"y":150.0},{"x":6.989369307522758,"y":135.0},{"x":7.177533725802586,"y":112.0},{"x":7.3656981440824145,"y":82.0},{"x":7.553862562362243,"y":75.0},{"x":7.742026980642071,"y":72.0},{"x":7.930191398921899,"y":48.0},{"x":8.118355817201726,"y":28.0},{"x":8.306520235481553,"y":28.0},{"x":8.49468465376138,"y":17.0},{"x":8.682849072041208,"y":16.0},{"x":8.871013490321035,"y":11.0},{"x":9.059177908600862,"y":2.0},{"x":9.24734232688069,"y":2.0},{"x":9.435506745160517,"y":0.0},{"x":9.623671163440344,"y":0.0},{"x":9.811835581720171,"y":0.0},{"x":9.999999999999998,"y":0.0},{"x":10.188164418279825,"y":43.0},{"x":10.376328836559653,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"30e21a2f-767b-4b4d-a6dc-f30ab0402649\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"30e21a2f-767b-4b4d-a6dc-f30ab0402649\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"30e21a2f-767b-4b4d-a6dc-f30ab0402649\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"30e21a2f-767b-4b4d-a6dc-f30ab0402649\", :values ({:x 0.5917790860086096, :y 0} {:x 0.7799435042884375, :y 5.0} {:x 0.9681079225682654, :y 4.0} {:x 1.1562723408480933, :y 20.0} {:x 1.3444367591279212, :y 28.0} {:x 1.532601177407749, :y 64.0} {:x 1.720765595687577, :y 93.0} {:x 1.9089300139674048, :y 173.0} {:x 2.0970944322472325, :y 274.0} {:x 2.28525885052706, :y 350.0} {:x 2.473423268806888, :y 419.0} {:x 2.6615876870867154, :y 558.0} {:x 2.849752105366543, :y 674.0} {:x 3.0379165236463708, :y 658.0} {:x 3.2260809419261984, :y 706.0} {:x 3.414245360206026, :y 667.0} {:x 3.6024097784858538, :y 528.0} {:x 3.7905741967656814, :y 339.0} {:x 3.978738615045509, :y 224.0} {:x 4.166903033325337, :y 711.0} {:x 4.355067451605165, :y 191.0} {:x 4.543231869884993, :y 198.0} {:x 4.731396288164821, :y 238.0} {:x 4.919560706444649, :y 228.0} {:x 5.107725124724477, :y 236.0} {:x 5.295889543004305, :y 239.0} {:x 5.4840539612841335, :y 241.0} {:x 5.672218379563962, :y 241.0} {:x 5.86038279784379, :y 190.0} {:x 6.048547216123618, :y 176.0} {:x 6.236711634403446, :y 180.0} {:x 6.424876052683274, :y 184.0} {:x 6.613040470963102, :y 142.0} {:x 6.80120488924293, :y 150.0} {:x 6.989369307522758, :y 135.0} {:x 7.177533725802586, :y 112.0} {:x 7.3656981440824145, :y 82.0} {:x 7.553862562362243, :y 75.0} {:x 7.742026980642071, :y 72.0} {:x 7.930191398921899, :y 48.0} {:x 8.118355817201726, :y 28.0} {:x 8.306520235481553, :y 28.0} {:x 8.49468465376138, :y 17.0} {:x 8.682849072041208, :y 16.0} {:x 8.871013490321035, :y 11.0} {:x 9.059177908600862, :y 2.0} {:x 9.24734232688069, :y 2.0} {:x 9.435506745160517, :y 0.0} {:x 9.623671163440344, :y 0.0} {:x 9.811835581720171, :y 0.0} {:x 9.999999999999998, :y 0.0} {:x 10.188164418279825, :y 43.0} {:x 10.376328836559653, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; 
;;; # Inferring nationality from GPA example
;;; 
;;; We might like to condition the probabilistic program, inferring the probability of a student being American if his or her GPA is exactly @@4.0@@ but this is syntactically not allowed in Anglican natively (on purpose).
;;; 
;;; However, if we condition on the fact that the student's GPA is appoximately equal to @@4.0@@, so we have some uncertaintly about the full precision (which could be represented via Gaussian noise), the posterior will be different and now there will be some positive probability that student's university is located in India as well.
;; **

;; @@
  (with-primitive-procedures [student-gpa american-gpa indian-gpa] 
    (defquery which-nationality [observed-gpa tolerance]
    (let [nationality (sample (categorical [["USA" 0.6] ["India" 0.4] ]))  
          student_gpa (if (= nationality "USA") 
                          (american-gpa)
                          (indian-gpa))]
          (observe (normal student_gpa tolerance) observed-gpa)
          (predict :nationality nationality))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;indian-gpa/which-nationality</span>","value":"#'indian-gpa/which-nationality"}
;; <=

;; @@
(def N 1000)
(def tolerance 0.01)
(def sampler (doquery :pcascade which-nationality [4.0 tolerance]))
(def samples (drop 100 (take (+ N 100) sampler)))
(def num-usa (count (filter #(= % "USA") (map :nationality (map get-predicts samples)))))
["USA" (/ num-usa N) "India" (/ (- N num-usa) N)]
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;USA&quot;</span>","value":"\"USA\""},{"type":"html","content":"<span class='clj-ratio'>493/500</span>","value":"493/500"},{"type":"html","content":"<span class='clj-string'>&quot;India&quot;</span>","value":"\"India\""},{"type":"html","content":"<span class='clj-ratio'>7/500</span>","value":"7/500"}],"value":"[\"USA\" 493/500 \"India\" 7/500]"}
;; <=

;; **
;;; By playing with the tolerance we can get different behavior.  Try, for instance, ```(def tolerance 0.0001)```
;;; 
;;; We can add a Dirac distribution to the language easily enough, but in so doing we must be careful, particularly we must realize that inference will effectively default to rejection sampling in most cases, and rejection sampling can be exponentially slow.  In this case we should be OK.
;; **

;; @@
(defn dirac
  "Dirac distribution"
  [x]
    (reify
      distribution
      (sample [this] x)
      (observe [this value] (if (= x value) 0.0 (- (/ 1.0 0.0))))))
(with-primitive-procedures [student-gpa american-gpa indian-gpa dirac] 
    (defquery which-nationality-with-dirac [observed-gpa]
    (let [nationality (sample (categorical [["USA" 0.6] ["India" 0.4] ]))  
          student_gpa (if (= nationality "USA") 
                          (american-gpa)
                          (indian-gpa))]
          (observe (dirac student_gpa) observed-gpa)
          (predict :nationality nationality))))
(def N 1000)
(def sampler (doquery :pcascade which-nationality-with-dirac [4.0]))
(def samples (drop 100 (take (+ N 100) sampler)))
(def num-usa (count (filter #(= % "USA") (map :nationality (map get-predicts samples)))))
["USA" (/ num-usa N) "India" (/ (- N num-usa) N) "N" N]
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-string'>&quot;USA&quot;</span>","value":"\"USA\""},{"type":"html","content":"<span class='clj-long'>1</span>","value":"1"},{"type":"html","content":"<span class='clj-string'>&quot;India&quot;</span>","value":"\"India\""},{"type":"html","content":"<span class='clj-long'>0</span>","value":"0"},{"type":"html","content":"<span class='clj-string'>&quot;N&quot;</span>","value":"\"N\""},{"type":"html","content":"<span class='clj-long'>1000</span>","value":"1000"}],"value":"[\"USA\" 1 \"India\" 0 \"N\" 1000]"}
;; <=

;; @@

;; @@
