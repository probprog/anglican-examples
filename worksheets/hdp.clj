;; gorilla-repl.fileformat = 1

;; **
;;; # Hierarchical Dirichlet Process
;; **

;; @@
(ns hdp
  (:require [gorilla-plot.core :as plot]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:use [anglican core emit runtime [state :only [get-predicts get-log-weight]]]
        [anglib crp]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; # Introduction
;;; 
;;; We implement a hierarchical Dirichlet process mixture model as described in [\[Teh et al., 2004\]](http://papers.nips.cc/paper/2698-sharing-clusters-among-related-groups-hierarchical-dirichlet-processes.pdf).
;;; 
;;; $$
;;; G\_0\ |\ \alpha\_0, H \sim \operatorname{DP}(\alpha\_0, H)
;;; $$
;;; 
;;; $$
;;; G\_j\ |\ G\_0, \left\\{\alpha\_j\right\\} \sim \operatorname{DP}(\alpha\_j, G_0)
;;; $$
;;; 
;;; See http://en.wikipedia.org/wiki/Hierarchical_Dirichlet_process for quick intro.
;;; 
;;; # Case
;;; 
;;; We use one of six World Bank economic indicators located at http://www.robots.ox.ac.uk/~fwood/anglican/teaching/mlss2014/py_mem/data.csv. Columns are described in http://www.robots.ox.ac.uk/~fwood/anglican/teaching/mlss2014/py_mem/data_description.txt. We consider countries to be divided into groups by continent (the last column in the CSV file).
;; **

;; **
;;; 
;; **

;; @@
(def unprocessed-data
  (with-open [in-file (io/reader "http://www.robots.ox.ac.uk/~fwood/anglican/teaching/mlss2014/py_mem/data.csv")]
    (doall
      (csv/read-csv in-file))))

(def processed-data
  (doall (map (fn [row] (map read-string row)) unprocessed-data)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;hdp/processed-data</span>","value":"#'hdp/processed-data"}
;; <=

;; **
;;; We want to use the economic indicator `GDP per capita, PPP (current international $)` (4th column in the CSV file, enumeration starts from 1),
;;; and investigate how similar are two countries on the same continent.
;;; 
;;; Our base distribution @@H@@ provides mean and standard deviation for each cluster. Based on input data, we decide to choose @@H@@ to be `(fn [] (list (sample (normal 50000.00 30000.0)) (* (sample (gamma 1 1)) 3000.0)))`.
;; **

;; @@
; hyperparameters
(def base-concentration 3.0)
(def intra-groups-concentration 3.0)

(defm create-sethuraman-stick-breaking-process
  [c]
  (let
    [
      ; sethuraman-stick-breaking-rule returns a procedure that picks
      ; a stick each time its called from the set of sticks lazily constructed
      ; via the closed-over one-parameter stick breaking rule
      sethuraman-stick-breaking-rule (mem (fn [k] (sample (beta 1 c))))

      ; sample-stick-index is a procedure that samples an index from
      ; a potentially infinite dimensional discrete distribution 
      ; lazily constructed by a stick breaking rule
      sample-stick-index (fn sample-stick-index [k] 
                           (if (sample (flip (sethuraman-stick-breaking-rule k))) 
                             k 
                             (sample-stick-index (+ k 1))))

      ; wrapping a stick breaking process to return it
      stick-breaking-process (fn [] (sample-stick-index 1))]
    stick-breaking-process))

(defquery hdp-example-1
  [input-data]
  (let
    [ 
      ; DPmem is a procedure that takes two arguments -- the concentration
      ; to a Dirichlet process and a base sampling procedure
      ; DPmem returns a procedure 
      DPmem (fn [concentration base]
              (let [get-value-from-cache-or-sample (mem (fn [args stick-index] 
                                                          (apply base args)))
                    get-stick-picking-procedure-from-cache (mem (fn [args] 
                                                                  (create-sethuraman-stick-breaking-process concentration)))]
                (fn [& varargs]
                  ; when the returned function is called, the first thing it does is get
                  ; the cached stick breaking procedure for the passed in arguments
                  ; and _calls_ it to get an index
                  (let [index ((get-stick-picking-procedure-from-cache varargs))]
                    ; if, for the given set of arguments and just sampled index
                    ; a return value has already been computed, get it from the cache
                    ; and return it, otherwise sample a new value
                    (get-value-from-cache-or-sample varargs index)))))

      H (fn [] (list (sample (normal 50000.00 30000.0)) (* (sample (gamma 1 1)) 3000.0)))

      ; define the base Dirichlet process
      base-dp (DPmem base-concentration H)

      ; a memoized procedure get-group-dp, depending on
      ; the group id (index j), returns the correspondent Dirichlet process G_j.
      ; We create all G_j-s with the same concentration parameter for simplicity
      get-group-dp (mem
                     (fn [group-id] (DPmem intra-groups-concentration base-dp)))

      ; a helper procedure sample-from-group-dp
      ; samples from the G_j by providing j as an argument
      sample-from-group-dp (fn [group-id]
                             ((get-group-dp group-id)))

      ; define the procedure country-parameters
      ; samples from the corresponding G_j by providing the continent name.
      ; This procedure is memoized so we can remember the sample for each country
      get-country-parameters (mem
                               (fn [country-id continent-id]
                                 (sample-from-group-dp continent-id)))]

    (map
      (fn [row]
        (let
          [
            country (nth row 0)
            indicator-value (nth row 3)
            continent (nth row 7)]
          (observe (apply normal (get-country-parameters country continent)) indicator-value)))
      input-data)

    ; predict how similar are Iceland and Germany
    (predict 'prediction
             (= (get-country-parameters "Iceland" "Europe")
                (get-country-parameters "Germany" "Europe")))))

(def samples
  (take 100
        (map get-predicts
             (doquery :pgibbs
                      hdp-example-1
                      [processed-data]
                      :number-of-particles 100))))

(def predictions
  (map
    (fn [sample] (if sample 1 0))
    (map (fn [sample] (get sample 'prediction)) samples)))

(plot/histogram predictions)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"a2da3c3b-348d-41e9-8d1a-530b777558a9","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"a2da3c3b-348d-41e9-8d1a-530b777558a9","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"a2da3c3b-348d-41e9-8d1a-530b777558a9"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"a2da3c3b-348d-41e9-8d1a-530b777558a9","values":[{"x":0.0,"y":0},{"x":0.12500000000000003,"y":57.0},{"x":0.25000000000000006,"y":0.0},{"x":0.3750000000000001,"y":0.0},{"x":0.5000000000000001,"y":0.0},{"x":0.6250000000000001,"y":0.0},{"x":0.7500000000000001,"y":0.0},{"x":0.8750000000000001,"y":0.0},{"x":1.0000000000000002,"y":43.0},{"x":1.1250000000000002,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"a2da3c3b-348d-41e9-8d1a-530b777558a9\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"a2da3c3b-348d-41e9-8d1a-530b777558a9\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"a2da3c3b-348d-41e9-8d1a-530b777558a9\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"a2da3c3b-348d-41e9-8d1a-530b777558a9\", :values ({:x 0.0, :y 0} {:x 0.12500000000000003, :y 57.0} {:x 0.25000000000000006, :y 0.0} {:x 0.3750000000000001, :y 0.0} {:x 0.5000000000000001, :y 0.0} {:x 0.6250000000000001, :y 0.0} {:x 0.7500000000000001, :y 0.0} {:x 0.8750000000000001, :y 0.0} {:x 1.0000000000000002, :y 43.0} {:x 1.1250000000000002, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;; Now let us sample and plot GDPs of two groups: `Europe` and `Africa`.
;; **

;; @@
(defquery hdp-example-2
  [input-data]
  (let
    [ 
      ; DPmem is a procedure that takes two arguments -- the concentration
      ; to a Dirichlet process and a base sampling procedure
      ; DPmem returns a procedure 
      DPmem (fn [concentration base]
              (let [get-value-from-cache-or-sample (mem (fn [args stick-index] 
                                                          (apply base args)))
                    get-stick-picking-procedure-from-cache (mem (fn [args] 
                                                                  (create-sethuraman-stick-breaking-process concentration)))]
                (fn [& varargs]
                  ; when the returned function is called, the first thing it does is get
                  ; the cached stick breaking procedure for the passed in arguments
                  ; and _calls_ it to get an index
                  (let [index ((get-stick-picking-procedure-from-cache varargs))]
                    ; if, for the given set of arguments and just sampled index
                    ; a return value has already been computed, get it from the cache
                    ; and return it, otherwise sample a new value
                    (get-value-from-cache-or-sample varargs index)))))

      H (fn [] (list (sample (normal 50000.00 30000.0)) (* (sample (gamma 1 1)) 3000.0)))

      base-dp (DPmem base-concentration H)

      get-group-dp (mem
                     (fn [group-id] (DPmem intra-groups-concentration base-dp)))

      sample-from-group-dp (fn [group-id]
                             ((get-group-dp group-id)))

      get-country-parameters (mem
                               (fn [country-id continent-id]
                                 (sample-from-group-dp continent-id)))]

    (map
      (fn [row]
        (let
          [
            country (nth row 0)
            indicator-value (nth row 3)
            continent (nth row 7)]
          (observe (apply normal (get-country-parameters country continent)) indicator-value)))
      input-data)

    ; predict a data point from an abstract country in Africa
    (predict 'prediction-for-Africa
             (repeatedly 1 (fn []
                               (sample (apply normal (sample-from-group-dp 'Africa))))))
    ; predict a data point from an abstract country in Europe
    (predict 'prediction-for-Europe
             (repeatedly 1 (fn []
                               (sample (apply normal (sample-from-group-dp 'Europe))))))))

(def samples
  (take 10000
        (map get-predicts
             (doquery :pgibbs
                      hdp-example-2
                      [processed-data]
                      :number-of-particles 100))))

(def predictions-for-Africa
  (apply concat (map (fn [sample] (get sample 'prediction-for-Africa)) samples)))

(def predictions-for-Europe
  (apply concat (map (fn [sample] (get sample 'prediction-for-Europe)) samples)))

(plot/compose
  (plot/histogram predictions-for-Africa :colour "#ff0000" :bins 100)
  (plot/histogram predictions-for-Europe :colour "#00ff00" :bins 100))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"58e5646d-de76-4f25-a5d1-49e556b9c121","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"58e5646d-de76-4f25-a5d1-49e556b9c121","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"58e5646d-de76-4f25-a5d1-49e556b9c121","values":[{"x":-25676.799604014024,"y":0},{"x":-23977.078153426934,"y":1.0},{"x":-22277.356702839843,"y":0.0},{"x":-20577.635252252752,"y":0.0},{"x":-18877.91380166566,"y":0.0},{"x":-17178.19235107857,"y":0.0},{"x":-15478.47090049148,"y":0.0},{"x":-13778.74944990439,"y":0.0},{"x":-12079.027999317299,"y":0.0},{"x":-10379.306548730208,"y":1.0},{"x":-8679.585098143118,"y":1.0},{"x":-6979.863647556027,"y":5.0},{"x":-5280.142196968936,"y":14.0},{"x":-3580.420746381846,"y":48.0},{"x":-1880.699295794756,"y":133.0},{"x":-180.97784520766572,"y":289.0},{"x":1518.7436053794245,"y":707.0},{"x":3218.4650559665147,"y":976.0},{"x":4918.186506553605,"y":1405.0},{"x":6617.907957140695,"y":1611.0},{"x":8317.629407727785,"y":1515.0},{"x":10017.350858314876,"y":1126.0},{"x":11717.072308901967,"y":594.0},{"x":13416.793759489057,"y":254.0},{"x":15116.515210076148,"y":92.0},{"x":16816.23666066324,"y":34.0},{"x":18515.95811125033,"y":20.0},{"x":20215.67956183742,"y":11.0},{"x":21915.40101242451,"y":21.0},{"x":23615.1224630116,"y":18.0},{"x":25314.843913598692,"y":20.0},{"x":27014.565364185783,"y":46.0},{"x":28714.286814772873,"y":91.0},{"x":30414.008265359964,"y":67.0},{"x":32113.729715947055,"y":78.0},{"x":33813.45116653414,"y":60.0},{"x":35513.17261712123,"y":51.0},{"x":37212.894067708316,"y":48.0},{"x":38912.6155182954,"y":59.0},{"x":40612.33696888249,"y":54.0},{"x":42312.05841946958,"y":51.0},{"x":44011.779870056664,"y":76.0},{"x":45711.50132064375,"y":53.0},{"x":47411.22277123084,"y":44.0},{"x":49110.944221817925,"y":59.0},{"x":50810.66567240501,"y":48.0},{"x":52510.3871229921,"y":47.0},{"x":54210.108573579186,"y":35.0},{"x":55909.83002416627,"y":18.0},{"x":57609.55147475336,"y":24.0},{"x":59309.27292534045,"y":31.0},{"x":61008.994375927534,"y":10.0},{"x":62708.71582651462,"y":12.0},{"x":64408.43727710171,"y":10.0},{"x":66108.1587276888,"y":4.0},{"x":67807.8801782759,"y":8.0},{"x":69507.60162886299,"y":2.0},{"x":71207.32307945009,"y":2.0},{"x":72907.04453003718,"y":0.0},{"x":74606.76598062427,"y":2.0},{"x":76306.48743121137,"y":4.0},{"x":78006.20888179846,"y":0.0},{"x":79705.93033238556,"y":1.0},{"x":81405.65178297265,"y":0.0},{"x":83105.37323355975,"y":0.0},{"x":84805.09468414684,"y":1.0},{"x":86504.81613473393,"y":0.0},{"x":88204.53758532103,"y":0.0},{"x":89904.25903590812,"y":1.0},{"x":91603.98048649522,"y":0.0},{"x":93303.70193708231,"y":1.0},{"x":95003.4233876694,"y":0.0},{"x":96703.1448382565,"y":0.0},{"x":98402.8662888436,"y":0.0},{"x":100102.58773943069,"y":0.0},{"x":101802.30919001778,"y":0.0},{"x":103502.03064060488,"y":0.0},{"x":105201.75209119197,"y":1.0},{"x":106901.47354177907,"y":2.0},{"x":108601.19499236616,"y":0.0},{"x":110300.91644295325,"y":0.0},{"x":112000.63789354035,"y":0.0},{"x":113700.35934412744,"y":1.0},{"x":115400.08079471454,"y":0.0},{"x":117099.80224530163,"y":0.0},{"x":118799.52369588873,"y":0.0},{"x":120499.24514647582,"y":1.0},{"x":122198.96659706291,"y":0.0},{"x":123898.68804765001,"y":0.0},{"x":125598.4094982371,"y":0.0},{"x":127298.1309488242,"y":0.0},{"x":128997.85239941129,"y":0.0},{"x":130697.57384999839,"y":0.0},{"x":132397.29530058548,"y":0.0},{"x":134097.01675117257,"y":0.0},{"x":135796.73820175967,"y":0.0},{"x":137496.45965234676,"y":0.0},{"x":139196.18110293386,"y":0.0},{"x":140895.90255352095,"y":0.0},{"x":142595.62400410805,"y":0.0},{"x":144295.34545469514,"y":1.0},{"x":145995.06690528223,"y":0}]},{"name":"2a760bb6-8105-4e4e-bc26-f852d2ed04dd","values":[{"x":-10430.760468133276,"y":0},{"x":-9311.392640426177,"y":1.0},{"x":-8192.024812719077,"y":0.0},{"x":-7072.656985011979,"y":0.0},{"x":-5953.28915730488,"y":1.0},{"x":-4833.921329597782,"y":0.0},{"x":-3714.553501890683,"y":0.0},{"x":-2595.1856741835845,"y":1.0},{"x":-1475.8178464764858,"y":4.0},{"x":-356.45001876938704,"y":14.0},{"x":762.9178089377117,"y":33.0},{"x":1882.2856366448104,"y":47.0},{"x":3001.653464351909,"y":111.0},{"x":4121.0212920590075,"y":152.0},{"x":5240.389119766106,"y":182.0},{"x":6359.7569474732045,"y":226.0},{"x":7479.124775180303,"y":221.0},{"x":8598.492602887402,"y":206.0},{"x":9717.860430594501,"y":173.0},{"x":10837.2282583016,"y":146.0},{"x":11956.5960860087,"y":108.0},{"x":13075.9639137158,"y":66.0},{"x":14195.331741422899,"y":60.0},{"x":15314.699569129998,"y":58.0},{"x":16434.067396837097,"y":82.0},{"x":17553.435224544195,"y":97.0},{"x":18672.803052251293,"y":99.0},{"x":19792.17087995839,"y":132.0},{"x":20911.538707665488,"y":125.0},{"x":22030.906535372585,"y":131.0},{"x":23150.274363079683,"y":128.0},{"x":24269.64219078678,"y":141.0},{"x":25389.01001849388,"y":152.0},{"x":26508.377846200976,"y":200.0},{"x":27627.745673908074,"y":207.0},{"x":28747.11350161517,"y":304.0},{"x":29866.48132932227,"y":353.0},{"x":30985.849157029366,"y":360.0},{"x":32105.216984736464,"y":302.0},{"x":33224.584812443565,"y":271.0},{"x":34343.95264015067,"y":259.0},{"x":35463.32046785777,"y":237.0},{"x":36582.68829556487,"y":224.0},{"x":37702.05612327197,"y":222.0},{"x":38821.42395097907,"y":247.0},{"x":39940.79177868617,"y":234.0},{"x":41060.159606393274,"y":268.0},{"x":42179.527434100375,"y":241.0},{"x":43298.895261807476,"y":262.0},{"x":44418.26308951458,"y":255.0},{"x":45537.63091722168,"y":254.0},{"x":46656.99874492878,"y":259.0},{"x":47776.36657263588,"y":238.0},{"x":48895.73440034298,"y":252.0},{"x":50015.102228050084,"y":219.0},{"x":51134.470055757185,"y":202.0},{"x":52253.837883464286,"y":163.0},{"x":53373.20571117139,"y":179.0},{"x":54492.57353887849,"y":146.0},{"x":55611.94136658559,"y":134.0},{"x":56731.30919429269,"y":118.0},{"x":57850.67702199979,"y":96.0},{"x":58970.044849706894,"y":81.0},{"x":60089.412677413995,"y":77.0},{"x":61208.780505121096,"y":54.0},{"x":62328.1483328282,"y":50.0},{"x":63447.5161605353,"y":36.0},{"x":64566.8839882424,"y":25.0},{"x":65686.2518159495,"y":16.0},{"x":66805.6196436566,"y":22.0},{"x":67924.9874713637,"y":8.0},{"x":69044.3552990708,"y":7.0},{"x":70163.7231267779,"y":10.0},{"x":71283.090954485,"y":2.0},{"x":72402.4587821921,"y":2.0},{"x":73521.8266098992,"y":0.0},{"x":74641.1944376063,"y":2.0},{"x":75760.5622653134,"y":0.0},{"x":76879.9300930205,"y":2.0},{"x":77999.29792072761,"y":0.0},{"x":79118.66574843471,"y":0.0},{"x":80238.03357614181,"y":0.0},{"x":81357.40140384891,"y":0.0},{"x":82476.76923155601,"y":0.0},{"x":83596.13705926311,"y":0.0},{"x":84715.50488697022,"y":0.0},{"x":85834.87271467732,"y":1.0},{"x":86954.24054238442,"y":0.0},{"x":88073.60837009152,"y":0.0},{"x":89192.97619779862,"y":0.0},{"x":90312.34402550572,"y":0.0},{"x":91431.71185321282,"y":1.0},{"x":92551.07968091992,"y":0.0},{"x":93670.44750862702,"y":0.0},{"x":94789.81533633413,"y":0.0},{"x":95909.18316404123,"y":0.0},{"x":97028.55099174833,"y":0.0},{"x":98147.91881945543,"y":0.0},{"x":99267.28664716253,"y":0.0},{"x":100386.65447486963,"y":0.0},{"x":101506.02230257673,"y":1.0},{"x":102625.39013028383,"y":0}]}],"marks":[{"type":"line","from":{"data":"58e5646d-de76-4f25-a5d1-49e556b9c121"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"#ff0000"},"fillOpacity":{"value":0.4},"stroke":{"value":"#ff0000"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"2a760bb6-8105-4e4e-bc26-f852d2ed04dd"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"#00ff00"},"fillOpacity":{"value":0.4},"stroke":{"value":"#00ff00"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"58e5646d-de76-4f25-a5d1-49e556b9c121\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"58e5646d-de76-4f25-a5d1-49e556b9c121\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"58e5646d-de76-4f25-a5d1-49e556b9c121\", :values ({:x -25676.799604014024, :y 0} {:x -23977.078153426934, :y 1.0} {:x -22277.356702839843, :y 0.0} {:x -20577.635252252752, :y 0.0} {:x -18877.91380166566, :y 0.0} {:x -17178.19235107857, :y 0.0} {:x -15478.47090049148, :y 0.0} {:x -13778.74944990439, :y 0.0} {:x -12079.027999317299, :y 0.0} {:x -10379.306548730208, :y 1.0} {:x -8679.585098143118, :y 1.0} {:x -6979.863647556027, :y 5.0} {:x -5280.142196968936, :y 14.0} {:x -3580.420746381846, :y 48.0} {:x -1880.699295794756, :y 133.0} {:x -180.97784520766572, :y 289.0} {:x 1518.7436053794245, :y 707.0} {:x 3218.4650559665147, :y 976.0} {:x 4918.186506553605, :y 1405.0} {:x 6617.907957140695, :y 1611.0} {:x 8317.629407727785, :y 1515.0} {:x 10017.350858314876, :y 1126.0} {:x 11717.072308901967, :y 594.0} {:x 13416.793759489057, :y 254.0} {:x 15116.515210076148, :y 92.0} {:x 16816.23666066324, :y 34.0} {:x 18515.95811125033, :y 20.0} {:x 20215.67956183742, :y 11.0} {:x 21915.40101242451, :y 21.0} {:x 23615.1224630116, :y 18.0} {:x 25314.843913598692, :y 20.0} {:x 27014.565364185783, :y 46.0} {:x 28714.286814772873, :y 91.0} {:x 30414.008265359964, :y 67.0} {:x 32113.729715947055, :y 78.0} {:x 33813.45116653414, :y 60.0} {:x 35513.17261712123, :y 51.0} {:x 37212.894067708316, :y 48.0} {:x 38912.6155182954, :y 59.0} {:x 40612.33696888249, :y 54.0} {:x 42312.05841946958, :y 51.0} {:x 44011.779870056664, :y 76.0} {:x 45711.50132064375, :y 53.0} {:x 47411.22277123084, :y 44.0} {:x 49110.944221817925, :y 59.0} {:x 50810.66567240501, :y 48.0} {:x 52510.3871229921, :y 47.0} {:x 54210.108573579186, :y 35.0} {:x 55909.83002416627, :y 18.0} {:x 57609.55147475336, :y 24.0} {:x 59309.27292534045, :y 31.0} {:x 61008.994375927534, :y 10.0} {:x 62708.71582651462, :y 12.0} {:x 64408.43727710171, :y 10.0} {:x 66108.1587276888, :y 4.0} {:x 67807.8801782759, :y 8.0} {:x 69507.60162886299, :y 2.0} {:x 71207.32307945009, :y 2.0} {:x 72907.04453003718, :y 0.0} {:x 74606.76598062427, :y 2.0} {:x 76306.48743121137, :y 4.0} {:x 78006.20888179846, :y 0.0} {:x 79705.93033238556, :y 1.0} {:x 81405.65178297265, :y 0.0} {:x 83105.37323355975, :y 0.0} {:x 84805.09468414684, :y 1.0} {:x 86504.81613473393, :y 0.0} {:x 88204.53758532103, :y 0.0} {:x 89904.25903590812, :y 1.0} {:x 91603.98048649522, :y 0.0} {:x 93303.70193708231, :y 1.0} {:x 95003.4233876694, :y 0.0} {:x 96703.1448382565, :y 0.0} {:x 98402.8662888436, :y 0.0} {:x 100102.58773943069, :y 0.0} {:x 101802.30919001778, :y 0.0} {:x 103502.03064060488, :y 0.0} {:x 105201.75209119197, :y 1.0} {:x 106901.47354177907, :y 2.0} {:x 108601.19499236616, :y 0.0} {:x 110300.91644295325, :y 0.0} {:x 112000.63789354035, :y 0.0} {:x 113700.35934412744, :y 1.0} {:x 115400.08079471454, :y 0.0} {:x 117099.80224530163, :y 0.0} {:x 118799.52369588873, :y 0.0} {:x 120499.24514647582, :y 1.0} {:x 122198.96659706291, :y 0.0} {:x 123898.68804765001, :y 0.0} {:x 125598.4094982371, :y 0.0} {:x 127298.1309488242, :y 0.0} {:x 128997.85239941129, :y 0.0} {:x 130697.57384999839, :y 0.0} {:x 132397.29530058548, :y 0.0} {:x 134097.01675117257, :y 0.0} {:x 135796.73820175967, :y 0.0} {:x 137496.45965234676, :y 0.0} {:x 139196.18110293386, :y 0.0} {:x 140895.90255352095, :y 0.0} {:x 142595.62400410805, :y 0.0} {:x 144295.34545469514, :y 1.0} {:x 145995.06690528223, :y 0})} {:name \"2a760bb6-8105-4e4e-bc26-f852d2ed04dd\", :values ({:x -10430.760468133276, :y 0} {:x -9311.392640426177, :y 1.0} {:x -8192.024812719077, :y 0.0} {:x -7072.656985011979, :y 0.0} {:x -5953.28915730488, :y 1.0} {:x -4833.921329597782, :y 0.0} {:x -3714.553501890683, :y 0.0} {:x -2595.1856741835845, :y 1.0} {:x -1475.8178464764858, :y 4.0} {:x -356.45001876938704, :y 14.0} {:x 762.9178089377117, :y 33.0} {:x 1882.2856366448104, :y 47.0} {:x 3001.653464351909, :y 111.0} {:x 4121.0212920590075, :y 152.0} {:x 5240.389119766106, :y 182.0} {:x 6359.7569474732045, :y 226.0} {:x 7479.124775180303, :y 221.0} {:x 8598.492602887402, :y 206.0} {:x 9717.860430594501, :y 173.0} {:x 10837.2282583016, :y 146.0} {:x 11956.5960860087, :y 108.0} {:x 13075.9639137158, :y 66.0} {:x 14195.331741422899, :y 60.0} {:x 15314.699569129998, :y 58.0} {:x 16434.067396837097, :y 82.0} {:x 17553.435224544195, :y 97.0} {:x 18672.803052251293, :y 99.0} {:x 19792.17087995839, :y 132.0} {:x 20911.538707665488, :y 125.0} {:x 22030.906535372585, :y 131.0} {:x 23150.274363079683, :y 128.0} {:x 24269.64219078678, :y 141.0} {:x 25389.01001849388, :y 152.0} {:x 26508.377846200976, :y 200.0} {:x 27627.745673908074, :y 207.0} {:x 28747.11350161517, :y 304.0} {:x 29866.48132932227, :y 353.0} {:x 30985.849157029366, :y 360.0} {:x 32105.216984736464, :y 302.0} {:x 33224.584812443565, :y 271.0} {:x 34343.95264015067, :y 259.0} {:x 35463.32046785777, :y 237.0} {:x 36582.68829556487, :y 224.0} {:x 37702.05612327197, :y 222.0} {:x 38821.42395097907, :y 247.0} {:x 39940.79177868617, :y 234.0} {:x 41060.159606393274, :y 268.0} {:x 42179.527434100375, :y 241.0} {:x 43298.895261807476, :y 262.0} {:x 44418.26308951458, :y 255.0} {:x 45537.63091722168, :y 254.0} {:x 46656.99874492878, :y 259.0} {:x 47776.36657263588, :y 238.0} {:x 48895.73440034298, :y 252.0} {:x 50015.102228050084, :y 219.0} {:x 51134.470055757185, :y 202.0} {:x 52253.837883464286, :y 163.0} {:x 53373.20571117139, :y 179.0} {:x 54492.57353887849, :y 146.0} {:x 55611.94136658559, :y 134.0} {:x 56731.30919429269, :y 118.0} {:x 57850.67702199979, :y 96.0} {:x 58970.044849706894, :y 81.0} {:x 60089.412677413995, :y 77.0} {:x 61208.780505121096, :y 54.0} {:x 62328.1483328282, :y 50.0} {:x 63447.5161605353, :y 36.0} {:x 64566.8839882424, :y 25.0} {:x 65686.2518159495, :y 16.0} {:x 66805.6196436566, :y 22.0} {:x 67924.9874713637, :y 8.0} {:x 69044.3552990708, :y 7.0} {:x 70163.7231267779, :y 10.0} {:x 71283.090954485, :y 2.0} {:x 72402.4587821921, :y 2.0} {:x 73521.8266098992, :y 0.0} {:x 74641.1944376063, :y 2.0} {:x 75760.5622653134, :y 0.0} {:x 76879.9300930205, :y 2.0} {:x 77999.29792072761, :y 0.0} {:x 79118.66574843471, :y 0.0} {:x 80238.03357614181, :y 0.0} {:x 81357.40140384891, :y 0.0} {:x 82476.76923155601, :y 0.0} {:x 83596.13705926311, :y 0.0} {:x 84715.50488697022, :y 0.0} {:x 85834.87271467732, :y 1.0} {:x 86954.24054238442, :y 0.0} {:x 88073.60837009152, :y 0.0} {:x 89192.97619779862, :y 0.0} {:x 90312.34402550572, :y 0.0} {:x 91431.71185321282, :y 1.0} {:x 92551.07968091992, :y 0.0} {:x 93670.44750862702, :y 0.0} {:x 94789.81533633413, :y 0.0} {:x 95909.18316404123, :y 0.0} {:x 97028.55099174833, :y 0.0} {:x 98147.91881945543, :y 0.0} {:x 99267.28664716253, :y 0.0} {:x 100386.65447486963, :y 0.0} {:x 101506.02230257673, :y 1.0} {:x 102625.39013028383, :y 0})}), :marks ({:type \"line\", :from {:data \"58e5646d-de76-4f25-a5d1-49e556b9c121\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"#ff0000\"}, :fillOpacity {:value 0.4}, :stroke {:value \"#ff0000\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"2a760bb6-8105-4e4e-bc26-f852d2ed04dd\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"#00ff00\"}, :fillOpacity {:value 0.4}, :stroke {:value \"#00ff00\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; @@

;; @@
