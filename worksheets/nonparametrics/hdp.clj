;; gorilla-repl.fileformat = 1

;; **
;;; # Hierarchical Dirichlet Process
;; **

;; @@
(ns hdp
  (:require [gorilla-plot.core :as plot]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io]
            [anglican.stat :as stat])
  (:use [anglican core emit runtime 
         [inference :only [collect-by]]
         [state :only [get-predicts get-log-weight]]]))
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
          (observe (apply normal (get-country-parameters country continent)) 
                   indicator-value)))
      input-data)

    ; predict how similar Iceland and Germany are 
    
             (= (get-country-parameters "Iceland" "Europe")
                (get-country-parameters "Germany" "Europe"))))

(def samples
  (take 100
             (doquery :smc
                      hdp-example-1
                      [processed-data]
                      :number-of-particles 100)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;hdp/samples</span>","value":"#'hdp/samples"}
;; <=

;; @@
(->> (doquery :smc hdp-example-1 [processed-data] :number-of-particles 100)
     (take 1000)
     (collect-by :result)
     (stat/empirical-distribution)
     (#(plot/bar-chart (keys %) (vals %))))
;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"data":[{"name":"1c1aeea2-975e-429d-8099-35c2b515dbbb","values":[{"x":true,"y":0.3400000002798596},{"x":false,"y":0.6599999997201909}]}],"marks":[{"type":"rect","from":{"data":"1c1aeea2-975e-429d-8099-35c2b515dbbb"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"width":{"scale":"x","band":true,"offset":-1},"y":{"scale":"y","field":"data.y"},"y2":{"scale":"y","value":0}},"update":{"fill":{"value":"steelblue"},"opacity":{"value":1}},"hover":{"fill":{"value":"#FF29D2"}}}}],"scales":[{"name":"x","type":"ordinal","range":"width","domain":{"data":"1c1aeea2-975e-429d-8099-35c2b515dbbb","field":"data.x"}},{"name":"y","range":"height","nice":true,"domain":{"data":"1c1aeea2-975e-429d-8099-35c2b515dbbb","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :data [{:name \"1c1aeea2-975e-429d-8099-35c2b515dbbb\", :values ({:x true, :y 0.3400000002798596} {:x false, :y 0.6599999997201909})}], :marks [{:type \"rect\", :from {:data \"1c1aeea2-975e-429d-8099-35c2b515dbbb\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :width {:scale \"x\", :band true, :offset -1}, :y {:scale \"y\", :field \"data.y\"}, :y2 {:scale \"y\", :value 0}}, :update {:fill {:value \"steelblue\"}, :opacity {:value 1}}, :hover {:fill {:value \"#FF29D2\"}}}}], :scales [{:name \"x\", :type \"ordinal\", :range \"width\", :domain {:data \"1c1aeea2-975e-429d-8099-35c2b515dbbb\", :field \"data.x\"}} {:name \"y\", :range \"height\", :nice true, :domain {:data \"1c1aeea2-975e-429d-8099-35c2b515dbbb\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}]}}"}
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
    {:prediction-for-Africa
             (repeatedly 1 (fn []
                               (sample (apply normal (sample-from-group-dp 'Africa)))))
    ; predict a data point from an abstract country in Europe
    :prediction-for-Europe
             (repeatedly 1 (fn []
                               (sample (apply normal (sample-from-group-dp 'Europe)))))}))

(def samples
  (take 10000
             (doquery :smc
                      hdp-example-2
                      [processed-data]
                      :number-of-particles 100)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;hdp/samples</span>","value":"#'hdp/samples"}
;; <=

;; @@
(let [getres (fn [ky] (map (fn [[m w]] (first (ky m)))
                           (->> (take 5000 samples)
                             (collect-by :result)
                             (stat/empirical-distribution))))]
  (plot/compose
    (plot/histogram (getres :prediction-for-Africa) :colour "#ff0000" :bins 100)
    (plot/histogram (getres :prediction-for-Europe) :colour "#00ff00" :bins 100)))

;; @@
;; =>
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"top":10,"left":55,"bottom":40,"right":10},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"3e6d7834-9fa6-4eb9-a2a3-53ec9dd5dd6a","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"3e6d7834-9fa6-4eb9-a2a3-53ec9dd5dd6a","field":"data.y"}}],"axes":[{"type":"x","scale":"x"},{"type":"y","scale":"y"}],"data":[{"name":"3e6d7834-9fa6-4eb9-a2a3-53ec9dd5dd6a","values":[{"x":-31930.96821582002,"y":0},{"x":-30438.341888494542,"y":1.0},{"x":-28945.715561169065,"y":0.0},{"x":-27453.089233843588,"y":0.0},{"x":-25960.46290651811,"y":2.0},{"x":-24467.836579192634,"y":2.0},{"x":-22975.210251867156,"y":2.0},{"x":-21482.58392454168,"y":2.0},{"x":-19989.957597216202,"y":7.0},{"x":-18497.331269890725,"y":5.0},{"x":-17004.704942565248,"y":9.0},{"x":-15512.07861523977,"y":12.0},{"x":-14019.452287914293,"y":14.0},{"x":-12526.825960588816,"y":17.0},{"x":-11034.199633263339,"y":21.0},{"x":-9541.573305937862,"y":28.0},{"x":-8048.946978612385,"y":41.0},{"x":-6556.320651286907,"y":53.0},{"x":-5063.69432396143,"y":77.0},{"x":-3571.0679966359526,"y":124.0},{"x":-2078.441669310475,"y":176.0},{"x":-585.8153419849973,"y":266.0},{"x":906.8109853404803,"y":409.0},{"x":2399.437312665958,"y":795.0},{"x":3892.0636399914356,"y":567.0},{"x":5384.689967316914,"y":343.0},{"x":6877.316294642391,"y":277.0},{"x":8369.942621967868,"y":183.0},{"x":9862.568949293345,"y":189.0},{"x":11355.195276618822,"y":163.0},{"x":12847.8216039443,"y":136.0},{"x":14340.447931269777,"y":123.0},{"x":15833.074258595254,"y":96.0},{"x":17325.70058592073,"y":85.0},{"x":18818.32691324621,"y":68.0},{"x":20310.953240571685,"y":61.0},{"x":21803.579567897163,"y":66.0},{"x":23296.20589522264,"y":37.0},{"x":24788.832222548117,"y":43.0},{"x":26281.458549873594,"y":48.0},{"x":27774.08487719907,"y":38.0},{"x":29266.71120452455,"y":43.0},{"x":30759.337531850026,"y":36.0},{"x":32251.963859175503,"y":25.0},{"x":33744.59018650098,"y":27.0},{"x":35237.21651382646,"y":24.0},{"x":36729.84284115194,"y":25.0},{"x":38222.46916847742,"y":28.0},{"x":39715.0954958029,"y":24.0},{"x":41207.721823128384,"y":19.0},{"x":42700.348150453865,"y":14.0},{"x":44192.974477779346,"y":17.0},{"x":45685.60080510483,"y":11.0},{"x":47178.22713243031,"y":17.0},{"x":48670.85345975579,"y":21.0},{"x":50163.47978708127,"y":21.0},{"x":51656.10611440675,"y":6.0},{"x":53148.73244173223,"y":5.0},{"x":54641.35876905771,"y":5.0},{"x":56133.98509638319,"y":5.0},{"x":57626.61142370867,"y":3.0},{"x":59119.237751034154,"y":2.0},{"x":60611.864078359635,"y":2.0},{"x":62104.490405685116,"y":4.0},{"x":63597.1167330106,"y":1.0},{"x":65089.74306033608,"y":1.0},{"x":66582.36938766156,"y":3.0},{"x":68074.99571498703,"y":0.0},{"x":69567.6220423125,"y":3.0},{"x":71060.24836963798,"y":3.0},{"x":72552.87469696345,"y":1.0},{"x":74045.50102428893,"y":2.0},{"x":75538.1273516144,"y":1.0},{"x":77030.75367893987,"y":0.0},{"x":78523.38000626535,"y":3.0},{"x":80016.00633359082,"y":1.0},{"x":81508.6326609163,"y":2.0},{"x":83001.25898824177,"y":0.0},{"x":84493.88531556724,"y":0.0},{"x":85986.51164289271,"y":1.0},{"x":87479.13797021819,"y":0.0},{"x":88971.76429754366,"y":1.0},{"x":90464.39062486913,"y":1.0},{"x":91957.01695219461,"y":1.0},{"x":93449.64327952008,"y":1.0},{"x":94942.26960684556,"y":1.0},{"x":96434.89593417103,"y":0.0},{"x":97927.5222614965,"y":0.0},{"x":99420.14858882198,"y":0.0},{"x":100912.77491614745,"y":1.0},{"x":102405.40124347292,"y":0.0},{"x":103898.0275707984,"y":0.0},{"x":105390.65389812387,"y":0.0},{"x":106883.28022544934,"y":1.0},{"x":108375.90655277482,"y":0.0},{"x":109868.53288010029,"y":0.0},{"x":111361.15920742576,"y":0.0},{"x":112853.78553475124,"y":0.0},{"x":114346.41186207671,"y":0.0},{"x":115839.03818940218,"y":0.0},{"x":117331.66451672766,"y":0.0},{"x":118824.29084405313,"y":1.0},{"x":120316.9171713786,"y":0}]},{"name":"0a3d5ddf-7883-4ba9-b485-7bb3726d6463","values":[{"x":-37754.3268703162,"y":0},{"x":-36177.929032473396,"y":1.0},{"x":-34601.53119463059,"y":0.0},{"x":-33025.13335678778,"y":0.0},{"x":-31448.735518944974,"y":2.0},{"x":-29872.337681102166,"y":2.0},{"x":-28295.93984325936,"y":0.0},{"x":-26719.54200541655,"y":2.0},{"x":-25143.144167573744,"y":1.0},{"x":-23566.746329730937,"y":0.0},{"x":-21990.34849188813,"y":1.0},{"x":-20413.950654045322,"y":2.0},{"x":-18837.552816202515,"y":5.0},{"x":-17261.154978359707,"y":3.0},{"x":-15684.7571405169,"y":2.0},{"x":-14108.359302674093,"y":4.0},{"x":-12531.961464831285,"y":8.0},{"x":-10955.563626988478,"y":7.0},{"x":-9379.16578914567,"y":7.0},{"x":-7802.7679513028625,"y":13.0},{"x":-6226.370113460054,"y":16.0},{"x":-4649.972275617246,"y":20.0},{"x":-3073.5744377744377,"y":29.0},{"x":-1497.1765999316294,"y":44.0},{"x":79.22123791117883,"y":57.0},{"x":1655.619075753987,"y":92.0},{"x":3232.0169135967953,"y":143.0},{"x":4808.414751439604,"y":122.0},{"x":6384.812589282412,"y":149.0},{"x":7961.21042712522,"y":135.0},{"x":9537.608264968028,"y":131.0},{"x":11114.006102810836,"y":109.0},{"x":12690.403940653643,"y":126.0},{"x":14266.80177849645,"y":137.0},{"x":15843.199616339258,"y":123.0},{"x":17419.597454182065,"y":114.0},{"x":18995.995292024872,"y":168.0},{"x":20572.39312986768,"y":280.0},{"x":22148.790967710487,"y":230.0},{"x":23725.188805553295,"y":167.0},{"x":25301.586643396102,"y":162.0},{"x":26877.98448123891,"y":147.0},{"x":28454.382319081717,"y":141.0},{"x":30030.780156924524,"y":119.0},{"x":31607.17799476733,"y":112.0},{"x":33183.57583261014,"y":102.0},{"x":34759.97367045295,"y":149.0},{"x":36336.37150829576,"y":130.0},{"x":37912.769346138564,"y":133.0},{"x":39489.16718398137,"y":144.0},{"x":41065.56502182418,"y":162.0},{"x":42641.962859666986,"y":124.0},{"x":44218.360697509794,"y":94.0},{"x":45794.7585353526,"y":92.0},{"x":47371.15637319541,"y":107.0},{"x":48947.554211038216,"y":150.0},{"x":50523.95204888102,"y":127.0},{"x":52100.34988672383,"y":68.0},{"x":53676.74772456664,"y":31.0},{"x":55253.145562409445,"y":30.0},{"x":56829.54340025225,"y":14.0},{"x":58405.94123809506,"y":43.0},{"x":59982.33907593787,"y":14.0},{"x":61558.736913780675,"y":20.0},{"x":63135.13475162348,"y":11.0},{"x":64711.53258946629,"y":9.0},{"x":66287.9304273091,"y":13.0},{"x":67864.32826515191,"y":10.0},{"x":69440.72610299473,"y":11.0},{"x":71017.12394083754,"y":8.0},{"x":72593.52177868036,"y":6.0},{"x":74169.91961652317,"y":8.0},{"x":75746.31745436598,"y":2.0},{"x":77322.7152922088,"y":4.0},{"x":78899.11313005161,"y":4.0},{"x":80475.51096789443,"y":3.0},{"x":82051.90880573724,"y":7.0},{"x":83628.30664358006,"y":3.0},{"x":85204.70448142287,"y":3.0},{"x":86781.10231926569,"y":4.0},{"x":88357.5001571085,"y":3.0},{"x":89933.89799495132,"y":1.0},{"x":91510.29583279413,"y":3.0},{"x":93086.69367063695,"y":4.0},{"x":94663.09150847976,"y":3.0},{"x":96239.48934632257,"y":2.0},{"x":97815.88718416539,"y":2.0},{"x":99392.2850220082,"y":0.0},{"x":100968.68285985102,"y":1.0},{"x":102545.08069769383,"y":2.0},{"x":104121.47853553665,"y":1.0},{"x":105697.87637337946,"y":0.0},{"x":107274.27421122228,"y":0.0},{"x":108850.67204906509,"y":2.0},{"x":110427.0698869079,"y":1.0},{"x":112003.46772475072,"y":0.0},{"x":113579.86556259354,"y":1.0},{"x":115156.26340043635,"y":0.0},{"x":116732.66123827916,"y":0.0},{"x":118309.05907612198,"y":0.0},{"x":119885.4569139648,"y":1.0},{"x":121461.85475180761,"y":0}]}],"marks":[{"type":"line","from":{"data":"3e6d7834-9fa6-4eb9-a2a3-53ec9dd5dd6a"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"#ff0000"},"fillOpacity":{"value":0.4},"stroke":{"value":"#ff0000"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"0a3d5ddf-7883-4ba9-b485-7bb3726d6463"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"#00ff00"},"fillOpacity":{"value":0.4},"stroke":{"value":"#00ff00"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:top 10, :left 55, :bottom 40, :right 10}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"3e6d7834-9fa6-4eb9-a2a3-53ec9dd5dd6a\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"3e6d7834-9fa6-4eb9-a2a3-53ec9dd5dd6a\", :field \"data.y\"}}], :axes [{:type \"x\", :scale \"x\"} {:type \"y\", :scale \"y\"}], :data ({:name \"3e6d7834-9fa6-4eb9-a2a3-53ec9dd5dd6a\", :values ({:x -31930.96821582002, :y 0} {:x -30438.341888494542, :y 1.0} {:x -28945.715561169065, :y 0.0} {:x -27453.089233843588, :y 0.0} {:x -25960.46290651811, :y 2.0} {:x -24467.836579192634, :y 2.0} {:x -22975.210251867156, :y 2.0} {:x -21482.58392454168, :y 2.0} {:x -19989.957597216202, :y 7.0} {:x -18497.331269890725, :y 5.0} {:x -17004.704942565248, :y 9.0} {:x -15512.07861523977, :y 12.0} {:x -14019.452287914293, :y 14.0} {:x -12526.825960588816, :y 17.0} {:x -11034.199633263339, :y 21.0} {:x -9541.573305937862, :y 28.0} {:x -8048.946978612385, :y 41.0} {:x -6556.320651286907, :y 53.0} {:x -5063.69432396143, :y 77.0} {:x -3571.0679966359526, :y 124.0} {:x -2078.441669310475, :y 176.0} {:x -585.8153419849973, :y 266.0} {:x 906.8109853404803, :y 409.0} {:x 2399.437312665958, :y 795.0} {:x 3892.0636399914356, :y 567.0} {:x 5384.689967316914, :y 343.0} {:x 6877.316294642391, :y 277.0} {:x 8369.942621967868, :y 183.0} {:x 9862.568949293345, :y 189.0} {:x 11355.195276618822, :y 163.0} {:x 12847.8216039443, :y 136.0} {:x 14340.447931269777, :y 123.0} {:x 15833.074258595254, :y 96.0} {:x 17325.70058592073, :y 85.0} {:x 18818.32691324621, :y 68.0} {:x 20310.953240571685, :y 61.0} {:x 21803.579567897163, :y 66.0} {:x 23296.20589522264, :y 37.0} {:x 24788.832222548117, :y 43.0} {:x 26281.458549873594, :y 48.0} {:x 27774.08487719907, :y 38.0} {:x 29266.71120452455, :y 43.0} {:x 30759.337531850026, :y 36.0} {:x 32251.963859175503, :y 25.0} {:x 33744.59018650098, :y 27.0} {:x 35237.21651382646, :y 24.0} {:x 36729.84284115194, :y 25.0} {:x 38222.46916847742, :y 28.0} {:x 39715.0954958029, :y 24.0} {:x 41207.721823128384, :y 19.0} {:x 42700.348150453865, :y 14.0} {:x 44192.974477779346, :y 17.0} {:x 45685.60080510483, :y 11.0} {:x 47178.22713243031, :y 17.0} {:x 48670.85345975579, :y 21.0} {:x 50163.47978708127, :y 21.0} {:x 51656.10611440675, :y 6.0} {:x 53148.73244173223, :y 5.0} {:x 54641.35876905771, :y 5.0} {:x 56133.98509638319, :y 5.0} {:x 57626.61142370867, :y 3.0} {:x 59119.237751034154, :y 2.0} {:x 60611.864078359635, :y 2.0} {:x 62104.490405685116, :y 4.0} {:x 63597.1167330106, :y 1.0} {:x 65089.74306033608, :y 1.0} {:x 66582.36938766156, :y 3.0} {:x 68074.99571498703, :y 0.0} {:x 69567.6220423125, :y 3.0} {:x 71060.24836963798, :y 3.0} {:x 72552.87469696345, :y 1.0} {:x 74045.50102428893, :y 2.0} {:x 75538.1273516144, :y 1.0} {:x 77030.75367893987, :y 0.0} {:x 78523.38000626535, :y 3.0} {:x 80016.00633359082, :y 1.0} {:x 81508.6326609163, :y 2.0} {:x 83001.25898824177, :y 0.0} {:x 84493.88531556724, :y 0.0} {:x 85986.51164289271, :y 1.0} {:x 87479.13797021819, :y 0.0} {:x 88971.76429754366, :y 1.0} {:x 90464.39062486913, :y 1.0} {:x 91957.01695219461, :y 1.0} {:x 93449.64327952008, :y 1.0} {:x 94942.26960684556, :y 1.0} {:x 96434.89593417103, :y 0.0} {:x 97927.5222614965, :y 0.0} {:x 99420.14858882198, :y 0.0} {:x 100912.77491614745, :y 1.0} {:x 102405.40124347292, :y 0.0} {:x 103898.0275707984, :y 0.0} {:x 105390.65389812387, :y 0.0} {:x 106883.28022544934, :y 1.0} {:x 108375.90655277482, :y 0.0} {:x 109868.53288010029, :y 0.0} {:x 111361.15920742576, :y 0.0} {:x 112853.78553475124, :y 0.0} {:x 114346.41186207671, :y 0.0} {:x 115839.03818940218, :y 0.0} {:x 117331.66451672766, :y 0.0} {:x 118824.29084405313, :y 1.0} {:x 120316.9171713786, :y 0})} {:name \"0a3d5ddf-7883-4ba9-b485-7bb3726d6463\", :values ({:x -37754.3268703162, :y 0} {:x -36177.929032473396, :y 1.0} {:x -34601.53119463059, :y 0.0} {:x -33025.13335678778, :y 0.0} {:x -31448.735518944974, :y 2.0} {:x -29872.337681102166, :y 2.0} {:x -28295.93984325936, :y 0.0} {:x -26719.54200541655, :y 2.0} {:x -25143.144167573744, :y 1.0} {:x -23566.746329730937, :y 0.0} {:x -21990.34849188813, :y 1.0} {:x -20413.950654045322, :y 2.0} {:x -18837.552816202515, :y 5.0} {:x -17261.154978359707, :y 3.0} {:x -15684.7571405169, :y 2.0} {:x -14108.359302674093, :y 4.0} {:x -12531.961464831285, :y 8.0} {:x -10955.563626988478, :y 7.0} {:x -9379.16578914567, :y 7.0} {:x -7802.7679513028625, :y 13.0} {:x -6226.370113460054, :y 16.0} {:x -4649.972275617246, :y 20.0} {:x -3073.5744377744377, :y 29.0} {:x -1497.1765999316294, :y 44.0} {:x 79.22123791117883, :y 57.0} {:x 1655.619075753987, :y 92.0} {:x 3232.0169135967953, :y 143.0} {:x 4808.414751439604, :y 122.0} {:x 6384.812589282412, :y 149.0} {:x 7961.21042712522, :y 135.0} {:x 9537.608264968028, :y 131.0} {:x 11114.006102810836, :y 109.0} {:x 12690.403940653643, :y 126.0} {:x 14266.80177849645, :y 137.0} {:x 15843.199616339258, :y 123.0} {:x 17419.597454182065, :y 114.0} {:x 18995.995292024872, :y 168.0} {:x 20572.39312986768, :y 280.0} {:x 22148.790967710487, :y 230.0} {:x 23725.188805553295, :y 167.0} {:x 25301.586643396102, :y 162.0} {:x 26877.98448123891, :y 147.0} {:x 28454.382319081717, :y 141.0} {:x 30030.780156924524, :y 119.0} {:x 31607.17799476733, :y 112.0} {:x 33183.57583261014, :y 102.0} {:x 34759.97367045295, :y 149.0} {:x 36336.37150829576, :y 130.0} {:x 37912.769346138564, :y 133.0} {:x 39489.16718398137, :y 144.0} {:x 41065.56502182418, :y 162.0} {:x 42641.962859666986, :y 124.0} {:x 44218.360697509794, :y 94.0} {:x 45794.7585353526, :y 92.0} {:x 47371.15637319541, :y 107.0} {:x 48947.554211038216, :y 150.0} {:x 50523.95204888102, :y 127.0} {:x 52100.34988672383, :y 68.0} {:x 53676.74772456664, :y 31.0} {:x 55253.145562409445, :y 30.0} {:x 56829.54340025225, :y 14.0} {:x 58405.94123809506, :y 43.0} {:x 59982.33907593787, :y 14.0} {:x 61558.736913780675, :y 20.0} {:x 63135.13475162348, :y 11.0} {:x 64711.53258946629, :y 9.0} {:x 66287.9304273091, :y 13.0} {:x 67864.32826515191, :y 10.0} {:x 69440.72610299473, :y 11.0} {:x 71017.12394083754, :y 8.0} {:x 72593.52177868036, :y 6.0} {:x 74169.91961652317, :y 8.0} {:x 75746.31745436598, :y 2.0} {:x 77322.7152922088, :y 4.0} {:x 78899.11313005161, :y 4.0} {:x 80475.51096789443, :y 3.0} {:x 82051.90880573724, :y 7.0} {:x 83628.30664358006, :y 3.0} {:x 85204.70448142287, :y 3.0} {:x 86781.10231926569, :y 4.0} {:x 88357.5001571085, :y 3.0} {:x 89933.89799495132, :y 1.0} {:x 91510.29583279413, :y 3.0} {:x 93086.69367063695, :y 4.0} {:x 94663.09150847976, :y 3.0} {:x 96239.48934632257, :y 2.0} {:x 97815.88718416539, :y 2.0} {:x 99392.2850220082, :y 0.0} {:x 100968.68285985102, :y 1.0} {:x 102545.08069769383, :y 2.0} {:x 104121.47853553665, :y 1.0} {:x 105697.87637337946, :y 0.0} {:x 107274.27421122228, :y 0.0} {:x 108850.67204906509, :y 2.0} {:x 110427.0698869079, :y 1.0} {:x 112003.46772475072, :y 0.0} {:x 113579.86556259354, :y 1.0} {:x 115156.26340043635, :y 0.0} {:x 116732.66123827916, :y 0.0} {:x 118309.05907612198, :y 0.0} {:x 119885.4569139648, :y 1.0} {:x 121461.85475180761, :y 0})}), :marks ({:type \"line\", :from {:data \"3e6d7834-9fa6-4eb9-a2a3-53ec9dd5dd6a\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"#ff0000\"}, :fillOpacity {:value 0.4}, :stroke {:value \"#ff0000\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"0a3d5ddf-7883-4ba9-b485-7bb3726d6463\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"#00ff00\"}, :fillOpacity {:value 0.4}, :stroke {:value \"#00ff00\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=
