;; gorilla-repl.fileformat = 1

;; **
;;; # Hierarchical Dirichlet Process
;; **

;; @@
(ns hdp
  (:require [gorilla-plot.core :as plot]
            [clojure.data.csv :as csv]
            [clojure.java.io :as io])
  (:use [anglican core emit runtime [state :only [get-predicts get-log-weight]]]))
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
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"235a38eb-9b1c-4dff-bdcf-75f89883aba9","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"235a38eb-9b1c-4dff-bdcf-75f89883aba9","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"235a38eb-9b1c-4dff-bdcf-75f89883aba9"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"235a38eb-9b1c-4dff-bdcf-75f89883aba9","values":[{"x":0.0,"y":0},{"x":0.12500000000000003,"y":45.0},{"x":0.25000000000000006,"y":0.0},{"x":0.3750000000000001,"y":0.0},{"x":0.5000000000000001,"y":0.0},{"x":0.6250000000000001,"y":0.0},{"x":0.7500000000000001,"y":0.0},{"x":0.8750000000000001,"y":0.0},{"x":1.0000000000000002,"y":55.0},{"x":1.1250000000000002,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"235a38eb-9b1c-4dff-bdcf-75f89883aba9\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"235a38eb-9b1c-4dff-bdcf-75f89883aba9\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"235a38eb-9b1c-4dff-bdcf-75f89883aba9\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"235a38eb-9b1c-4dff-bdcf-75f89883aba9\", :values ({:x 0.0, :y 0} {:x 0.12500000000000003, :y 45.0} {:x 0.25000000000000006, :y 0.0} {:x 0.3750000000000001, :y 0.0} {:x 0.5000000000000001, :y 0.0} {:x 0.6250000000000001, :y 0.0} {:x 0.7500000000000001, :y 0.0} {:x 0.8750000000000001, :y 0.0} {:x 1.0000000000000002, :y 55.0} {:x 1.1250000000000002, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}}}"}
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
;;; {"type":"vega","content":{"width":400,"height":247.2187957763672,"padding":{"bottom":40,"top":10,"right":10,"left":55},"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"e3366b69-ec2f-4bb8-913e-fc93029a8c14","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"e3366b69-ec2f-4bb8-913e-fc93029a8c14","field":"data.y"}}],"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"data":[{"name":"e3366b69-ec2f-4bb8-913e-fc93029a8c14","values":[{"x":-12062.868845969851,"y":0},{"x":-10638.213017945329,"y":4.0},{"x":-9213.557189920806,"y":7.0},{"x":-7788.901361896284,"y":23.0},{"x":-6364.245533871761,"y":77.0},{"x":-4939.589705847238,"y":179.0},{"x":-3514.9338778227157,"y":385.0},{"x":-2090.278049798193,"y":704.0},{"x":-665.6222217736706,"y":1134.0},{"x":759.0336062508518,"y":1396.0},{"x":2183.689434275374,"y":1549.0},{"x":3608.3452622998966,"y":1384.0},{"x":5033.001090324419,"y":1074.0},{"x":6457.656918348942,"y":679.0},{"x":7882.3127463734645,"y":390.0},{"x":9306.968574397986,"y":182.0},{"x":10731.624402422509,"y":112.0},{"x":12156.280230447031,"y":71.0},{"x":13580.936058471554,"y":50.0},{"x":15005.591886496077,"y":36.0},{"x":16430.2477145206,"y":35.0},{"x":17854.903542545122,"y":16.0},{"x":19279.559370569645,"y":19.0},{"x":20704.215198594167,"y":35.0},{"x":22128.87102661869,"y":31.0},{"x":23553.526854643213,"y":23.0},{"x":24978.182682667735,"y":39.0},{"x":26402.838510692258,"y":24.0},{"x":27827.49433871678,"y":26.0},{"x":29252.150166741303,"y":32.0},{"x":30676.805994765826,"y":20.0},{"x":32101.46182279035,"y":25.0},{"x":33526.11765081487,"y":22.0},{"x":34950.773478839394,"y":28.0},{"x":36375.429306863916,"y":39.0},{"x":37800.08513488844,"y":42.0},{"x":39224.74096291296,"y":18.0},{"x":40649.396790937484,"y":11.0},{"x":42074.05261896201,"y":6.0},{"x":43498.70844698653,"y":7.0},{"x":44923.36427501105,"y":0.0},{"x":46348.020103035575,"y":2.0},{"x":47772.6759310601,"y":3.0},{"x":49197.33175908462,"y":3.0},{"x":50621.98758710914,"y":7.0},{"x":52046.643415133665,"y":2.0},{"x":53471.29924315819,"y":5.0},{"x":54895.95507118271,"y":1.0},{"x":56320.61089920723,"y":2.0},{"x":57745.266727231756,"y":5.0},{"x":59169.92255525628,"y":2.0},{"x":60594.5783832808,"y":1.0},{"x":62019.234211305324,"y":1.0},{"x":63443.890039329846,"y":1.0},{"x":64868.54586735437,"y":0.0},{"x":66293.20169537888,"y":2.0},{"x":67717.8575234034,"y":6.0},{"x":69142.51335142793,"y":0.0},{"x":70567.16917945245,"y":0.0},{"x":71991.82500747697,"y":1.0},{"x":73416.4808355015,"y":1.0},{"x":74841.13666352602,"y":0.0},{"x":76265.79249155054,"y":0.0},{"x":77690.44831957507,"y":1.0},{"x":79115.10414759959,"y":3.0},{"x":80539.75997562411,"y":3.0},{"x":81964.41580364863,"y":1.0},{"x":83389.07163167316,"y":0.0},{"x":84813.72745969768,"y":0.0},{"x":86238.3832877222,"y":1.0},{"x":87663.03911574672,"y":0.0},{"x":89087.69494377125,"y":1.0},{"x":90512.35077179577,"y":2.0},{"x":91937.00659982029,"y":1.0},{"x":93361.66242784481,"y":1.0},{"x":94786.31825586934,"y":1.0},{"x":96210.97408389386,"y":0.0},{"x":97635.62991191838,"y":0.0},{"x":99060.2857399429,"y":0.0},{"x":100484.94156796743,"y":2.0},{"x":101909.59739599195,"y":1.0},{"x":103334.25322401647,"y":0.0},{"x":104758.909052041,"y":0.0},{"x":106183.56488006552,"y":0.0},{"x":107608.22070809004,"y":1.0},{"x":109032.87653611456,"y":0.0},{"x":110457.53236413909,"y":0.0},{"x":111882.18819216361,"y":0.0},{"x":113306.84402018813,"y":0.0},{"x":114731.49984821265,"y":0.0},{"x":116156.15567623718,"y":0.0},{"x":117580.8115042617,"y":0.0},{"x":119005.46733228622,"y":1.0},{"x":120430.12316031074,"y":0.0},{"x":121854.77898833527,"y":0.0},{"x":123279.43481635979,"y":0.0},{"x":124704.09064438431,"y":0.0},{"x":126128.74647240883,"y":0.0},{"x":127553.40230043336,"y":0.0},{"x":128978.05812845788,"y":0.0},{"x":130402.7139564824,"y":1.0},{"x":131827.3697845069,"y":0}]},{"name":"2478bb71-e9d5-4266-9f21-ac04a5d724c5","values":[{"x":-8816.856612801239,"y":0},{"x":-7666.760200589816,"y":1.0},{"x":-6516.663788378393,"y":0.0},{"x":-5366.5673761669705,"y":3.0},{"x":-4216.470963955548,"y":9.0},{"x":-3066.374551744125,"y":15.0},{"x":-1916.278139532702,"y":34.0},{"x":-766.1817273212791,"y":61.0},{"x":383.9146848901437,"y":74.0},{"x":1534.0110971015665,"y":129.0},{"x":2684.1075093129894,"y":163.0},{"x":3834.203921524412,"y":207.0},{"x":4984.300333735835,"y":190.0},{"x":6134.396745947258,"y":168.0},{"x":7284.493158158681,"y":130.0},{"x":8434.589570370103,"y":82.0},{"x":9584.685982581526,"y":59.0},{"x":10734.78239479295,"y":36.0},{"x":11884.878807004374,"y":26.0},{"x":13034.975219215798,"y":26.0},{"x":14185.071631427221,"y":27.0},{"x":15335.168043638645,"y":54.0},{"x":16485.26445585007,"y":102.0},{"x":17635.360868061493,"y":248.0},{"x":18785.457280272916,"y":492.0},{"x":19935.55369248434,"y":883.0},{"x":21085.650104695764,"y":1250.0},{"x":22235.746516907187,"y":1261.0},{"x":23385.84292911861,"y":1037.0},{"x":24535.939341330035,"y":663.0},{"x":25686.03575354146,"y":328.0},{"x":26836.132165752882,"y":173.0},{"x":27986.228577964306,"y":108.0},{"x":29136.32499017573,"y":105.0},{"x":30286.421402387154,"y":94.0},{"x":31436.517814598577,"y":96.0},{"x":32586.61422681,"y":93.0},{"x":33736.710639021425,"y":96.0},{"x":34886.80705123285,"y":81.0},{"x":36036.90346344427,"y":89.0},{"x":37186.999875655696,"y":85.0},{"x":38337.09628786712,"y":66.0},{"x":39487.19270007854,"y":61.0},{"x":40637.28911228997,"y":65.0},{"x":41787.38552450139,"y":72.0},{"x":42937.481936712815,"y":60.0},{"x":44087.57834892424,"y":58.0},{"x":45237.67476113566,"y":43.0},{"x":46387.771173347086,"y":60.0},{"x":47537.86758555851,"y":51.0},{"x":48687.96399776993,"y":50.0},{"x":49838.06040998136,"y":54.0},{"x":50988.15682219278,"y":45.0},{"x":52138.253234404205,"y":45.0},{"x":53288.34964661563,"y":42.0},{"x":54438.44605882705,"y":44.0},{"x":55588.542471038476,"y":37.0},{"x":56738.6388832499,"y":49.0},{"x":57888.73529546132,"y":26.0},{"x":59038.83170767275,"y":37.0},{"x":60188.92811988417,"y":37.0},{"x":61339.024532095595,"y":26.0},{"x":62489.12094430702,"y":30.0},{"x":63639.21735651844,"y":24.0},{"x":64789.313768729866,"y":21.0},{"x":65939.41018094128,"y":20.0},{"x":67089.5065931527,"y":17.0},{"x":68239.60300536413,"y":10.0},{"x":69389.69941757555,"y":7.0},{"x":70539.79582978698,"y":9.0},{"x":71689.8922419984,"y":12.0},{"x":72839.98865420982,"y":7.0},{"x":73990.08506642125,"y":4.0},{"x":75140.18147863267,"y":5.0},{"x":76290.2778908441,"y":2.0},{"x":77440.37430305552,"y":3.0},{"x":78590.47071526694,"y":1.0},{"x":79740.56712747837,"y":0.0},{"x":80890.66353968979,"y":6.0},{"x":82040.75995190121,"y":1.0},{"x":83190.85636411264,"y":0.0},{"x":84340.95277632406,"y":2.0},{"x":85491.04918853549,"y":2.0},{"x":86641.14560074691,"y":0.0},{"x":87791.24201295833,"y":2.0},{"x":88941.33842516976,"y":1.0},{"x":90091.43483738118,"y":2.0},{"x":91241.5312495926,"y":1.0},{"x":92391.62766180403,"y":1.0},{"x":93541.72407401545,"y":0.0},{"x":94691.82048622688,"y":0.0},{"x":95841.9168984383,"y":1.0},{"x":96992.01331064972,"y":0.0},{"x":98142.10972286115,"y":1.0},{"x":99292.20613507257,"y":0.0},{"x":100442.302547284,"y":0.0},{"x":101592.39895949542,"y":0.0},{"x":102742.49537170684,"y":0.0},{"x":103892.59178391827,"y":0.0},{"x":105042.68819612969,"y":1.0},{"x":106192.78460834111,"y":1.0},{"x":107342.88102055254,"y":0}]}],"marks":[{"type":"line","from":{"data":"e3366b69-ec2f-4bb8-913e-fc93029a8c14"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"#ff0000"},"fillOpacity":{"value":0.4},"stroke":{"value":"#ff0000"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}},{"type":"line","from":{"data":"2478bb71-e9d5-4266-9f21-ac04a5d724c5"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"#00ff00"},"fillOpacity":{"value":0.4},"stroke":{"value":"#00ff00"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}]},"value":"#gorilla_repl.vega.VegaView{:content {:width 400, :height 247.2188, :padding {:bottom 40, :top 10, :right 10, :left 55}, :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"e3366b69-ec2f-4bb8-913e-fc93029a8c14\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"e3366b69-ec2f-4bb8-913e-fc93029a8c14\", :field \"data.y\"}}], :axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :data ({:name \"e3366b69-ec2f-4bb8-913e-fc93029a8c14\", :values ({:x -12062.868845969851, :y 0} {:x -10638.213017945329, :y 4.0} {:x -9213.557189920806, :y 7.0} {:x -7788.901361896284, :y 23.0} {:x -6364.245533871761, :y 77.0} {:x -4939.589705847238, :y 179.0} {:x -3514.9338778227157, :y 385.0} {:x -2090.278049798193, :y 704.0} {:x -665.6222217736706, :y 1134.0} {:x 759.0336062508518, :y 1396.0} {:x 2183.689434275374, :y 1549.0} {:x 3608.3452622998966, :y 1384.0} {:x 5033.001090324419, :y 1074.0} {:x 6457.656918348942, :y 679.0} {:x 7882.3127463734645, :y 390.0} {:x 9306.968574397986, :y 182.0} {:x 10731.624402422509, :y 112.0} {:x 12156.280230447031, :y 71.0} {:x 13580.936058471554, :y 50.0} {:x 15005.591886496077, :y 36.0} {:x 16430.2477145206, :y 35.0} {:x 17854.903542545122, :y 16.0} {:x 19279.559370569645, :y 19.0} {:x 20704.215198594167, :y 35.0} {:x 22128.87102661869, :y 31.0} {:x 23553.526854643213, :y 23.0} {:x 24978.182682667735, :y 39.0} {:x 26402.838510692258, :y 24.0} {:x 27827.49433871678, :y 26.0} {:x 29252.150166741303, :y 32.0} {:x 30676.805994765826, :y 20.0} {:x 32101.46182279035, :y 25.0} {:x 33526.11765081487, :y 22.0} {:x 34950.773478839394, :y 28.0} {:x 36375.429306863916, :y 39.0} {:x 37800.08513488844, :y 42.0} {:x 39224.74096291296, :y 18.0} {:x 40649.396790937484, :y 11.0} {:x 42074.05261896201, :y 6.0} {:x 43498.70844698653, :y 7.0} {:x 44923.36427501105, :y 0.0} {:x 46348.020103035575, :y 2.0} {:x 47772.6759310601, :y 3.0} {:x 49197.33175908462, :y 3.0} {:x 50621.98758710914, :y 7.0} {:x 52046.643415133665, :y 2.0} {:x 53471.29924315819, :y 5.0} {:x 54895.95507118271, :y 1.0} {:x 56320.61089920723, :y 2.0} {:x 57745.266727231756, :y 5.0} {:x 59169.92255525628, :y 2.0} {:x 60594.5783832808, :y 1.0} {:x 62019.234211305324, :y 1.0} {:x 63443.890039329846, :y 1.0} {:x 64868.54586735437, :y 0.0} {:x 66293.20169537888, :y 2.0} {:x 67717.8575234034, :y 6.0} {:x 69142.51335142793, :y 0.0} {:x 70567.16917945245, :y 0.0} {:x 71991.82500747697, :y 1.0} {:x 73416.4808355015, :y 1.0} {:x 74841.13666352602, :y 0.0} {:x 76265.79249155054, :y 0.0} {:x 77690.44831957507, :y 1.0} {:x 79115.10414759959, :y 3.0} {:x 80539.75997562411, :y 3.0} {:x 81964.41580364863, :y 1.0} {:x 83389.07163167316, :y 0.0} {:x 84813.72745969768, :y 0.0} {:x 86238.3832877222, :y 1.0} {:x 87663.03911574672, :y 0.0} {:x 89087.69494377125, :y 1.0} {:x 90512.35077179577, :y 2.0} {:x 91937.00659982029, :y 1.0} {:x 93361.66242784481, :y 1.0} {:x 94786.31825586934, :y 1.0} {:x 96210.97408389386, :y 0.0} {:x 97635.62991191838, :y 0.0} {:x 99060.2857399429, :y 0.0} {:x 100484.94156796743, :y 2.0} {:x 101909.59739599195, :y 1.0} {:x 103334.25322401647, :y 0.0} {:x 104758.909052041, :y 0.0} {:x 106183.56488006552, :y 0.0} {:x 107608.22070809004, :y 1.0} {:x 109032.87653611456, :y 0.0} {:x 110457.53236413909, :y 0.0} {:x 111882.18819216361, :y 0.0} {:x 113306.84402018813, :y 0.0} {:x 114731.49984821265, :y 0.0} {:x 116156.15567623718, :y 0.0} {:x 117580.8115042617, :y 0.0} {:x 119005.46733228622, :y 1.0} {:x 120430.12316031074, :y 0.0} {:x 121854.77898833527, :y 0.0} {:x 123279.43481635979, :y 0.0} {:x 124704.09064438431, :y 0.0} {:x 126128.74647240883, :y 0.0} {:x 127553.40230043336, :y 0.0} {:x 128978.05812845788, :y 0.0} {:x 130402.7139564824, :y 1.0} {:x 131827.3697845069, :y 0})} {:name \"2478bb71-e9d5-4266-9f21-ac04a5d724c5\", :values ({:x -8816.856612801239, :y 0} {:x -7666.760200589816, :y 1.0} {:x -6516.663788378393, :y 0.0} {:x -5366.5673761669705, :y 3.0} {:x -4216.470963955548, :y 9.0} {:x -3066.374551744125, :y 15.0} {:x -1916.278139532702, :y 34.0} {:x -766.1817273212791, :y 61.0} {:x 383.9146848901437, :y 74.0} {:x 1534.0110971015665, :y 129.0} {:x 2684.1075093129894, :y 163.0} {:x 3834.203921524412, :y 207.0} {:x 4984.300333735835, :y 190.0} {:x 6134.396745947258, :y 168.0} {:x 7284.493158158681, :y 130.0} {:x 8434.589570370103, :y 82.0} {:x 9584.685982581526, :y 59.0} {:x 10734.78239479295, :y 36.0} {:x 11884.878807004374, :y 26.0} {:x 13034.975219215798, :y 26.0} {:x 14185.071631427221, :y 27.0} {:x 15335.168043638645, :y 54.0} {:x 16485.26445585007, :y 102.0} {:x 17635.360868061493, :y 248.0} {:x 18785.457280272916, :y 492.0} {:x 19935.55369248434, :y 883.0} {:x 21085.650104695764, :y 1250.0} {:x 22235.746516907187, :y 1261.0} {:x 23385.84292911861, :y 1037.0} {:x 24535.939341330035, :y 663.0} {:x 25686.03575354146, :y 328.0} {:x 26836.132165752882, :y 173.0} {:x 27986.228577964306, :y 108.0} {:x 29136.32499017573, :y 105.0} {:x 30286.421402387154, :y 94.0} {:x 31436.517814598577, :y 96.0} {:x 32586.61422681, :y 93.0} {:x 33736.710639021425, :y 96.0} {:x 34886.80705123285, :y 81.0} {:x 36036.90346344427, :y 89.0} {:x 37186.999875655696, :y 85.0} {:x 38337.09628786712, :y 66.0} {:x 39487.19270007854, :y 61.0} {:x 40637.28911228997, :y 65.0} {:x 41787.38552450139, :y 72.0} {:x 42937.481936712815, :y 60.0} {:x 44087.57834892424, :y 58.0} {:x 45237.67476113566, :y 43.0} {:x 46387.771173347086, :y 60.0} {:x 47537.86758555851, :y 51.0} {:x 48687.96399776993, :y 50.0} {:x 49838.06040998136, :y 54.0} {:x 50988.15682219278, :y 45.0} {:x 52138.253234404205, :y 45.0} {:x 53288.34964661563, :y 42.0} {:x 54438.44605882705, :y 44.0} {:x 55588.542471038476, :y 37.0} {:x 56738.6388832499, :y 49.0} {:x 57888.73529546132, :y 26.0} {:x 59038.83170767275, :y 37.0} {:x 60188.92811988417, :y 37.0} {:x 61339.024532095595, :y 26.0} {:x 62489.12094430702, :y 30.0} {:x 63639.21735651844, :y 24.0} {:x 64789.313768729866, :y 21.0} {:x 65939.41018094128, :y 20.0} {:x 67089.5065931527, :y 17.0} {:x 68239.60300536413, :y 10.0} {:x 69389.69941757555, :y 7.0} {:x 70539.79582978698, :y 9.0} {:x 71689.8922419984, :y 12.0} {:x 72839.98865420982, :y 7.0} {:x 73990.08506642125, :y 4.0} {:x 75140.18147863267, :y 5.0} {:x 76290.2778908441, :y 2.0} {:x 77440.37430305552, :y 3.0} {:x 78590.47071526694, :y 1.0} {:x 79740.56712747837, :y 0.0} {:x 80890.66353968979, :y 6.0} {:x 82040.75995190121, :y 1.0} {:x 83190.85636411264, :y 0.0} {:x 84340.95277632406, :y 2.0} {:x 85491.04918853549, :y 2.0} {:x 86641.14560074691, :y 0.0} {:x 87791.24201295833, :y 2.0} {:x 88941.33842516976, :y 1.0} {:x 90091.43483738118, :y 2.0} {:x 91241.5312495926, :y 1.0} {:x 92391.62766180403, :y 1.0} {:x 93541.72407401545, :y 0.0} {:x 94691.82048622688, :y 0.0} {:x 95841.9168984383, :y 1.0} {:x 96992.01331064972, :y 0.0} {:x 98142.10972286115, :y 1.0} {:x 99292.20613507257, :y 0.0} {:x 100442.302547284, :y 0.0} {:x 101592.39895949542, :y 0.0} {:x 102742.49537170684, :y 0.0} {:x 103892.59178391827, :y 0.0} {:x 105042.68819612969, :y 1.0} {:x 106192.78460834111, :y 1.0} {:x 107342.88102055254, :y 0})}), :marks ({:type \"line\", :from {:data \"e3366b69-ec2f-4bb8-913e-fc93029a8c14\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"#ff0000\"}, :fillOpacity {:value 0.4}, :stroke {:value \"#ff0000\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}} {:type \"line\", :from {:data \"2478bb71-e9d5-4266-9f21-ac04a5d724c5\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"#00ff00\"}, :fillOpacity {:value 0.4}, :stroke {:value \"#00ff00\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}})}}"}
;; <=

;; @@

;; @@
