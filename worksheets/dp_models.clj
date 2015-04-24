;; gorilla-repl.fileformat = 1

;; **
;;; # Coding Dirichlet Process Models Via Probabilistic Programming
;; **

;; @@
(ns dp-models
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
;;; This short tutorial is provided in the form of questions and answers.
;;; 
;;; ## Introduction
;;; 
;;; Dirichlet process mixtures, reviewed in depth in \[[Teh, 2010](http://www.gatsby.ucl.ac.uk/~ywteh/research/npbayes/dp.pdf), [Orbanz and Teh, 2010](http://www.stats.ox.ac.uk/~teh/research/npbayes/OrbTeh2010a.pdf)\] and the subject of excellent tutorial presentations by [\[Teh, 2007\]](http://www.stats.ox.ac.uk/~teh/npbayes.html#dp), are widely used in Bayesian unsupervised clustering and density estimation tasks.  In particular the infinite Gaussian mixture model [\[Rasmussen, 1999\]](https://www.seas.harvard.edu/courses/cs281/papers/rasmussen-1999a.pdf) has been widely used.  A canonical example application is neural spike sorting [\[Wood and Black, 2008\]](http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3880746/) (this latter applied work also highlights efficient sequential inference).  
;;; 
;;; Stick-breaking constructions [\[Ishwaran and James, 2001\]](http://www.cis.upenn.edu/~taskar/courses/cis700-sp08/papers/stickBreaking.pdf) make coding some Bayesian nonparametric primitives in probabilistic programming systems relatively straightforward.  Additionally there is an interesting and deep (in not fully or even well described in the literature) connection between the action of  Dirichlet-like stochastic processes and relaxations of the programming languages technique called memoization [\[Michie, 1968\]](http://www.nature.com/nature/journal/v218/n5136/abs/218019a0.html).  The latter, simply put, is the idea of wrapping a function in a hashmap so that it remembers and thus never needs to recompute a return value if called again with the same arguments.  Memoization can sometimes give rise to very simple dynamic programming algorithms.
;; **

;; **
;;; ## Question \#1
;;; 
;;; Generalize the DPmem code below to default to duplicate the functionality of `mem` if the concentration is @@0@@.
;;; 
;;; Read the following code very carefully as it generalizes the Dirichlet process in a way that is natural in probabilistic programming – namely the base distribution of the Dirichlet process is a procedure.  In probabilistic programming applying a procedure produces a _sample_ so it is possible to use any procedure as the base distribution in any Dirichlet process.   What is more, the calling interface to the Dirichlet process is the same as `mem`, i.e. it is a function that takes a function and returns a function that calls the inner function when certain conditions are met (like in `mem`, for instance, if there doesn't already exist a return value for the specific provided arguments; or, like in a DP-based model, if a sample is generated from the base distribution rather than simply returning  one of the already generated base-distribution samples).  The original Church paper [\[Goodman, Mansinghka, et al., 2008\]](http://arxiv.org/pdf/1206.3255.pdf) introduced this idea and called it stochastic memoization, a powerful realisation that the Dirichlet process and its ilk provide a stochastic generalisation of `mem`.  These ideas and their connection to deeper ideas about computability have also been discussed in a short workshop paper [\[Roy, 2008\]](http://danroy.org/papers/RoyManGooTen-ICMLNPB-2008.pdf).
;; **

;; @@
(defm create-stick-breaking-process
  [c d]
  (let
    [
      ; V returns a procedure that picks
      ; a stick each time its called from the set of sticks lazily constructed
      ; via the closed-over two-parameter stick breaking rule
      V (mem (fn [k] (sample (beta (- 1 d) (+ c (* k d))))))

      ; sample-stick-index is a procedure that samples an index from
      ; a potentially infinite dimensional discrete distribution 
      ; lazily constructed by a stick breaking rule
      sample-stick-index (fn sample-stick-index [k] 
                           (if (sample (flip (V k))) 
                             k 
                             (sample-stick-index (+ k 1))))

      ; wrapping a stick breaking process to return it
      stick-breaking-process (fn [] (sample-stick-index 1))]
    stick-breaking-process))

(defquery dp-example-1
  []
  (let
    [ 
      ; DPmem is a procedure that takes two arguments -- the concentration
      ; to a Dirichlet process and a base sampling procedure
      ; DPmem returns a procedure 
      DPmem (fn [concentration base]
              (let [get-value-from-cache-or-sample (mem (fn [args stick-index] 
                                                          (apply base args)))
                    get-stick-picking-procedure-from-cache (mem (fn [args] 
                                                                  (create-stick-breaking-process concentration 0.0)))]
                (fn [& varargs]
                  ; when the returned function is called, the first thing it does is get
                  ; the cached stick breaking procedure for the passed in arguments
                  ; and _calls_ it to get an index
                  (let [index ((get-stick-picking-procedure-from-cache varargs))]
                    ; if, for the given set of arguments and just sampled index
                    ; a return value has already been computed, get it from the cache
                    ; and return it, otherwise sample a new value
                    (get-value-from-cache-or-sample varargs index)))))

      some-base-distribution
      (fn [] (sample (normal 0 1)))

      dp-process-instance (DPmem 1.0 some-base-distribution)]

    ; predict a data point from the DP mixture
    (predict 'prediction
             (repeatedly 10000 (fn [] (dp-process-instance))))))

(def samples
  (first
    (map get-predicts
         (doquery :pgibbs
                  dp-example-1
                  []
                  :number-of-particles 2))))

(def predictions (get samples 'prediction))

(plot/histogram predictions :bins 30)
;; @@
;; =>
;;; {"type":"vega","content":{"axes":[{"scale":"x","type":"x"},{"scale":"y","type":"y"}],"scales":[{"name":"x","type":"linear","range":"width","zero":false,"domain":{"data":"ab195ef0-49eb-445f-a56f-e07e00fb9702","field":"data.x"}},{"name":"y","type":"linear","range":"height","nice":true,"zero":false,"domain":{"data":"ab195ef0-49eb-445f-a56f-e07e00fb9702","field":"data.y"}}],"marks":[{"type":"line","from":{"data":"ab195ef0-49eb-445f-a56f-e07e00fb9702"},"properties":{"enter":{"x":{"scale":"x","field":"data.x"},"y":{"scale":"y","field":"data.y"},"interpolate":{"value":"step-before"},"fill":{"value":"steelblue"},"fillOpacity":{"value":0.4},"stroke":{"value":"steelblue"},"strokeWidth":{"value":2},"strokeOpacity":{"value":1}}}}],"data":[{"name":"ab195ef0-49eb-445f-a56f-e07e00fb9702","values":[{"x":-2.3815225477467568,"y":0},{"x":-2.234245680632194,"y":187.0},{"x":-2.0869688135176316,"y":0.0},{"x":-1.939691946403069,"y":451.0},{"x":-1.7924150792885065,"y":0.0},{"x":-1.645138212173944,"y":0.0},{"x":-1.4978613450593814,"y":0.0},{"x":-1.3505844779448188,"y":0.0},{"x":-1.2033076108302563,"y":0.0},{"x":-1.0560307437156937,"y":0.0},{"x":-0.908753876601131,"y":0.0},{"x":-0.7614770094865684,"y":1217.0},{"x":-0.6142001423720057,"y":0.0},{"x":-0.46692327525744304,"y":0.0},{"x":-0.31964640814288037,"y":166.0},{"x":-0.17236954102831772,"y":0.0},{"x":-0.02509267391375508,"y":2.0},{"x":0.12218419320080756,"y":1.0},{"x":0.2694610603153702,"y":702.0},{"x":0.4167379274299329,"y":0.0},{"x":0.5640147945444955,"y":0.0},{"x":0.7112916616590582,"y":43.0},{"x":0.8585685287736209,"y":1311.0},{"x":1.0058453958881834,"y":0.0},{"x":1.153122263002746,"y":0.0},{"x":1.3003991301173086,"y":3.0},{"x":1.4476759972318711,"y":2574.0},{"x":1.5949528643464337,"y":0.0},{"x":1.7422297314609962,"y":31.0},{"x":1.8895065985755588,"y":0.0},{"x":2.0367834656901214,"y":0.0},{"x":2.184060332804684,"y":3312.0},{"x":2.3313371999192465,"y":0}]}],"width":400,"height":247.2187957763672,"padding":{"bottom":20,"top":10,"right":10,"left":50}},"value":"#gorilla_repl.vega.VegaView{:content {:axes [{:scale \"x\", :type \"x\"} {:scale \"y\", :type \"y\"}], :scales [{:name \"x\", :type \"linear\", :range \"width\", :zero false, :domain {:data \"ab195ef0-49eb-445f-a56f-e07e00fb9702\", :field \"data.x\"}} {:name \"y\", :type \"linear\", :range \"height\", :nice true, :zero false, :domain {:data \"ab195ef0-49eb-445f-a56f-e07e00fb9702\", :field \"data.y\"}}], :marks [{:type \"line\", :from {:data \"ab195ef0-49eb-445f-a56f-e07e00fb9702\"}, :properties {:enter {:x {:scale \"x\", :field \"data.x\"}, :y {:scale \"y\", :field \"data.y\"}, :interpolate {:value \"step-before\"}, :fill {:value \"steelblue\"}, :fillOpacity {:value 0.4}, :stroke {:value \"steelblue\"}, :strokeWidth {:value 2}, :strokeOpacity {:value 1}}}}], :data [{:name \"ab195ef0-49eb-445f-a56f-e07e00fb9702\", :values ({:x -2.3815225477467568, :y 0} {:x -2.234245680632194, :y 187.0} {:x -2.0869688135176316, :y 0.0} {:x -1.939691946403069, :y 451.0} {:x -1.7924150792885065, :y 0.0} {:x -1.645138212173944, :y 0.0} {:x -1.4978613450593814, :y 0.0} {:x -1.3505844779448188, :y 0.0} {:x -1.2033076108302563, :y 0.0} {:x -1.0560307437156937, :y 0.0} {:x -0.908753876601131, :y 0.0} {:x -0.7614770094865684, :y 1217.0} {:x -0.6142001423720057, :y 0.0} {:x -0.46692327525744304, :y 0.0} {:x -0.31964640814288037, :y 166.0} {:x -0.17236954102831772, :y 0.0} {:x -0.02509267391375508, :y 2.0} {:x 0.12218419320080756, :y 1.0} {:x 0.2694610603153702, :y 702.0} {:x 0.4167379274299329, :y 0.0} {:x 0.5640147945444955, :y 0.0} {:x 0.7112916616590582, :y 43.0} {:x 0.8585685287736209, :y 1311.0} {:x 1.0058453958881834, :y 0.0} {:x 1.153122263002746, :y 0.0} {:x 1.3003991301173086, :y 3.0} {:x 1.4476759972318711, :y 2574.0} {:x 1.5949528643464337, :y 0.0} {:x 1.7422297314609962, :y 31.0} {:x 1.8895065985755588, :y 0.0} {:x 2.0367834656901214, :y 0.0} {:x 2.184060332804684, :y 3312.0} {:x 2.3313371999192465, :y 0})}], :width 400, :height 247.2188, :padding {:bottom 20, :top 10, :right 10, :left 50}}}"}
;; <=

;; **
;;;  An answer can be obtained by adding a condition (`if` statement) to the procedure `sample-stick-index`.
;;;  We should deterministically return a stick index @@1@@ if the concentration parameter is equal to zero.
;;;  Procedures {\tt sample-stick-index} and {\tt DPmem} remain untouched.
;; **

;; @@
(sample (flip (sample (beta 1 0))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>true</span>","value":"true"}
;; <=

;; @@

;; @@
