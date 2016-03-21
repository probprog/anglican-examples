;; gorilla-repl.fileformat = 1

;; **
;;; # Decision Making Under Uncertainty
;;; 
;;; Model-based decision making involves building models/simulators of the world, observing features of the world, assigning a utility value to every action in every state of the world, then rationally selecting an action based on maximizing the expected utility under updated beliefs about the state of the world given the observations that have been made.  Multi-stage Bayesian decisions problems can be expressed using a graphical notation known as decision or influence diagrams (Murphy, 2012). These diagrams extend traditional graphical models with the addition of decision or action nodes, represented by rectangles, and utility nodes represented by diamonds.  Encoding such decision problems as Anglican programs is straightforward as we will show now.
;;; 
;;; # Decision Diagram Notation
;;; 
;;; Traditional multi-stage Bayesian decisions problems can be represented in a graphical notation known as decision or influence diagrams (Murphy, 2012). These diagrams extend traditional graphical models with the addition of decision or action nodes, represented by rectangles, and utility nodes represented by diamonds.
;;; 
;;; <div style="text-align: center">
;;; <p>
;;; <img src="http://www.robots.ox.ac.uk/~fwood/interpreted-anglican/examples/influence_diagram/influence_diagram_website.png" alt="Decision diagram" style="width: 450px;"/>
;;; </p>
;;; <p>
;;; <strong>Figure 1:</strong> Decision diagrams a) oil wildcatter drill or don't-drill decision-making problem with no observations. <br> b) Decision diagram with noisy sounding observation included.<br> c) A further extension where we decide also whether or not to spend the money to perform the test to gather the observation (Murphy, 2012).
;;; </p>
;;; </div>
;;; 
;;; ## Example
;;; 
;;; The oil wildcatter problem is a well-known  decision making problem that can be expressed and solved using influence diagrams [(Raiffa, 1968)](http://facweb.knowlton.ohio-state.edu/pviton/courses/crp6600/raiffa_Chs_012.pdf) and [(Shenoy, 1992)](https://kuscholarworks.ku.edu/dspace/bitstream/1808/183/1/OR92.pdf). This example also appears in (Murphy, 2012) which is where we have derived this from.
;;; 
;;; In this problem an oil wildcatter must decide either to drill or not to drill. He is uncertain whether the well will be dry, wet or soaking in oil. The wildcatter may also make seismic soundings that will help determine the nature of the well.  Figure 1 demonstrates shows an evolution of the oil wildcatter decision-making problems  that starts with no measurement then adds a sounding, and finally terminates with including the cost of conducting the test.
;;; 
;;; As usual we will start with boilerplace inclusions.
;; **

;; @@
(ns drill-or-not
  (:require [clojure.core.matrix :as m
             :refer [mul mmul add sub div]]
            [anglican importance smc lmh pimh]
            [anglican.stat :as stat]
            [gorilla-plot.core :as plot])
  (:use [anglican core emit runtime
         [inference :refer [infer equalize]]
         [state :refer [get-predicts get-log-weight]]]))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Consider first the decision diagram in Figure 1a); the wildcatter must decide to drill @@d=1@@ (:drill) or not to drill @@d=0@@ (:dontdrill). Assume that are three states of nature: a dry well @@o=0@@ (:dry), a wet well @@o=1@@ (:wet) and a soaking well @@o=2@@ (:soaking). Also assume that we have the following prior beliefs about these states of nature 
;;; $$ p(o) = [0.5,0.3,0.2].$$ 
;;; 
;;; To make our decision we will also have to assign a utility associated to each action taken given every possible state of the world.  For instance we might decide that the utility assigned to deciding to drill and hitting a dry well is @@-\$70@@ (we lose the money we spent to drill the well), a wet well is @@\$50@@ (we make some money), and a soaking well is @@\$200@@ (winner!). The utility of not drilling at all is assigned value zero.
;;; 
;;; These utility function is exactly that, a function from action and world (well) state to utility value:  (here this is a defm because we're going to use it _within_ an Anglican program, this is an Anglican language function _not_ Clojure)
;; **

;; @@
(defm utilitym [action well-state]
  (if (= action :drill) 
    (get  {:dry -70 
           :wet 50 
           :soaking 200} well-state)
    (get  {:dry 0   
           :wet 0  
           :soaking 0}   well-state)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;drill-or-not/utilitym</span>","value":"#'drill-or-not/utilitym"}
;; <=

;; **
;;; Given the generative model of the world and our utility function we can now write a query to compute the conditional expectation of the utility of all actions
;; **

;; @@
(defquery drill-or-not-no-sounding 
  [utility-fn] 
  (let [well-state-prior (categorical {:dry 0.5 
                                       :wet 0.3 
                                       :soaking 0.2})
        well-state (sample well-state-prior)
        
        utilities {:drill     (utility-fn :drill     well-state) 
                   :dontdrill (utility-fn :dontdrill well-state)}]
    (predict :utilities utilities)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;drill-or-not/drill-or-not-no-sounding</span>","value":"#'drill-or-not/drill-or-not-no-sounding"}
;; <=

;; **
;;; If we use a rational policy, that is a policy that says to perform the action that has the highest expected utility, then what remains to do is compute the expected utilities of all actions and choose the action which maximized expected utility.  We first write a couple functions to compute the expected utilities of all actions
;; **

;; @@
(defn expected-utility [samples] 
  (let [N (count samples)]
  (loop [sum-dontdrill 0
         sum-drill 0
         samps samples]
    (if (empty? samps)
      {:drill     (float (/ sum-drill     N)) 
       :dontdrill (float (/ sum-dontdrill N))}
      (let [current-sample (first samps)]
        (recur (+ sum-dontdrill (:dontdrill current-sample))
               (+ sum-drill     (:drill     current-sample))
               (rest samps)))))))

(defn draw-samples 
  [query N & args] 
  (->> (repeatedly N 
                   #(sample ((conditional query :lmh) utilitym args)))
                           (map :utilities)))  
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;drill-or-not/draw-samples</span>","value":"#'drill-or-not/draw-samples"}
;; <=

;; **
;;; Using these we can approximate the expected utilities 
;; **

;; @@
(expected-utility (draw-samples drill-or-not-no-sounding 5000))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:drill</span>","value":":drill"},{"type":"html","content":"<span class='clj-unkown'>22.052</span>","value":"22.052"}],"value":"[:drill 22.052]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontdrill</span>","value":":dontdrill"},{"type":"html","content":"<span class='clj-unkown'>0.0</span>","value":"0.0"}],"value":"[:dontdrill 0.0]"}],"value":"{:drill 22.052, :dontdrill 0.0}"}
;; <=

;; **
;;; There is no inference in this example, it's merely a simulation of the world and a computation of the expected utility under uncertainty about the world.  If we are able to make measurements relating to the state of the world then we should update our belief about the state of the world by performing posterior inference.  
;;; 
;;; Making such an observation is be found in figure 1b). Suppose now that the wildcatter performs a sounding to estimate the state of the well in advance of drilling. The soundings will give a closed reflection pattern (indication for a lot of oil, @@s=2@@ (:closed), an open pattern (indication for some oil, @@s=1@@ (:open)), or a diffuse pattern  (almost no hope for oil, @@s=0@@ (:diffuse)). As can be seen in figure 1b), an information arc has been added from Sound to Drill, indicating that the sounding information is available before the wildcatter decides to drill or not. The reliability of the sensor is measured using a conditional distribution table for @@p(s|o)@@.
;;; 
;;; Suppose that the wildcatter observes an open reflection pattern @@s=0@@ (:open) from the sounding, indicating that it is unlikely that there is black gold beneath.  The computation that has to happen is the computation of the posterior distribution over worlds and the conditional expectation of the utility of each action.  This code does this via the _observe_ directive which informs Anglican to compute the posterior distribution of the utilities given that a diffuse observation was made.
;; **

;; @@
(defquery drill-or-not-with-sounding-no-arg 
  [utility-fn] 
  (let [well-state-prior (categorical {:dry 0.5 
                                       :wet 0.3 
                                       :soaking 0.2})
        well-state (sample well-state-prior)
        sounding-dist (cond
                        (= well-state :dry)
                          (categorical {:diffuse 0.6 
                                        :open 0.3 
                                        :closed 0.1})
                        (= well-state :wet)
                          (categorical {:diffuse 0.3 
                                        :open 0.4 
                                        :closed 0.3})
                        (= well-state :soaking)
                          (categorical {:diffuse 0.1 
                                        :open 0.4 
                                        :closed 0.5}))
        				utilities {:drill     (utility-fn :drill 
                                                          well-state) 
                                   :dontdrill (utility-fn :dontdrill 
                                                          well-state)}]
    (observe sounding-dist :diffuse)
    (predict :utilities utilities)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;drill-or-not/drill-or-not-with-sounding-no-arg</span>","value":"#'drill-or-not/drill-or-not-with-sounding-no-arg"}
;; <=

;; @@
(expected-utility (draw-samples drill-or-not-with-sounding-no-arg 5000))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:drill</span>","value":":drill"},{"type":"html","content":"<span class='clj-unkown'>-26.176</span>","value":"-26.176"}],"value":"[:drill -26.176]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontdrill</span>","value":":dontdrill"},{"type":"html","content":"<span class='clj-unkown'>0.0</span>","value":"0.0"}],"value":"[:dontdrill 0.0]"}],"value":"{:drill -26.176, :dontdrill 0.0}"}
;; <=

;; **
;;; This suggests that if we get a :diffuse reading we should _not_ drill the well.  
;;; 
;;; Of course we would like to be able to use the same query code to compute the expected utilities for all actions, to do this we can add a parameter to the query, noting that draw-samples was written to pass extra args to the program.
;; **

;; @@
(defquery drill-or-not-with-sounding [utility-fn sounding] 
  (let [well-state-prior (categorical {:dry 0.5 
                                       :wet 0.3 
                                       :soaking 0.2})
        well-state (sample well-state-prior)
        sounding-dist (cond
                        (= well-state :dry)
                          (categorical {:diffuse 0.6 
                                        :open 0.3 
                                        :closed 0.1})
                        (= well-state :wet)
                          (categorical {:diffuse 0.3 
                                        :open 0.4 
                                        :closed 0.3})
                        (= well-state :soaking)
                          (categorical {:diffuse 0.1 
                                        :open 0.4 
                                        :closed 0.5}))
        				utilities {:drill     (utility-fn :drill 
                                                          well-state) 
                                   :dontdrill (utility-fn :dontdrill 
                                                          well-state)}]
    (observe sounding-dist (first sounding))
    (predict :utilities utilities)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;drill-or-not/drill-or-not-with-sounding</span>","value":"#'drill-or-not/drill-or-not-with-sounding"}
;; <=

;; **
;;; A quick check to make sure that we get approximately the same value
;; **

;; @@
 (expected-utility  (draw-samples  drill-or-not-with-sounding 5000 :diffuse))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:drill</span>","value":":drill"},{"type":"html","content":"<span class='clj-unkown'>-27.766</span>","value":"-27.766"}],"value":"[:drill -27.766]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontdrill</span>","value":":dontdrill"},{"type":"html","content":"<span class='clj-unkown'>0.0</span>","value":"0.0"}],"value":"[:dontdrill 0.0]"}],"value":"{:drill -27.766, :dontdrill 0.0}"}
;; <=

;; **
;;; Using this parameterized version of the query, just a conditional distribution, conditioned on the arguments, we can compute the conditional expected utilities for all actions under each observation condition.  The optimal policy for each observation condition is then simply the choice of action that maximizes the expected utility one for each observation case
;; **

;; @@
(map #(expected-utility (draw-samples drill-or-not-with-sounding 5000 %)) [:diffuse :open :closed])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:drill</span>","value":":drill"},{"type":"html","content":"<span class='clj-unkown'>-25.264</span>","value":"-25.264"}],"value":"[:drill -25.264]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontdrill</span>","value":":dontdrill"},{"type":"html","content":"<span class='clj-unkown'>0.0</span>","value":"0.0"}],"value":"[:dontdrill 0.0]"}],"value":"{:drill -25.264, :dontdrill 0.0}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:drill</span>","value":":drill"},{"type":"html","content":"<span class='clj-unkown'>33.068</span>","value":"33.068"}],"value":"[:drill 33.068]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontdrill</span>","value":":dontdrill"},{"type":"html","content":"<span class='clj-unkown'>0.0</span>","value":"0.0"}],"value":"[:dontdrill 0.0]"}],"value":"{:drill 33.068, :dontdrill 0.0}"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:drill</span>","value":":drill"},{"type":"html","content":"<span class='clj-unkown'>71.684</span>","value":"71.684"}],"value":"[:drill 71.684]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontdrill</span>","value":":dontdrill"},{"type":"html","content":"<span class='clj-unkown'>0.0</span>","value":"0.0"}],"value":"[:dontdrill 0.0]"}],"value":"{:drill 71.684, :dontdrill 0.0}"}],"value":"({:drill -25.264, :dontdrill 0.0} {:drill 33.068, :dontdrill 0.0} {:drill 71.684, :dontdrill 0.0})"}
;; <=

;; **
;;; To make clear the separation of inference from the computation of an optimal rational policy via expected utility let's write this is a different way where the modeling language, Anglican, is used solely for posterior inference and the expected utilities are computed outside the inference problem specification and computation.
;;; 
;;; Now our well-state query will only compute the posterior of the well state given the passed in sounding.
;; **

;; @@
(defquery well-state [sounding] 
  (let [well-state-prior (categorical {:dry 0.5 
                                       :wet 0.3 
                                       :soaking 0.2})
        well-state (sample well-state-prior)
        sounding-dist (cond
                        (= well-state :dry)
                          (categorical {:diffuse 0.6 
                                        :open 0.3 
                                        :closed 0.1})
                        (= well-state :wet)
                          (categorical {:diffuse 0.3 
                                        :open 0.4 
                                        :closed 0.3})
                        (= well-state :soaking)
                          (categorical {:diffuse 0.1 
                                        :open 0.4 
                                        :closed 0.5}))]
    (observe sounding-dist sounding)
    (predict :well-state well-state)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;drill-or-not/well-state</span>","value":"#'drill-or-not/well-state"}
;; <=

;; **
;;; Now we can specify the utility of each action well-state pair.  This function now lives outside Anglican and is a regular Clojure function.
;; **

;; @@
(defn utility 
  [action well-state]
  (if (= action :drill) 
    (get  {:dry -70 
           :wet 50 
           :soaking 200} 
          well-state)
    (get  {:dry 0   
           :wet 0  
           :soaking 0}   
          well-state)))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;drill-or-not/utility</span>","value":"#'drill-or-not/utility"}
;; <=

;; **
;;; The posterior estimate will now, too, be computed differently.  Instead of using the simplifying "conditional" primitive we will here invoke the sampler explicitly.  Among other things coding it this way results in faster code and better convergence because the posterior samples include weights which are used to compute more efficient expectations.  The following block contains helper functions and test calls for gathering a posterior estimate, computing the expected utility of a set of actions given an observation, and so forth.
;; **

;; @@
(defn posterior-estimate [N observation] 
  (->> (doquery :pimh 
                well-state 
                [observation] 
                :number-of-particles 
                100)
     (take N)
     (map #(vector 
             (:well-state (get-predicts %)) 
             (:log-weight %)))
     (stat/empirical-distribution)))
(def N 5000)
(posterior-estimate N :diffuse)

(defn expected-utility-of-action [posterior action]
  (reduce + (map #(* (utility2 
                       action 
                       (key %)) 
                     (val %)) 
                 posterior)))
(expected-utility-of-action (posterior-estimate N :diffuse) :drill)

(defn expected-utility [N actions observation]
  (let [posterior (posterior-estimate N observation)]
  (vector observation (apply 
                        assoc 
                        {}
                        (interleave 
                          actions 
                          (map 
                            (partial 
                              expected-utility-of-action 
                              posterior)
                            actions))))))

(expected-utility N [:drill :dontdrill] :diffuse )

(map (partial expected-utility N [:drill :dontdrill]) [:diffuse :open :closed])
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:diffuse</span>","value":":diffuse"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontdrill</span>","value":":dontdrill"},{"type":"html","content":"<span class='clj-double'>0.0</span>","value":"0.0"}],"value":"[:dontdrill 0.0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:drill</span>","value":":drill"},{"type":"html","content":"<span class='clj-double'>-30.17199999999664</span>","value":"-30.17199999999664"}],"value":"[:drill -30.17199999999664]"}],"value":"{:dontdrill 0.0, :drill -30.17199999999664}"}],"value":"[:diffuse {:dontdrill 0.0, :drill -30.17199999999664}]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:open</span>","value":":open"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontdrill</span>","value":":dontdrill"},{"type":"html","content":"<span class='clj-double'>0.0</span>","value":"0.0"}],"value":"[:dontdrill 0.0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:drill</span>","value":":drill"},{"type":"html","content":"<span class='clj-double'>30.79400000000064</span>","value":"30.79400000000064"}],"value":"[:drill 30.79400000000064]"}],"value":"{:dontdrill 0.0, :drill 30.79400000000064}"}],"value":"[:open {:dontdrill 0.0, :drill 30.79400000000064}]"},{"type":"list-like","open":"<span class='clj-vector'>[</span>","close":"<span class='clj-vector'>]</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:closed</span>","value":":closed"},{"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:dontdrill</span>","value":":dontdrill"},{"type":"html","content":"<span class='clj-double'>0.0</span>","value":"0.0"}],"value":"[:dontdrill 0.0]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:drill</span>","value":":drill"},{"type":"html","content":"<span class='clj-double'>85.23799999999572</span>","value":"85.23799999999572"}],"value":"[:drill 85.23799999999572]"}],"value":"{:dontdrill 0.0, :drill 85.23799999999572}"}],"value":"[:closed {:dontdrill 0.0, :drill 85.23799999999572}]"}],"value":"([:diffuse {:dontdrill 0.0, :drill -30.17199999999664}] [:open {:dontdrill 0.0, :drill 30.79400000000064}] [:closed {:dontdrill 0.0, :drill 85.23799999999572}])"}
;; <=

;; **
;;; With these functions we can compute the optimal rational policy, i.e. what action to take given every possible observation.
;; **

;; @@
(defn make-rational-policy 
  [actions 
   possible-observations 
   expected-utility-estimator]
  (let [expected-utilities (map (partial 
                                  expected-utility-estimator 
                                  actions) 
                                possible-observations)]
    (apply 
      assoc 
      {} 
      (interleave 
        (map first expected-utilities)
        (map #(key (apply max-key val %)) 
             (map second expected-utilities))))))

(make-rational-policy [:drill :dontdrill] 
                      [:closed :diffuse :open] 
                      expected-utility)
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:open</span>","value":":open"},{"type":"html","content":"<span class='clj-keyword'>:drill</span>","value":":drill"}],"value":"[:open :drill]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:diffuse</span>","value":":diffuse"},{"type":"html","content":"<span class='clj-keyword'>:dontdrill</span>","value":":dontdrill"}],"value":"[:diffuse :dontdrill]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:closed</span>","value":":closed"},{"type":"html","content":"<span class='clj-keyword'>:drill</span>","value":":drill"}],"value":"[:closed :drill]"}],"value":"{:open :drill, :diffuse :dontdrill, :closed :drill}"}
;; <=

;; **
;;; And in this case if the sounding returns a either an open or closed measurement we should drill otherwise, not.
;; **
