;; gorilla-repl.fileformat = 1

;; **
;;; # Schelling coordination game
;;; 
;;; In this example, we will explore using mutually recursive inference to model theory of mind.
;;; 
;;; Authors:
;;; 
;;;  - Brooks Paige [brooks@robots.ox.ac.uk](mailto:brooks@robots.ox.ac.uk)
;;;  - Frank Wood [fwood@robots.ox.ac.uk](mailto:fwood@robots.ox.ac.uk)
;;;  
;;; This worksheet is based on an exercise from a [practical](https://bitbucket.org/probprog/mlss2015/) at MLSS 2015 in Tübingen.
;; **

;; @@
(ns coordination-game
  (:require [clojure.core.matrix :as m
             :refer [mul mmul add sub div]]
            [anglican importance smc lmh]
            [anglican.stat :as stat])
  (:use [anglican core emit runtime
         [inference :refer [infer equalize]]
         [state :refer [get-predicts]]]))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; Amy and Bob want to meet up -- but they have no way of communicating ahead of time whether to go to the local pub, or to the Starbucks. They both mildly would prefer the pub, selecting it with probability 0.6.
;;; 
;;; * Where do they go?
;;; * Do they meet up?
;;; 
;;; This is an example of a [coordination game](https://en.wikipedia.org/wiki/Coordination_game).
;;; This particular formulation follows two example models at [forestdb.org](http://forestdb.org).
;;; 
;;; 
;;; We'll also use this example to explore additional Anglican language features.
;;; The distribution objects included in Anglican (for example `normal` and `flip`), are written in Clojure and implement the methods `sample` and `observe`.
;;; A call to `sample` should draw a random variate, and a call to `observe` should return a log-probability.
;;; 
;;; There's a macro shortcut `defdist` which can be used to define additional distributions.
;;; For example, we can create a distribution object in Anglican representing the prior distribution over their location preferences, which takes a `pub-preference` parameter between 0 and 1.
;; **

;; @@
(defdist location [pub-preference] []
  (sample* [this] (if (sample* (flip pub-preference)) :pub :starbucks))
  (observe* [this value] (observe* (flip pub-preference) (= value :pub))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-unkown'>#multifn[print-method 0x6a9f7e46]</span>","value":"#multifn[print-method 0x6a9f7e46]"}
;; <=

;; **
;;; We can then sample from this distribution just as any of the Anglican builtins:
;; **

;; @@
(repeatedly 10 #(sample* (location 0.6)))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-lazy-seq'>(</span>","close":"<span class='clj-lazy-seq'>)</span>","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:pub</span>","value":":pub"},{"type":"html","content":"<span class='clj-keyword'>:starbucks</span>","value":":starbucks"},{"type":"html","content":"<span class='clj-keyword'>:starbucks</span>","value":":starbucks"},{"type":"html","content":"<span class='clj-keyword'>:pub</span>","value":":pub"},{"type":"html","content":"<span class='clj-keyword'>:pub</span>","value":":pub"},{"type":"html","content":"<span class='clj-keyword'>:pub</span>","value":":pub"},{"type":"html","content":"<span class='clj-keyword'>:pub</span>","value":":pub"},{"type":"html","content":"<span class='clj-keyword'>:starbucks</span>","value":":starbucks"},{"type":"html","content":"<span class='clj-keyword'>:pub</span>","value":":pub"},{"type":"html","content":"<span class='clj-keyword'>:starbucks</span>","value":":starbucks"}],"value":"(:pub :starbucks :starbucks :pub :pub :pub :pub :starbucks :pub :starbucks)"}
;; <=

;; **
;;; If they both choose at random, where do they end up? How often do they meet each other?
;;; 
;;; We can write this as an Anglican query. The new distribution constructor `location` must be included as a primitive procedure:
;; **

;; @@
(with-primitive-procedures [location]
  (defquery meet-by-chance []
    (let [amy-location (sample (location 0.6))
          bob-location (sample (location 0.6))]
      {:amy amy-location
       :meet (= amy-location bob-location)})))

(println "p(Amy at pub) ="
  (stat/mean (map #(if (= (:amy %) :pub) 1.0 0.0)
                (repeatedly 1000 #(sample* ((conditional meet-by-chance)))))))

(println "p(Both at same location) ="
  (stat/mean (map #(if (:meet %) 1.0 0.0) 
                (repeatedly 1000 #(sample* ((conditional meet-by-chance)))))))
;; @@
;; ->
;;; p(Amy at pub) = 0.592
;;; p(Both at same location) = 0.525
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## Rejection sampling and hard constraints
;;; 
;;; What if we wanted to consider the posterior probability of the two being at the pub (the best possible outcome for both of them), conditioned on them successfully meeting up?
;;; 
;;; One way of writing this query would involve effectively adding a hard constraint.
;;; Adding hard constraints makes inference difficult, as we then effectively are performing rejection sampling;
;;; this is not recommended!
;;; 
;;; We can add a hard constraint by `(observe (flip 1.0) value)`, which has log-probability 0 when `value` is `true`, and log-probability `-Infinity` when `value` is `false`.
;;; 
;;; Here's an inefficient way of writing this query:
;;; 
;; **

;; @@
(with-primitive-procedures [location]
  (defquery meet-at-pub-inefficient []
    (let [amy-location (sample (location 0.6))
          bob-location (sample (location 0.6))]
      (observe (flip 1.0) (= amy-location bob-location))
      amy-location)))

(frequencies (repeatedly 1000 #(sample* ((conditional meet-at-pub-inefficient)))))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:pub</span>","value":":pub"},{"type":"html","content":"<span class='clj-long'>631</span>","value":"631"}],"value":"[:pub 631]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:starbucks</span>","value":":starbucks"},{"type":"html","content":"<span class='clj-long'>369</span>","value":"369"}],"value":"[:starbucks 369]"}],"value":"{:pub 631, :starbucks 369}"}
;; <=

;; **
;;; This model is simple enough that rejection sampling is feasible: we get a valid sample with @@p(\text{Both at same location})@@, which we computed above. For more complicated queries, this quickly becomes unreasonable.
;;; 
;;; Fortunately, we can re-write the model above such that it no longer has a hard constraint. Instead of sampling both values and having a deterministic observation, we can `observe` instead of `sample` one of the locations directly.
;;; 
;;; Exercise: convince yourself that both these programs define the exact same distribution over the predict value.
;; **

;; @@
(with-primitive-procedures [location]
  (defquery meet-at-pub-efficient []
    (let [amy-location (sample (location 0.6))
          bob-location-dist (location 0.6)]
      (observe bob-location-dist amy-location)
      amy-location)))

(frequencies (repeatedly 1000 #(sample* ((conditional meet-at-pub-efficient)))))
;; @@
;; =>
;;; {"type":"list-like","open":"<span class='clj-map'>{</span>","close":"<span class='clj-map'>}</span>","separator":", ","items":[{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:starbucks</span>","value":":starbucks"},{"type":"html","content":"<span class='clj-long'>310</span>","value":"310"}],"value":"[:starbucks 310]"},{"type":"list-like","open":"","close":"","separator":" ","items":[{"type":"html","content":"<span class='clj-keyword'>:pub</span>","value":":pub"},{"type":"html","content":"<span class='clj-long'>690</span>","value":"690"}],"value":"[:pub 690]"}],"value":"{:starbucks 310, :pub 690}"}
;; <=

;; **
;;; The true distribution here can be computed analytically. The probability they both go to the pub is 0.36, and the probability that they both go to the starbucks is 0.16, so
;;; 
;;; $$p(pub|meet) = \frac{0.36}{0.16 + 0.36} = 0.6923$$.
;;; 
;;; Whenever possible, performing this sort of transformation on queries to avoid specifying programs with rejection sample semantics is highly recommended.
;; **

;; **
;;; ## A mutually recursive model
;;; 
;;; Now, suppose Amy and Bob are a bit more clever about it. Rather than choosing independently at random, they each choose by simulating what they think the other would do.
;;; 
;;; We do this by defining mutually recursive functions for sampling the location of Amy and Bob, conditioned on what they each think the other is thinking. Roughly:
;;; 
;;; * Amy wants to go where Bob goes
;;; * Amy knows Bob wants to go where Amy goes
;;; * Amy knows Bob knows that Amy wants to go where Bob goes
;;; 
;;; and so on. This recursion would continue indefinitely, so we add a `depth` parameter explicitly limiting the amount of meta-reasoning which takes place.
;;; 
;;; In the base case of the recursion, one of the actors (here, Bob) eventually chooses to simply pick a location.
;;; 
;;; The behaviour of the two actors are defined in the Anglican functions `amy` and `bob`, which each call one another, and return a location. We use the same reparameterization trick as above to specify our model in a way which avoids conditioning on hard constraints.
;; **

;; @@
(declare amy bob)

(with-primitive-procedures [location]
  (defm amy [depth]
    (let [amy-location (location 0.6)
          bob-location (bob (dec depth))]
      (observe amy-location bob-location)
      bob-location))

  (defm bob [depth]
    (let [bob-location (location 0.6)]
      (if (> depth 0)
        (let [amy-location (amy depth)]
          (observe bob-location amy-location)
          amy-location)
        (sample bob-location)))))

;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;coordination-game/bob</span>","value":"#'coordination-game/bob"}
;; <=

;; **
;;; Here is a helper function which runs both Amy's and Bob's location selection procedures for a given recursion depth. We want to predict:
;;; 
;;; * What is the probability Amy is at the pub?
;;; * What is the probability Bob is at the pub?
;;; 
;;; The answers to these will vary according to the supplied `meta-reasoning-depth`.
;; **

;; @@
(defn coordinate [meta-reasoning-depth]
  (let [N 1000
        amy-dist ((conditional (query [depth] (amy depth)) :lmh) meta-reasoning-depth)
        bob-dist ((conditional (query [depth] (bob depth)) :lmh) meta-reasoning-depth)
        amy-samples (repeatedly N #(sample* amy-dist))
        bob-samples (repeatedly N #(sample* bob-dist))
        _ (prn (take 5 amy-samples))
        pub-probability (fn [outcomes] (/ (count (filter #(= :pub %) outcomes)) (float N)))]
    (println "recursion depth: " meta-reasoning-depth)
    (println "p(Amy at pub): " (pub-probability  amy-samples))
    (println "p(Bob at pub): " (pub-probability  bob-samples))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;coordination-game/coordinate</span>","value":"#'coordination-game/coordinate"}
;; <=

;; **
;;; At a recursion depth of zero, Bob just picks at random, so is at the pub with probabilty 0.6.
;;; Amy knows this, so is more likely to go to the pub herself.
;; **

;; @@
(def depth 0)
(coordinate depth)
;; @@
;; ->
;;; (:pub :pub :pub :pub :pub)
;;; recursion depth:  0
;;; p(Amy at pub):  0.695
;;; p(Bob at pub):  0.618
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; At a recursion depth of 1, Bob also reasons about what Amy will do.
;;; Bob is now much more likely to go to the pub, too.
;; **

;; @@
(def depth 1)
(coordinate depth)
;; @@
;; ->
;;; (:starbucks :starbucks :starbucks :starbucks :starbucks)
;;; recursion depth:  1
;;; p(Amy at pub):  0.698
;;; p(Bob at pub):  0.785
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; At higher recursion depths, they both become more likely to go to the pub.
;;; By depth 3, this is greater than 90% for both of them: as they both "think harder", they are more likely to reach the best outcome -- both going to the pub.
;; **

;; @@
(def depth 2)
(coordinate depth)

(def depth 3)
(coordinate depth)
;; @@
;; ->
;;; (:pub :pub :pub :pub :pub)
;;; recursion depth:  2
;;; p(Amy at pub):  0.826
;;; p(Bob at pub):  0.863
;;; (:pub :pub :pub :pub :pub)
;;; recursion depth:  3
;;; p(Amy at pub):  0.929
;;; p(Bob at pub):  0.965
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=

;; **
;;; ## Exercise: Coordination game with false beliefs
;;; 
;;; Now consider a modification of this coordination game:
;;; 
;;; suppose Amy is actually trying to avoid Bob, but Bob doesn't know this, and Amy knows that Bob doesn't know this.
;;; 
;;; We can write this as an Anglican program where Bob simulates from an incorrect model for Amy, using the `amy-true-model`, `amy-false-model`, and `bob-confused` functions below:
;; **

;; @@
(declare amy-true-model amy-false-model bob-confused)

(with-primitive-procedures [location]
  (defm amy-true-model [depth]
    (let [amy-location (location 0.6)
          not-bob-location (if (= (bob-confused (dec depth)) :pub)
                             :starbucks
                             :pub)]
      (observe amy-location not-bob-location)
      not-bob-location))

  (defm amy-false-model [depth]
    (let [amy-location (location 0.6)
          bob-location (bob-confused (dec depth))]
      (observe amy-location bob-location)
      bob-location))

  (defm bob-confused [depth]
    (let [bob-location (location 0.6)]
      (if (> depth 0)
        (let [amy-location (amy-false-model depth)]
          (observe bob-location amy-location)
          amy-location)
        (sample bob-location)))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;coordination-game/bob-confused</span>","value":"#'coordination-game/bob-confused"}
;; <=

;; **
;;; Here is a similar helper function as before, to estimate the probability of each of them selecting the pub.
;; **

;; @@
(defn coordinate-false-belief [meta-reasoning-depth]
  (let [N 1000
        amy-dist ((conditional (query [depth]  (amy-true-model depth)) :lmh) meta-reasoning-depth)
        bob-dist ((conditional (query [depth]  (bob-confused depth)) :lmh) meta-reasoning-depth)
        amy-samples (repeatedly N #(sample* amy-dist))
        bob-samples (repeatedly N #(sample* bob-dist))
        pub-probability (fn [outcomes] (/ (count (filter #(= :pub %) outcomes)) (float N)))]
    (println "recursion depth: " meta-reasoning-depth)
    (println "p(Amy at pub): " (pub-probability amy-samples))
    (println "p(Bob at pub): " (pub-probability bob-samples))))
;; @@
;; =>
;;; {"type":"html","content":"<span class='clj-var'>#&#x27;coordination-game/coordinate-false-belief</span>","value":"#'coordination-game/coordinate-false-belief"}
;; <=

;; **
;;; The more meta-reasoning involved, the more likely Amy is going to Starbucks (and successfully dodge Bob), and the more Bob will believe that going to the pub will lead to meeting Amy.
;; **

;; @@
(coordinate-false-belief 0)
(coordinate-false-belief 1)
(coordinate-false-belief 2)
(coordinate-false-belief 3)
;; @@
;; ->
;;; recursion depth:  0
;;; p(Amy at pub):  0.502
;;; p(Bob at pub):  0.597
;;; recursion depth:  1
;;; p(Amy at pub):  0.476
;;; p(Bob at pub):  0.783
;;; recursion depth:  2
;;; p(Amy at pub):  0.305
;;; p(Bob at pub):  0.875
;;; recursion depth:  3
;;; p(Amy at pub):  0.166
;;; p(Bob at pub):  0.943
;;; 
;; <-
;; =>
;;; {"type":"html","content":"<span class='clj-nil'>nil</span>","value":"nil"}
;; <=
