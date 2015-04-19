(ns distribution
  (:refer-clojure :exclude [rand rand-int rand-nth])
  (:use [anglican
         [core :only [doquery]]
         [runtime :exclude [distribution]]
         [state :only [get-predicts get-log-weight]]
         [inference :only [rand]]
         emit]))

(defmacro distribution [args & body] 
  (let [default-options {:algorithm :lmh}
        [options & body] (if (map? (first body))
                         body
                         (cons default-options body))
        options (merge default-options options)
        algorithm (:algorithm options)
        options (flatten (seq (dissoc options :algorithm)))]

    `(let [~'samples (ref (doquery ~algorithm 
                                   (query ~@body) ~args
                                   ~@options))
           ~'next-sample 
           (fn []
             (let [~'sample
                   (dosync
                     (let [[~'first ~'second & ~'more] @~'samples
                           ~'sample
                           (if (> (- (get-log-weight ~'second)
                                     (get-log-weight ~'first))
                                  (log (rand)))
                             ~'second
                             ~'first)]
                       (ref-set ~'samples (cons ~'sample ~'more))
                       ~'sample))]
               (if (> (get-log-weight ~'sample) (/ -1. 0.))
                 (get-predicts ~'sample)
                 (recur))))]

       (reify anglican.runtime.distribution
         (sample [~'this] (~'next-sample))
         (observe [~'this ~'value]
           (assert false (str "cannot call observe"
                              " on distributions of this type")))))))
