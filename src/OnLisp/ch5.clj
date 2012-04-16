(ns OnLisp.ch5
  (:use [clojure.repl]
        [seesaw.core]))

;; Section 5.1
(filter even? (range 10))
;; => (0 2 4 6 8)

(filter (complement even?) (range 10))
;; -> (1 3 5 7 9)


(defn joiner [obj]
  (cond
   (sequential? obj) (partial conj obj)
   (number? obj) (partial + obj)))

(defn make-adder [n]
  (partial + n))

;; Section 5.2
(assoc {:a 1 :b 2} :b 3)

(def f (frame :title "Getting to know Seesaw"))

(-> f pack! show!)

; The properties of a widget can be queried ...
(config f :title)
;=> "Get to know Seesaw"

; ... and modified
(config! f :title "No RLY, get to know")
;=> #

;; Section 5.3
(defn slow [x]
  (do (Thread/sleep 5000) x))

(def slow (memoize slow))

(slow 1)

;; Section 5.4
((comp inc *) 2 2)
;; => 5

((juxt inc dec) 2)
;; => [3 1]

;; First the examples of recursive functions:

(defn our-length [lst ]
  (loop [lst lst count 0]
    (if (seq lst)
      (recur (rest lst) (inc count))
      count )))

(defn our-every [fn lst]
  (loop [lst lst result true]
    (if (seq lst)
      (recur (rest lst)
             (and result (fn (first lst))))
      result)))


;;And now a function that returns a recursive function:

(defn lrec [trans base]
  (fn [lst]
    (loop [lst lst result base]
      (if (seq lst)
        (recur (rest lst) (trans (first lst) result))
        result))))

((lrec (fn [new acc] (inc acc)) 0) [2 4 6])
((lrec (fn [new acc] (and acc (even? new))) true) [2 4 6 7])
