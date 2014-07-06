(ns OnLisp.ch3
  (:use [clojure.math.numeric-tower
   :only [expt]]))

;; Section 3.1

(defn good-reverse [lst]
  (loop [lst lst acc ()]
    (if (= () lst)
      acc
      (recur (rest lst)
        (cons (first lst) acc )))))

(good-reverse [1 2 3 4])


(defn split-decimal [n]
  {:quotient (quot n 1)
   :remainder (rem n 1)
   :quotent-int (int n)})

(split-decimal 26.235)
(split-decimal 26.235M)


;; Section 3.2
; functional with java math
(defn fun [x]
  (list 'a (Math/pow (first x) 2)))

; functional with clojure math
(defn fun [x]
  (list 'a (expt (first x) 2)))

; imperative
(defn imp [x]
  (let [y (first x) sqr (expt y 2)]
    (list 'a sqr)))

;; Section 3.3
(defn qualify [expr]
  (cons 'maybe expr))

(qualify '(this is true))

(defn qualify [expr]
    (into expr '(maybe)))

(qualify ['this 'is 'true])

;; mutable vs immutable

;; x is immutable
(let [x 0]
  (defn total [y]
    (+ x y)))

;; x is mutable
(let [x (atom 0)]
  (defn total [y]
    (swap! x #(+ % y))))
