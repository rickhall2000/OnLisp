(ns OnLisp.ch7)

;; 7.1

;; set atom to nil
(def x (atom 0))
(reset! x 5)

(defmacro atomnil! [var]
  (list 'reset! var nil))

(atomnil! x)


;; set ref to nil
(def y (ref 0))
(dosync
 (ref-set y 5))

(defmacro refnil! [var]
  (list 'dosync (list 'ref-set var nil)))

(refnil! y)


;; 7.2
;; in clojure the backquote is named "syntax quote" but is still the same symbol `
;; in clojure unquoting is done with a tilde ~ instead of a comma ,
(defmacro atomnil! [var]
  `(reset! ~var nil))
(atomnil! x)


(macroexpand-1 '(atomnil! x))

(defmacro refnil! [var]
  `(dosync (ref-set ~var nil)))

(refnil! y)
(macroexpand-1 '(refnil! y))


;; 3 way numerical if
(defmacro nif [expr pos zero neg]
  `(cond
    (< ~expr 0) ~neg
    (> ~expr 0) ~pos
    :else ~zero ))

(nif -1 "pos" "zero" "neg")

;; Just as the , was replaced by ~  `@ becomes ~@

(def b '(1 2 3))

`(a ~b c)
;; => OnLisp.ch7/a (1 2 3) OnLisp.ch7/c

`(a ~@b c)
;; => OnLisp.ch7/a 1 2 3 OnLisp.ch7/c

;; like progn in clisp do executes a series of statements and returns the value of the last
(defmacro our-when [test & body]
  `(if ~test
     (do ~@body)))


(our-when (< 1 2)
          (println "This is a side effect")
          (println "This is another side effect")
          "this is a value")


;; 7.3 Section

;; Clojure doesn't have a member function
(defmacro memq [obj lst]
  `(some (partial = ~obj) ~lst))

;; ok, now it does
(defn member [obj lst]
  (some (partial = obj) lst))

;; and they work the same
(member 4 [1 2 3])
(memq 4 [1 2 3])


;; not the most durable implementation
(defmacro our-while [test & body]
  `(if ~test
    (do
      (swap! ~test not)
      ~@body)))

(our-while (atom true) (println "side effect") "Value")


;; Section 7.4
(defmacro mac [expr]
  `(clojure.pprint/pprint (macroexpand-1 '~expr)))

(mac (our-while (atom true) (println "side effect") "Value"))
