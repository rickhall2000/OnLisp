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


;; Section 7.5

(let [[x [y] & z] ['a ['b] 'c 'd ]]
  (list x y z))

(doseq [x '(1 2 3)]
  (println x))

;; this follows the same pattern as Graham's function, but because
;; map is lazy, it is never actually realized!
(defmacro our-dolist [[lst & result] & body]
  `(do  (map ~@body ~lst)
        ~@result))


(macroexpand-1 (our-dolist [[1 2 3] ] #(println %)))
(macroexpand-1 (our-dolist [[1 2 3] 4] #(println %)))

(defmacro when-bind [[var expr] body]
  `(let [~var ~expr]
     (when ~var)
     ~@body ))

(when-bind [input "something"] (println input))

;; Section 7.6
(defmacro our-defmacro [name params & body]
  `(defn ~name [~@params]
     (do
       ~@body)))

(macroexpand-1 '(our-defmacro test [x] (println x)(+ x 2)))

;; Section 7.7  Just because we can do a thing, it does not follow that we must do a thing

;; our desired result
(let [w 3 x 1 y 2 z nil]
  (loop [x x y y]
    (if (> x 10)
      (do (println z) y )
       (do
         (println x)
         (println y)
         (recur (inc x) (inc y))))))


;; the macro
(defmacro our-looper [{:keys [initial-vals
                              body
                              loop-params
                              recursion-expr
                              exit-cond
                              exit-code
                              return-val]}]
  `(let [~@initial-vals]
     (loop [~@loop-params]
       (if ~exit-cond
         (do ~exit-code
             ~return-val)
         (do ~@body
             (recur ~@recursion-expr))
         ))))

;; how to call it
(our-looper {:initial-vals [w 3 x 1 y 2 z nil]
             :body ((println x) (println y))
             :loop-params [x x y y]
             :recursion-expr ((inc x) (inc y))
             :exit-cond (> x 10)
             :exit-code (println z)
             :return-val y})


;; Section 7.8
(defmacro our-and [& args]
  (loop [lst args]
    (cond
     (= (count lst) 0) true
     (= (count lst) 1) (first lst)
     :else (if (first lst) (recur (rest lst)) false))))


;; section 7.9
;; if func a depends on func b and func b gets modified, func a knows about it
;; if func c depends on macro d and macro d gets modified, func c doesn't know
(defn func-a [input]
  (+ input 1))

(defn func-b []
  (func-a 3))

(func-b)
;; 4

(defn func-a [input]
  (+ input 10))

(func-b)
;; 13


(defmacro macro-c [input]
  `(+ ~input 1))

(defn func-d []
  (macro-d 3))

(func-d)
;; 4

(defmacro macro-c [input]
  `(+ ~input 10))

(func-d)
;; 4

;; Section 7.10
(defn second-f [x]
  (first (rest x)))

(defmacro second-m [x]
  `(first (rest ~x)))

(defn noisy-second-f [x]
  (println "Someone is taking a cadr")
  (first (rest x)))

(defmacro noisy-second-m [x]
  `(do
     (println "Someone is taking a cadr")
     (first (rest ~x))))

(defn sum-f [& args]
  (apply + args))

(defmacro sum-m [& args]
  `(apply + (list ~@args)))

(defmacro sum2-m [& args]
  `(+ ~@args))


(defn foo [x y z]
  (list x (let [x y]
            (list x z))))

(defmacro foo-m [x y z]
  `(list ~x
         (let [x# ~y]
           (list x# ~z))))
