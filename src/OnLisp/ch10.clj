(ns OnLisp.ch10)

:: Section 10.1
;; correct version
(defmacro for' [[var start stop] & body]
  `(let [~var (atom ~start) gstop# ~stop]
     (while (< (deref ~var) gstop#)
         ~@body
         (swap! ~var inc))))

;; subject to multiple evaluations
(defmacro for' [[var start stop] & body]
  `(let [~var (atom ~start)]
     (while (< (deref ~var) ~stop)
       ~@body
       (swap! ~var inc))))


;; incorrect order of evaluation
(defmacro for' [[var start stop] & body]
    `(let [gstop# ~stop ~var (atom ~start)]
       (while (< (deref ~var) ~stop)
         (swap! ~var inc)
         ~@body)))

;; Section 10.2
(def x (atom 10))

(+ (reset! x 3) @x)

;; notice the difference between calling the correct for' and the incorrect order of evaluation version.
(let [x (atom 1)]
  (for' [i @x (reset! x 13)]
        (println @i)))

;; Section 10.3

;; Section 10.4
(defn our-length [x]
  (loop [lst x acc 0]
    (if (empty? lst) acc
        (recur (rest lst) (inc acc)))))

(defn ntha [n lst]
  (if (= n 0)
    (first lst)
    (ntha (- n 1) (rest lst))))

(defmacro nthb [n lst]
  `(if (= ~n 0)
     (first ~lst)
     (nthb (- ~n 1) (rest ~lst))))

(macroexpand-1 '(nthb 2 [1 2 3 4 5]))

(defmacro nthe [n lst]
  `(loop [n# ~n lst# ~lst]
     (if (= n# 0)
       (first lst#)
       (recur (dec n#) (rest lst#)))))


(defn or-expand [args]
  (if (empty? args)
    nil
    (let [sym (first args)]
       (if sym
         sym
         (or-expand (rest args))))))

(defmacro ora [& args]
  (or-expand args))

(defmacro orb [& args]
  (loop [lst args]
    (if (empty? lst) false
        (let [sym (first lst)]
          (if sym sym (recur (rest lst)))))))
