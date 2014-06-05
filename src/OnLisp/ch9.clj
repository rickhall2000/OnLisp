(ns onlisp.ch9)

;; Section 9.1
;; Argument capture example
(defmacro for' [[var start stop] & body]
  `(do
     (def ~var (atom ~start))
     (def limit ~stop)
     (while (< (deref ~var) limit)
       ~@body
       (swap! ~var inc))))

;; seems to work
(for' [ x 1 10] (println (str "current value is " @x)))
;; fails with error
#_(for' [limit 1 10] (println (str "current value is " @limit)))

;; On Lisp version gives no error, but no results.  Seems to work in Clojure
(let [limit 5]
  (for' [i 1 10]
        (when (> @i limit)
          (println (str @i) ) )))



;; Section 9.2
(def w (atom []))

(defmacro gripe [warning]
  `(do (swap! w #(conj % (list ~warning)))
       nil))

(defn simple-ratio [v w]
  (let [vn (count v) wn (count w)]
    (if (or (< vn 2) (< wn 2))
      (gripe "sample < 2")
      (/ vn wn))))


;; Section 9.3
(def x 1)

(defmacro cap1 []
  `(+ x 1))

;; this won't compile, let [x ... is not legal in a defmacro
#_(defmacro cap2 [var]
  `(let [x 2 ~var 3]
     (+ x ~var)))

(defmacro cap2 [var]
  `(let [x# 2 ~var 3]
     (+ x# ~var)))

;; Section 9.5
(defn position [needle haystack]
  (loop [acc 0 lst haystack]
    (cond (empty? lst) nil
          (= needle (first lst)) acc
          :default (recur (inc acc) (rest lst)))))


(defmacro before [x y seq2]
  `(let [seq# ~seq2]
     (< (position ~x seq#)
        (position ~y seq#))))

(before (do (def seq2 '(b a)) 'a) 'b '(a b))

(defmacro for' [[var start stop] & body]
  `(let [~var (atom ~start) limit# ~stop ]
     (while (< (deref ~var) limit#)
       ~@body
       (swap! ~var inc))))

;; Section 9.8 -- I didn't get the error
(defn fun [x] (+ x 1))

(defmacro mac [x] `(fun ~x))

(mac 10)

(let [fun  (fn [y] (- y 1))]
  (mac 10))
