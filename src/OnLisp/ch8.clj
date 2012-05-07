(ns OnLisp.ch8)

;; Section 8.1
(defn addf [x]
  (+ x 1))

(defmacro addm [x]
  `(+ 1 ~x))

(defmacro our-while [test & body]
  `(if ~test
     (do
       (swap! ~test not)
       ~@body)))

(defmacro foo [x]
  `(+ ~x y))

;; Section 8.2

(defn avg [& args]
  (/ (apply + args) (count args)))

(defmacro avgm [& args]
  `(/ (+ ~@args) ~(count args)))

((fn [x y] (avgm x y)) 1 3)

;; Section 8.3
;; In an earlier section we had a different nil! macro for refs and atoms, but we can combine them.
(defmacro nil! [x]
  `(cond (instance? clojure.lang.Atom ~x ) (reset! ~x nil)
         (instance? clojure.lang.Ref ~x) (dosync (ref-set ~x nil))))

;; defn is also a macro in Clojure.
(defn foo [x] (* x 2))
(def foo (fn [x] (* x 2)))

(defmacro our-defn [name params & body]
  `(def ~name
     (fn ~params  ~@body)))


;; redraw and bounds weren't in the book, but we need them to compile and run the examples.
(defn redraw [from-x from-y to-x to-y]
  (println (str "Redrawing from: " from-x "," from-y " to "
                to-x "," to-y)))

(defn bounds [objs]
  (list
   (apply min (for  [o ( :objects objs)]
                (deref  (:obj-x o))))
   (apply min (for  [o ( :objects objs)]
                (deref  (:obj-y o))))
   (apply max (for  [o ( :objects objs)]
                (+  (deref  (:obj-x o)) (deref (:obj-dx o)))))
   (apply max (for  [o ( :objects objs)]
                (+  (deref  (:obj-y o)) (deref (:obj-dy o)))))))

;; The examples before the with-redraw macro
(defn move-objs [objs dx dy]
  (let [[x0 y0 x1 y1] (bounds objs)]
    (doseq [o (:objects objs)]
      (swap! (:obj-x o) + dx)
      (swap! (:obj-y o) + dy))
    (let [[xa ya xb yb] (bounds objs)]
      (redraw (min x0 xa) (min y0 ya)
               (max x1 xb) (max y1 yb)))))

(defn scale-objs [objs factor]
  (let [[x0 y0 x1 y1] (bounds objs)]
    (doseq [o (:objects objs)]
      (swap! (:obj-dx o) * factor)
      (swap! (:obj-dy o) * factor))
    (let [[xa ya xb yb] (bounds objs)]
      (redraw (min x0 xa) (min y0 ya)
              (max x1 xb) (max y1 yb)))))

;; a sample data structure to pass to the functions
(def sample-object-collection
  {:objects [{:name "Object 1"
              :obj-x (atom 0) :obj-y (atom 0)
              :obj-dx (atom 5) :obj-dy (atom 5)}
             {:name "Object 2"
              :obj-x (atom 10) :obj-y (atom 20)
              :obj-dx (atom 20) :obj-dy (atom 20)}]})

(move-objs sample-object-collection 5 5)


;; The with-redraw macro
(defmacro with-redraw [varname objs & body]
  `(let [[x0# y0# x1# y1#] (bounds ~objs)]
     (doseq [~varname (:objects ~objs)] ~@body)
    (let [[xa# ya# xb# yb#] (bounds ~objs)]
      (redraw (min x0# xa#) (min y0# ya#)
              (max x1# xb#) (max y1# yb#)))))

;; the new improved transformations
(defn move-objs [objs dx dy]
  (with-redraw o objs
    (swap! (:obj-x o) + dx)
    (swap! (:obj-y o) + dy)))

(defn scale-objs [objs factor]
  (with-redraw o objs
    (swap! (:obj-dx o) * factor)
    (swap! (:obj-dy o) * factor)))
