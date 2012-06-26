(ns OnLisp.ch11)

;; Section 11.1
(let [x 'b] (list x))

(defmacro our-let [binds & body]
  `((fn [~@(take-nth 2 binds)]
       (do ~@body)) ~@(take-nth 2 (rest binds))))

(macroexpand-1 '(our-let [x 1 y 2] (+ x y)))

(our-let [x 1 y 2] (+ x y))

(defmacro when-bind [[var expr] & body]
  `(let [~var ~expr]
     (when ~var
       ~@body)))

#_(let [sun-place 'park rain-place 'library]
  (if (sunny)
    (visit sun-place)
    (visit rain-place)))

(defn condlet-bind [bindings]
  (loop [binds bindings]
    (cond (empty? binds) []
          (first binds) (first (rest binds))
          :default (recur (rest (rest binds))))))

(defmacro condlet [clauses & body]
  `(let ~(condlet-bind clauses)
     ~@body))

(macroexpand-1
 '(condlet [(= 1 2) [x 2 y 3]
           (= 1 1) [x 1 y 2]]
          (println (str "sum is " (+ x y)))))

(condlet [(= 1 2) [x 2 y 3]
           (= 1 1) [x 1 y 2]]
          (println (str "sum is " (+ x y))))


;; Section 11.2

;; Clojure has a with-open macro that is used for dealing with all sorts of resources
(with-open [writer (clojure.java.io/writer "output-file" :append true)]
  (.write writer "99"))

;; file io has its own set of macros, spit and slurp
(spit "output.txt" "test" :append true )
(slurp "output.txt")

;; unwind-protect becomes try catch
(try
  (do (println "What error?")
      (throw (Exception. "This Error."))
      (println "This won't run"))
  (catch Exception e (.getMessage e))
  (finally (println "this runs regardless")))


;; with clojure data types locks are unnecessary.
(def ^:dynamic *db* "some connection")

(binding [ *db* "other connection"]
         (println *db*))


(def db2 (StringBuilder. "connection"))

;; this is the code we are going to replace with a macro
(let [old-val (.toString db2)]
  (.replace db2 0 (.length db2) "new connection")
  (locking db2
    (println (.toString db2)))
  (.replace db2 0 (.length db2) old-val))

;; with-db abstracts away the inner details
(defmacro with-db [db & body]
  `(let [temp# (.toString db2)]
    (try
      (.replace db2 0 (.length db2) ~db)
      (locking db2
        ~@body)
      (finally
       (.replace db2 0 (.length db2) temp#)))))

;; this is much nicer than the let form above.
(with-db "new connection"
   (println (.toString db2)))



;; Section 11.3
(if true 'phew (/ 2 0))


(defmacro if3 [test t-case nil-case f-case]
  `(let [expr# ~test]
     (cond
      (nil? expr#) ~nil-case
      (false? expr#) ~f-case
      :default ~t-case)))

(defmacro nif  [expr pos zero neg]
  `(let [expr# ~expr]
    (cond
     (pos? expr#) ~pos
     (zero? expr#) ~zero
     :default ~neg)))


;; Clojure version of Graham's `in` macro
(defmacro in? [needle & haystack]
  ( let [target# needle]
    `(or ~@(map (fn [x#] `(= ~x# ~target#)) haystack))))

(macroexpand-1
 '(in? 1 1 2 3))

;; Just to make sure it is working the way we hope
(in? 1 1 (do (println "called") 2) 3)


;; using lazyness
(defn member? [needle haystack]
  (take 1 (filter (partial = needle)  haystack)))

(member? 2 (iterate inc 1) )

(defmacro in-if [func needle & haystack]
  ( let [target# needle]
    `(or ~@(map (fn [x#] `(~func ~x# ~target#)) haystack))))

;; Clojure's cond already behaves like Graham's >case
(cond
 (do (println "First cond") false) (println "one")
 (do (println "Second cond") true) (println "two")
 (do (println "Third cond") true) (println "three")
 :default (println "Nothing matched"))

;; Section 11.4
(defmacro do-tuples-o [parms source & body]
  (let [src# source]
    `(map (fn [~parms] ~@body)
          (partition (count (quote ~parms)) 1 ~src#))))

;; an example call to do-tuples-o
(do-tuples-o [x y] [1 2 3 4 5] (+ x y))

;; do-tuples-o functionality without the macro
(map
 (fn [[x y]] (+ x y))
 (partition 2 1 [1 2 3 4 5]))


(defn partition-round [size step source]
  (partition size step
             (take (- (+ size (count source)) step)
                   (cycle source))))

(defmacro do-tuples-c [parms source & body]
  (let [src# source]
    `(map (fn [~parms] ~@body)
          (partition-round (count ( quote ~parms)) 1 ~src#))))


(do-tuples-c [x y z] [1 2 3 4 5] (+ x y z))

(map
 (fn [[x y z]] (+ x y z))
 (partition-round 3 1 [1 2 3 4 5]))

;; Section 11.6 The Need for Macros
(defn fnif [test then else]
  (if test (then) (else)))

;; we have to pass in the code as functions to delay evaluation
(fnif true (fn [] (do (println "true") (+ 1 2))) (fn []  (do (println "false")  (- 2 1))))

(defn forever [fn]
  (if true
    (do
      (fn)
      (recur fn))
    'done))

;; commented out to prevent running on loading (or ever)
#_(forever (fn [] (println "this is dumb")))
