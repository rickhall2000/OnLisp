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
