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



;; Section 11.3
(if true 'phew (/ 2 0))
