(ns OnLisp.ch4)

;; Section 4.1
(def people ["William" "Samuel"])

(defn nicknames [name]
  (if (= name "William")
    '(bill will billy)
    nil))

(defn all-nicknames [names]
  ( loop [names names acc {}]
    (if (empty? names)
      {}
      (recur (rest names)
        (conj (nicknames (first names) acc))))))

;; shorter version
(map nicknames people)


;; find function
(defn find2 [fn lst]
  (if (empty? lst)
    nil
    (if-let [val (fn (first lst))]
      {(first lst) val}
      (recur fn (rest lst)))))

(def bars {:atlanta '(taco-mac)
           :boston '(cheers)})

(defn return-bars [town]
  (town bars))

(find2 return-bars [:chicago :atlanta ])


;; Section 4.2

;; compare the length of 2 collections
(> (count [1 2 3]) (count [1 2]))

;; join lists together before mapping over them
(map even? (reduce into '([1 2] [3 4] [5 6])))

;; Section 4.3
(conj [1 2 3] 4) ;; vector => [1 2 3 4]
(conj '(2 3 4) 1) ;; list => '(1 2 3 4)
(conj #{1 2 3} 4) ;; set => #{1 2 3 4}

(defn mklist [obj]
  (if (list? obj) obj (list obj)))

(defn mklist [obj]
  (if (sequential? obj) obj (list obj)))

(mklist [1 2])

;; No definition of lookup or data are
;; provided, but if they
; (map #(mklist (lookup %)) data)
(map inc
     (filter
      (fn [x] (instance? Number x))
      ['a 1 2 'b 3 'c 'd  4]) )

(map (partial + 2)
     (filter (fn [x] (instance? Number x))
          ['a 1 2 'b 3 'c 'd 4]))

(defn longer? [x y]
  (if (or (empty? x) (empty? y))
    (not (empty? x))
    (recur (rest x) (rest y))))

(take 5 (filter odd? (iterate inc 1)))

(partition 2 [1 2 3 4 5])
;; ((1 2) (3 4))
(partition-all 2 [1 2 3 4 5])
;; ((1 2) (3 4) (5))
(take 5 (partition 2 (iterate inc 1)))
;; ((1 2) (3 4) (5 6) (7 8) (9 10))


;; Section 4.5

(defn map-> [fn a test-fn succ-fn]
  (map fn (take-while test-fn
            (iterate succ-fn a))))

(map-> inc -2 #(< % 2) (partial + 0.5 ))

;; Section 4.6
(defn read-list []
  (str \( (read-line) \)))

(defn prompt [text]
  (do
    (println text)
    (read-line)))

(defn break-loop [fn quit]
  (let [x (read-string
    (prompt (str "type " quit " to quit")))]
    (if (not (= x quit))
      (do
        (fn x)
        (recur fn quit)))))

(break-loop #(println (eval %)) :q)

;; Section 4.7
(str Math/PI " pieces of " 'pi)
;;seq can convert strings into sequences of characters
(seq "bomb")
;; => \b \o \m \b

;;strings can be converted to symbols, with
;;or without namespaces
(symbol "foo")
(symbol "some-ns" "foo")

;;Clojure also adds keywords, which you can convert
;;to from strings:
(keyword "foo")
(keyword "some-ns" "foo")

;;If you are not qualifying the keyword with a namespace,
;;you can also create a keyword directly from a symbol
(keyword 'foo)
(keyword "some-ns" (str 'foo))
