(ns OnLisp.ch2)

;; Section 2.2

;; double function
(defn double [x] (* x 2))

;; invoking double
(double 1)


;; lambdas are defined two ways in Clojure
(fn [x] (* x 2))
#(* % 2)

;; invoking an anonymous function
((fn [x] (* x 2)) 3)
(#(* % 2) 3)

;; we can assign a function to a variable
(def x double)
(x 2)

;; Section 2.3

;; four expressions with the same effect
(+ 1 2)
(apply + '(1 2))
(apply + 1 '(2))


;; funcall doesn't exist in Clojure

;; map applies a function to each argument in a list
(map #(+ % 10) '(1 2 3))
(map + '(1 2 3) '(10 100 1000))

;; sort is ascending by default
(sort '(1 4 2 5 6 7 3))
;; to specify sort order, pass the function then the list
(sort > '(1 4 2 5 6 7 3))

;; instead of remove-if, in Clojure use filter, true values are returned
(filter odd? '( 1 2 3 4 5 6 7))
;; pred can also be an anonymous function
(filter #(= 0 (rem % 3)) '(1 2 3 4 5 6 7 8 9))

;; our-remove-if was implemented with recursion in the book.
(defn our-remove-if [pred lst]
  (if (empty? lst)
    nil
    (if (pred (first lst))
      (our-remove-if pred (rest lst))
      (cons (first lst) (our-remove-if pred (rest lst))))))

(our-remove-if even? '(1 2 3 4 5))

;; Section 2.4

;; conditional version
(defn behave [animal]
  (cond
   (= animal 'dog) (do '(wag-tail) '(bark))
   (= animal 'rat) (do '(scurry) '(squeek))
   (= animal 'cat) (do '(rub-legs) '(scratch-carpet))))

;; Protocols. Define the protocol
(defprotocol animal
  (behave [this] ))

;; define a dog
(defrecord dog [breed]  )

;; add the animal protocol to dog type
(extend dog
  animal
  {:behave (fn [src] (do '(wag-tail) '(bark)))})

;; create a dog
(def my-dog (dog. "collie"))

;; see what it does
(behave my-dog)

;; define a rat
(deftype rat [color])

;; add the animal protocol to the rat type
(extend rat
  animal
  {:behave (fn [src] (do '(scurry) '(squeek)))})

;; create a rat
(def brown-rat (rat. "brown") )

;; see what it does
(behave brown-rat)

(extend String
  animal
  {:behave (fn [src] (do '(what)))})

(behave "huh")

;; Multimethods. Define the multimethod type
(defmulti behave-multi identity)

;; define implementations for our animals
(defmethod behave-multi 'dog [x]
  (do '(wag-tail) '(bark)))
(defmethod behave-multi 'rat [x]
  do '(scurry) '(squeek))

;; try them out
(behave-multi 'dog)
(behave-multi 'rat)

;; You can dispatch on parameter values,
;; not just parameter types
(defmulti two-behaviors
  (fn [num] (if (odd? num) :odd :even)))

(defmethod two-behaviors :odd [num]
  (str num " is odd"))

(defmethod two-behaviors :even [num]
  (str num " is even"))

(two-behaviors 3)
(two-behaviors 4)


;; Section 2.5

(let [y 7]
  (defn scope-test [x]
    (list x y)))

(let [y 5]
  (scope-test 3))

;; Section 2.6
(defn list+ [lst n]
  (map (fn [x] (+ x n))
       lst))

(list+ '(1 2 3) 10)


;; lexically scoped counter
(let [counter (atom 0)]
  (defn new-id [] (swap! counter + 1))
  (defn reset-id [] (reset! counter 0 )))

(new-id)
(reset-id)


;; adder fuctions
(defn make-adder [n]
  (fn [x] (+ x n)))

(def add2 (make-adder 2))
(def add10 (make-adder 10))
(add2 5)
(add10 3)


;; adder that allows changing the added amount
(defn make-adderb [n]
  (let [amt (atom n)]
    (fn [x & change ]
      (if change  (reset! amt x ))
      (+ x @amt))))

(def addx (make-adderb 1))

(addx 3)
(addx 100 true)
(addx 3)


;; First dbms version
(defn make-dbms [db]
  (list
   (fn [key]
     (db key))
   (fn [key val]
     (assoc db key val))
   (fn [key]
     (dissoc db key))))

(def cities (make-dbms {'boston 'us, 'paris 'france}) )

((first cities) 'boston)
((second cities) 'london 'england)
((last cities) 'boston)

;; Mutable dbms example

(defn make-mutable-dbms [db]
  (let [mdb (atom db)]
    (list
     (fn [key]
       (@mdb key))
     (fn [key val]
       (swap! mdb assoc key val))
     (fn [key]
       (swap! mdb dissoc key)))))

(def citiesx (make-mutable-dbms
  {'boston 'us, 'paris 'france}) )

((first citiesx) 'boston)
((second citiesx) 'london 'england)
((last citiesx) 'boston)


;; dbms with commands stored in a
;; map instead of a list

(defn make-dbms-map [db]
  (let [mdb (atom db)]
    {:select (fn [key] (@mdb key))
      :insert (fn [key val]
        (swap! mdb assoc key val))
      :delete (fn [key]
        (swap! mdb dissoc key))}))

(def citiesm (make-dbms-map
  {'boston 'us 'paris 'france}))
((:select citiesm) 'boston)
((:insert citiesm) 'london 'england)
((:delete citiesm) 'boston)

;; Section 2.7
;; apply function to all items in list
(map (fn [x] (+ 2 x))
     '(2 5 7 3))
;; or
(map #(+ 2 %)
     '(2 5 7 3))
;; or
(map (partial + 2)
     '(2 5 7 3))

(map inc '(1 2 3))

(defn list+ [lst n]
  (map #(+ n %) lst))
(list+ '(1 2 3) 3)

;; let binds sequentially
(let [my-inc (fn [x] (+ x 1))] (my-inc 3))
(let [x 10 y x] y)

(defn count-instances [obj lists]
  (map (fn instances-in [lst]
    (if (seq lst)
      (+ (if (= (first lst) obj) 1 0)
         (instances-in (rest lst))) 0)) lists))

(count-instances 2 [[1 2 3] [1 2] [1]])

;; Section 2.8
(defn our-find-if [pred lst]
  (if (pred (first lst))
    (first lst) (recur pred (rest lst))))
(our-find-if even? [1 2 3 4])

;; tail call version of our lenght

(defn our-length [lst]
  (loop [lst lst acc 0]
    (if (empty? lst)
      acc
      (recur (rest lst) (+ acc 1) ))))
(our-length '(1 2 3 4))

;; using type hints for performace
(defn triangle [^long n]
  (loop [c 0 n n]
    (if (zero? n)
    c
    (recur (+ n c) (- n 1)))))
(triangle 1000000)
