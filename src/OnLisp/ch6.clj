(ns OnLisp.ch6)

;; section 6.1

(def nodes
  {:people {:question "Is the person a man?" :yes :male :no :female}
   :male {:question "Is he dead?" :yes :deadman :no :liveman }
   :deadman {:question "Was he American?" :yes :us :no :them}
   :us {:question "Is he on a coin?" :yes :coin :no :cidence}
   :coin {:question "Is the coin a penny?" :yes :penny :no :coins}
   :penny {:answer "Lincoln"}})

(defn ask-question [question]
  true)

(defn run-node [name nodes]
  (let [n (name nodes)]
    (if-let [result (get n :answer)]
      result
      (if (ask-question (:question n))
        (recur (:yes n) nodes)
        (recur (:no n) nodes)))))

(run-node :people nodes )


;; function to add nodes
(defn add-node
  ([nodes tag answer]
     (conj nodes {tag {:answer answer}}))
  ([nodes tag question yes no]
     (conj nodes {tag {:question question :yes yes :no no}})))

(add-node nodes :liveman "Is he a former president" :texas :california)
(add-node nodes :texas "George W Bush")

(-> (add-node nodes :liveman "Is he a former president" :texas :california)
    (add-node :texas "George W Bush"))

;; Section 6.2



(defn add-node2
  ([nodes tag answer]
     (conj nodes {tag answer}))
  ([nodes tag question yes no]
     (conj nodes {tag (if (ask-question question) (yes nodes) (no nodes))})))

;; this is not working the way I expected
(def node2
  (-> (add-node2 {} :people "Is the preson a man?" :male :female)
      (add-node2 :male "Is he dead?" :deadman :liveman)))
node2
;; => {:male nil, :people nil}

(def node2
  (-> (add-node2 {} :penny "Lincoln")
      (add-node2 :coin "is the coin a penny?" :penny :coins)
      (add-node2 :us "Is he on a coin" :coin :cindence)))
node2
;; =>  {:us "Lincoln", :coin "Lincoln", :penny "Lincoln"}

;; this didn't fix it
(defn add-node2
  ([nodes tag answer]
     (conj nodes {tag answer}))
  ([nodes tag question yes no]
     (conj nodes
           {tag
            (if ((fn [x] (ask-question x)) question )
              (yes nodes) (no nodes) )})))

;; neither did this
(defn prompt [text]
  (do
    (println text)
    (read-line)))

(defn ask-question [question]
  (prompt question))
