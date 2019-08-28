(def c 100)  ;number of children in each generation
(def p 0.05) ;mutation probability
 
(def target "METHINKS IT IS LIKE A WEASEL")
(def tsize (count target))
 
(def alphabet " ABCDEFGHIJLKLMNOPQRSTUVWXYZ")

(defn fitness [s] (count (filter true? (map = s target))))
(defn perfectly-fit? [s] (= (fitness s) tsize))
 
(defn randc [] (rand-nth alphabet))
(defn mutate [s] (map #(if (< (rand) p) (randc) %) s))

(loop [generation 1, parent (repeatedly tsize randc)]
  (println generation, (apply str parent), (fitness parent))
  (if-not (perfectly-fit? parent)
    (let [children (repeatedly c #(mutate parent))
          fittest (apply max-key fitness parent children)]
      (recur (inc generation), fittest))))