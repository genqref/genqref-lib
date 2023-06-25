(ns genqref-lib.algo.astar)

(defn path
  "Takes a ADJACENCY-FN, which takes one argument, a spot, and returns a
  list of adjacent spots. Takes COST-FN, which takes one argument, a
  tuple of spots, and returns the the cost of moving from the first
  spot to the second. Takes two spots START and GOAL and calculates
  the cheapest path. Returns a list of spots. Returns nil if there is
  not valid path."
  [adjacency-fn cost-fn start goal]
  (loop [[been [[priority [spot path cost]] & queue]]
         [#{} [[1 [start [] 0]]]]]
    (when spot
      (if-not (been spot)
        (let [new-path (conj path spot)]
          (if (= spot goal)
            new-path
            (recur [(conj been spot)
                    (->> spot
                         adjacency-fn
                         (map (fn [new-spot]
                                (let [new-cost (+ cost (cost-fn [spot new-spot]))]
                                  [new-cost
                                   [new-spot new-path new-cost]])))
                         (concat queue)
                         (sort-by first))])))
        (recur [been queue])))))

#_(path {:a [:b :c]} (constantly 1) :a :b)
#_(path {:a [:b] :b [:c]} (constantly 1) :a :c)
#_(path {:a [:b] :b [:c :d] :d [:c]} #(get {[:b :c] 9} % 1) :a :c)
