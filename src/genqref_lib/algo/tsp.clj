(ns genqref-lib.algo.tsp
  "Traveling Sales Man, well not really atm, but at least a poor man's
  approximation."
  (:require [genqref-lib.util :as util]))

(defn closest [loc locs]
  (->> locs (sort-by (partial util/distance loc)) first))

(defn poor-mans-tsp
  "Given a SHIP and WAYPOINTS, approximate a tsp route."
  [waypoints start]
  (loop [current start
         remaining (remove #{start} waypoints)
         route [start]]
    (if (empty? remaining)
      route
      (let [next (closest current remaining)]
        (recur next
               (remove #{next} remaining)
               (conj route next))))))

#_(def waypoints [{:id "A" :x -5 :y 5}
                  {:id "C" :x 5 :y -5}
                  {:id "B" :x 4 :y 5}
                  {:id "D" :x -5 :y -5}])
#_(poor-mans waypoints (first waypoints))
