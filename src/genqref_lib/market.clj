(ns genqref-lib.market
  (:require [genqref-lib.api :refer [state refresh!]]
            [genqref-lib.util :as util :refer [sym]]))

(defn good
  "The price at which this good can be purchased from the market."
  [waypoint-or-market trade-good]
  (->> waypoint-or-market
       sym
       (refresh! :market)
       :tradeGoods
       (filter #(-> % :symbol #{(sym trade-good)}))
       first))

(defn purchase-price
  "The price at which this good can be purchased from the market."
  [waypoint-or-market trade-good]
  (:purchasePrice (good waypoint-or-market trade-good)))

(defn sell-price
  "The price at which this good can be sold to the market."
  [waypoint-or-market trade-good]
  (:sellPrice (good waypoint-or-market trade-good)))

(defn- lookup-mapper [{:keys [symbol] :as market}]
  (->> market
       :tradeGoods
       (map #(assoc % :waypointSymbol symbol))))

(defn lookup-table
  "Returns a flat list of all tradeGoods in the given MARKETS with
  assoc'd `:waypointSymbol`."
  [markets]
  (flatten (map lookup-mapper markets)))

;; TODO: take distance and tradeVolume into account
(defn best-market-to-purchase
  ([trade-good] (best-market-to-purchase trade-good (-> @state :markets vals)))
  ([trade-good markets]
   (->> markets
        lookup-table
        (filter #(-> % :symbol #{(sym trade-good)}))
        (sort-by :purchasePrice)
        first)))

(defn distance [m¹ m²]
  (Math/round (+ 1 (util/distance (-> @state :waypoints (get (keyword m¹)))
                                  (-> @state :waypoints (get (keyword m²)))))))

(defn- rank [profit distance]
  (float (/ profit (Math/pow distance 0.2))))

(defn- fuel-cost-reducer [{:keys [units] :as agg} [wp¹ wp²]]
  (let [dist (Math/round (+ 1 (util/distance wp¹ wp²)))
        units (+ units (int (Math/ceil (float (/ dist 50)))))
        price (purchase-price wp² "FUEL")]
    (if price ;; if wp² does not sell FUEL we accumulate units instead
      (-> agg
          (update :cost + (* price units))
          (assoc :units 0))
      (assoc agg :units units))))

(defn fuel-cost [route]
  (let [waypoints (map (partial refresh! :waypoint) route)
        legs (partition 2 1 waypoints)]
    (reduce fuel-cost-reducer {:units 0 :cost 0} legs)))

(defn opportunities
  "Returns a ranked list of opportunities for a given LOOKUP.
  Opportunities come with `:rank` (float), `:cost` (integer),
  `:margin` (integer), `:distance` (integer, rounded), `:source` (good
  with `:waypointSymbol`), and `:target` (same as `:source`). If an
  optional SHIP is given the oppportunities will include the cost for
  travelling to the source waypoint from its current location and
  `:profit` (integer) will be given as the expexted net profit."
  ([lookup] (opportunities lookup nil))
  ([lookup ship]
   (sort-by
    (comp - :rank)
    (remove
     nil?
     (for [pos¹ lookup ;; place of purchase
	  pos² lookup] ;; place of sell
       (let [margin (- (:sellPrice pos²) (:purchasePrice pos¹))]
         (when (and (not= pos¹ pos²)
                    (= (:symbol pos¹) (:symbol pos²))
                    (pos? margin))
           (let [dist (distance (:waypointSymbol pos¹)
				(:waypointSymbol pos²))
                 cost (:cost (fuel-cost [(:waypointSymbol pos¹)
                                         (:waypointSymbol pos²)]))]
             (merge
              {:rank (rank margin dist)
               :cost cost
               :margin margin
               :distance dist
               :source pos¹
               :target pos²}
              (when ship
                (let [cost (:cost (fuel-cost [(-> ship :nav :waypointSymbol)
                                              (:waypointSymbol pos¹)
                                              (:waypointSymbol pos²)]))
                      profit (- (* margin (-> ship :cargo :capacity)) cost)]
                  {:cost cost
                   :profit profit})))))))))))

"genqref-lib.market"
