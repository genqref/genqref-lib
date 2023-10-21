(ns genqref-lib.interceptors.log-for-humans
  (:require [clojure.string :as str]
            [genqref-lib.util :as util :refer [sym]]))

;; TODO read and render templates from yaml with io/resource yaml/read-str and comb/eval

(defmulti ->message (fn [{:keys [point endpoint]}]
                      (->> [point endpoint] (map name) (str/join "-") keyword)))

(defmethod ->message :default [_ _]
  nil)

(defn- sjoin [& args]
  (str/join " " args))

(defn- list-goods [goods]
  (->> goods
       (map (fn [{:keys [units tradeSymbol]}] (str units " " tradeSymbol)))
       (str/join ", ")))

;; TODO: provide helper fns, like `id`

;; TODO: keep multimethod approach for overrides

(defmethod ->message :before-purchase-ship [{:keys [shipyard ship-type]}]
  (sjoin "About to purchase" ship-type "at" (sym shipyard)))

(defmethod ->message :after-purchase-ship [{:keys [shipyard ship-type]}]
  (sjoin "Purchased" ship-type "at" (sym shipyard)))

(defmethod ->message :error-purchase-ship [{:keys [shipyard ship-type]}]
  (sjoin "Failed to purchase" ship-type "at" (sym shipyard)))

(defmethod ->message :before-orbit-ship [{:keys [ship]}]
  (sjoin "Ship" (sym ship) "is about to move to orbit"))

(defmethod ->message :after-orbit-ship [{:keys [ship nav]}]
  (sjoin "Ship" (sym ship) "moved to orbit"))

(defmethod ->message :error-orbit-ship [{:keys [ship]}]
  (sjoin "Ship" (sym ship) "failed to move to orbit"))

(defmethod ->message :before-ship-refine [{:keys [ship produce]}]
  (sjoin "Ship" (sym ship) "is about to refine" produce))

(defmethod ->message :after-ship-refine [{:keys [ship consumed produced cargo cooldown]}]
  (sjoin "Ship" (sym ship) "refined" (list-goods consumed) "into" (list-goods produced)))

(defmethod ->message :error-ship-refine [{:keys [ship produce]}]
  (sjoin "Ship" (sym ship) "failed to refine" produce))

(defmethod ->message :before-create-chart [{:keys [ship]}]
  (sjoin "Ship" (sym ship) "is about to chart a waypoint"))

(defmethod ->message :after-create-chart [{:keys [ship chart waypoint]}]
  (sjoin "Ship" (sym ship) "charted waypoint"))

(defmethod ->message :error-create-chart [{:keys [ship]}]
  (sjoin "Ship" (sym ship) "failed to chart waypoint"))

(defmethod ->message :before-dock-ship [{:keys [ship]}]
  (sjoin "Ship" (sym ship) "is about to dock"))

(defmethod ->message :after-dock-ship [{:keys [ship nav]}]
  (sjoin "Ship" (sym ship) "docked at" (:waypointSymbol nav)))

(defmethod ->message :error-dock-ship [{:keys [ship]}]
  (sjoin "Ship" (sym ship) "failed to dock"))

(defmethod ->message :before-create-survey []
  (sjoin ))

(defmethod ->message :after-create-survey []
  (sjoin ))

(defmethod ->message :error-create-survey []
  (sjoin ))

(defmethod ->message :before-extract-resources []
  (sjoin ))

(defmethod ->message :after-extract-resources []
  (sjoin ))

(defmethod ->message :error-extract-resources []
  (sjoin ))

(defmethod ->message :before-jettison []
  (sjoin ))

(defmethod ->message :after-jettison []
  (sjoin ))

(defmethod ->message :error-jettison []
  (sjoin ))

(defmethod ->message :before-jump-ship []
  (sjoin ))

(defmethod ->message :after-jump-ship []
  (sjoin ))

(defmethod ->message :error-jump-ship []
  (sjoin ))

(defmethod ->message :before-navigate-ship []
  (sjoin ))

(defmethod ->message :after-navigate-ship []
  (sjoin ))

(defmethod ->message :error-navigate-ship []
  (sjoin ))

(defmethod ->message :before-patch-ship-nav []
  (sjoin ))

(defmethod ->message :after-patch-ship-nav []
  (sjoin ))

(defmethod ->message :error-patch-ship-nav []
  (sjoin ))

(defmethod ->message :before-warp-ship []
  (sjoin ))

(defmethod ->message :after-warp-ship []
  (sjoin ))

(defmethod ->message :error-warp-ship []
  (sjoin ))

(defmethod ->message :before-sell-cargo []
  (sjoin ))

(defmethod ->message :after-sell-cargo []
  (sjoin ))

(defmethod ->message :error-sell-cargo []
  (sjoin ))

(defmethod ->message :before-create-ship-system-scan []
  (sjoin ))

(defmethod ->message :after-create-ship-system-scan []
  (sjoin ))

(defmethod ->message :error-create-ship-system-scan []
  (sjoin ))

(defmethod ->message :before-create-ship-waypoint-scan []
  (sjoin ))

(defmethod ->message :after-create-ship-waypoint-scan []
  (sjoin ))

(defmethod ->message :error-create-ship-waypoint-scan []
  (sjoin ))

(defmethod ->message :before-create-ship-ship-scan []
  (sjoin ))

(defmethod ->message :after-create-ship-ship-scan []
  (sjoin ))

(defmethod ->message :error-create-ship-ship-scan []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defmethod ->message :before- []
  (sjoin ))

(defmethod ->message :after- []
  (sjoin ))

(defmethod ->message :error- []
  (sjoin ))

(defn- combined [{:keys [request response]}]
  (-> request
      :params
      (merge response)
      (merge :endpoint (:endpoint request))))

(defn interceptor
  "An interceptor that logs what's happening in a human readable
  format."
  [{:keys [before-fn after-fn error-fn]
    :or {before-fn println
         after-fn println
         error-fn println}
    :as options}]
  {:enter (fn [{:keys [request] :as ctx}]
            (when before-fn
              (-> request
                  :params
                  (assoc :point :before
                         :endpoint (:endpoint request))
                  ->message
                  before-fn))
            ctx)
   :leave (fn [ctx]
            (when after-fn
              (-> ctx
                  combined
                  (assoc :point :after)
                  ->message
                  after-fn))
            ctx)
   :error (fn [{:keys [request] :as ctx}]
            (when error-fn
              (-> request
                  :params
                  ;; TODO: somehow add details from the error
                  (assoc :point :error
                         :endpoint (:endpoint request))
                  ->message
                  error-fn))
            ctx)})
