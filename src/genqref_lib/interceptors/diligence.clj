(ns genqref-lib.interceptors.diligence)

(def ^:private diligence-map
  "This maps hold all the response keys that we're currently using. This
  is used to detect unused keys by the diligence interceptor."
  {:purchase-ship [:agent :ship :transaction]})

(defn diligence
  ([report-fn] (diligence report-fn diligence-map))
  ([report-fn dmap]
   {:leave (fn [{:keys [response] {:keys [endpoint]} :request :as ctx}]
             (when-let [unused (not-empty (apply dissoc response (dmap endpoint)))]
               (report-fn "UNUSED BY" endpoint (prn-str unused)))
             ctx)}))
