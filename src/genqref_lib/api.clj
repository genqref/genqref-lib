(ns genqref-lib.api
  "- https://api.spacetraders.io/v2
   - https://docs.spacetraders.io/
   - https://spacetraders.stoplight.io/docs/spacetraders/
   - https://twitter.com/SpaceTradersAPI"
  (:require [martian.core :as martian]
            [martian.clj-http :as martian-http]
            [clojure.string :as str]
            [clojure.set :as set]
            [duratom.core :refer [duratom]]
            [cheshire.core :as json]
            [taoensso.timbre :as timbre :refer [trace debug info warn error]]
            [clj-http.client :as http]
            [genqref-lib.time :as t]
            [genqref-lib.util :as util :refer [sym]]
            [genqref-lib.scheduler :refer [schedule!]])
  (:use [slingshot.slingshot :only [try+ throw+]])
  (:gen-class))

;;; public api

(def swagger-spec "spacetraders.v2.json")

(def public-api (martian-http/bootstrap-openapi swagger-spec))

(def tokens (duratom :local-file
                     :file-path "tokens.edn"
                     :init {}))

(defn status
  "Returns the status of Space Traders API."
  []
  (-> "https://api.spacetraders.io/v2"
      http/get
      :body
      (json/parse-string true)))

#_(status)

(defn token
  "Returns the token for the current run."
  []
  (get @tokens (:resetDate (status))))

#_(token)

(declare state)
(declare init!)

(defn register! [options]
  "Registers a new agent and stores its token. This will fail if there
  already is a token for the current run. On success this will reset
  state (the local cache) and populate it with the data received from
  registration."
  (if (token)
    (warn "There is already a token for the current run. Abort.")
    (when-let [{:keys [token agent contract faction ship] :as response}
               (:data (:body (martian/response-for public-api :register options)))]
      (swap! tokens assoc :resetDate token)
      (reset! state {:agent agent
                     :contracts {(keyword (:id contract)) contract}
                     :factions {(keyword (sym faction)) faction}
                     :ships {(keyword (sym ship)) ship}})
      (init!)
      (info "Successfully registered and reinitialized.")
      response)))

#_(register! {:faction "COSMIC"
              :symbol "CALLSIGN"
              :email "EMAIL"})

;;; main api setup

(defn- add-authentication-header [token]
  {:name ::add-authentication-header
   :enter (fn [ctx]
            (assoc-in ctx [:request :headers "Authorization"] (str "Bearer " token)))})

(defn api [token]
  (martian-http/bootstrap-openapi swagger-spec
                                  {:interceptors (concat martian/default-interceptors
                                                         [(add-authentication-header token)]
                                                         martian-http/default-interceptors)}))

(def ^:dynamic *api*)

(defn init! []
  (alter-var-root #'*api* (constantly (api (token)))))

;;; error handling

;; NOTE: the whole error handling is a mess and must be refactored
;; TODO: use a interceptor pattern for error recovery and active rate
;; limiting on client side

(declare dock!)
(declare orbit!)
(declare refuel!)
(declare state)

;; TODO: move to util
(defn parse-waypoint [signature]
  ;; "X1-VS75-67965Z-97E75E"
  (->> (str/split signature #"-")
       butlast
       (str/join "-")))

(defn- remove-survey
  "As error 4224 doesn't come with machine readble metadata we need to
  parse the signature of the exhausted survey from the error
  message. "
  [message]
  ;; "Ship extract failed. Survey X1-VS75-67965Z-97E75E has been exhausted."
  (let [signature (re-find #"[0-9A-Z-]{21}" message)
        waypoint (parse-waypoint signature)]
    (swap! state update-in [:surveys (keyword waypoint)]
           (partial remove #(-> % :signature (= signature))))
    ;; TODO: use only keywords everywhere
    (swap! state update-in [:surveys waypoint]
           (partial remove #(-> % :signature (= signature))))
    (info "Removed survey" signature "with depleted deposits")))

;; TODO: handle network issues with retrying
;; TODO: catch registration required after reset
(defn- q* [args]
  (let [result
        (try+
         (let [response (apply (partial martian/response-for *api*) args)]
           (-> response :body :data))
         (catch [:status 400] {:keys [request-time headers body] :as e}
           (let [{{:keys [code message]} :error} (json/parse-string body true)]
             (info "Error" code (str "(400)") message)
             ;; some error codes we can recover from
             (case code
               ;; Failed to execute jump. System ... is out of range.
               ;; ?
               ;;
               ;; 400 - The jump drive / gate has a range of 500
               ;; units. Ship transfer failed. Both ships must be
               ;; docked or both ships must be in orbit.
               400 (do)
               ;; 4202 - Navigate request failed. Destination
               ;; X1-VS75-93799Z is outside the X1-XN54 system.
               4202 (do)
               ;; 4203 - Navigate request failed. Ship GENQREF-3
               ;; requires 56 more fuel for navigation.
               4203 (do
                      (dock! (:shipSymbol (second args)))
                      (refuel! (:shipSymbol (second args)))
                      (orbit! (:shipSymbol (second args)))
                      {:retry 0})
               ;; 4204 - Navigate request failed. Ship GENQREF-3 is
               ;; currently located at the destination.
               4204 (do)
               ;; 4205 - Ship extract failed. PLANET is not a valid
               ;; location for extracting minerals.
               4205 (throw e)
               ;; 4208 - Failed to execute jump. Ship cannot execute
               ;; jump to the system it is currently located in.
               4208 (do)
               ;; 4214 - Ship action failed. Ship is not currently in
               ;; orbit at ...
               4214 (do)
               ;; 4217 - Failed to update ship cargo. Cannot add 3
               ;; unit(s) to ship cargo. Exceeds max limit of 30.
               4217 (do)
               ;; 4219 - Ship is not at the delivery destination.
               ;; Expected ..., but ship is at ...
               4219 (do)
               ;; 4221 - Ship survey failed. Target signature is no
               ;; longer in range or valid.
               4221 (do)
               ;; 4222 - Ship survey failed. Ship GENQREF-1 is not at
               ;; a valid location for surveying.
               4222 (do)
               ;; 4228 - Failed to update ship cargo. Ship is at
               ;; maximum capacity and has 0 units of available space.
               4228 (do)
               ;; 4230 - Waypoint already charted: X1-PZ25-44312A
               4230 (do)
               ;; 4236 - Ship is currently in-transit from ... to ...
               ;; and arrives in ... seconds.
               4236 (do (orbit! (:shipSymbol (second args))) {:retry 0})
               ;; 4244 - Ship action failed. Ship is not currently
               ;; docked at X1-VS75-67965Z.
               4244 (do)
               ;; 4509 - Contract delivery terms for IRON_ORE have been met.
               4509 (do) ;; TODO: fullfill contract
               ;; 4510 - Failed to update ship cargo. Ship ... cargo
               ;; does not contain ...
               4510 (do)
               ;; 4601 - Market purchase failed. Trade good FUEL is
               ;; not available at X1-VX95-71385D.
               4601 (do)
               ;; 4602 - Market sell failed. Trade good IRON_ORE is
               ;; not available at X1-VS75-70500X.
               4602 (do)
               ;; default
               (let [file (str "error_" code "_" (t/now) ".edn")]
                 (spit file (prn-str e))
                 (info "Full report written to" file)))))
         (catch [:status 409] {:keys [request-time headers body] :as e}
           (let [{{:keys [code message data]} :error} (json/parse-string body true)]
             (info "Error" code (str "(409)") message)
             (prn data)
             (case code
               ;; Ship GENQREF-2 is currently locked while processing
               ;; another request. Concurrent requests to control a
               ;; ship are not supported.
               409 (do)
               4000 data
               ;; 4224 - Ship extract failed. Survey X1-VS75-67965Z-97E75E has been exhausted.
               4224 (remove-survey message)
               ;; default
               (let [file (str "error_" code "_" (t/now) ".edn")]
                 (spit file (prn-str e))
                 (info "Full report written to" file)))))
         ;; exceeded the rate limit
         (catch [:status 429] {:keys [request-time headers body]}
           (let [{:keys [error]} (json/parse-string body true)
                 timeout (-> error :data :retryAfter)]
             ;; (prn error)
             ;; (info (-> error :data))
             ;; (info (-> error :data :retryAfter))
             (info ">>> We're going too fast (429), timeout for" timeout "seconds. <<<")
             {:retry timeout}))
         (catch [:status 502] _
           ;; The server encountered a temporary error and could not
           ;; complete your request. Please try again in 30 seconds.
           (info ">>> Encountered a server error (502). Retry in 30s, as advised. <<<")
           {:retry 30}))]
    (if (contains? result :retry)
      (do
        (Thread/sleep (* 1000 (or (:retry result) 5)))
        (recur args))
      result)))

(defn q [& args]
  (q* args))

;;; state (local cache)

(def state (duratom :local-file
                    :file-path "state.json"
                    :init {:genqref "rule"}
                    ;; TODO: use util/json-rw instead
                    :rw {:read (comp #(json/parse-string % true) slurp)
                         :write #(spit %1 (json/generate-string %2))}))

;;; operations on state

(defn ship-by-name [name]
  (-> @state :ships (get name)))

(defn waypoints-with-traits
  ([traits]
   (waypoints-with-traits traits (-> @state :waypoints vals)))
  ([traits waypoints]
   (filter #(->> %
                 :traits
                 (map :symbol)
                 set
                 (set/difference (set traits))
                 empty?)
           waypoints)))

(defn waypoints-of-type
  ([type]
   (waypoints-of-type type (-> @state :waypoints vals)))
  ([type waypoints]
   (->> waypoints (filter #(-> % :type (= type))))))

(defn ships-with-role [role]
  (->> @state
       :ships
       vals
       (filter #(-> %
                    :registration
                    :role
                    (= role)))))

;;; GET (refresh, query the api and populate state)

(defmulti refresh (fn [entity _] entity))

;; TODO: in all refresh methods use util/deep-merge instead of assoc,
;; assoc-in or merge, to be less destructive of existing information

;;;; factions

(defmethod refresh :factions [_ _]
  (trace "Query factions")
  (:factions (swap! state assoc :factions
                    (util/index-by (comp keyword :symbol) (q :get-factions)))))

(defmethod refresh :faction [_ faction]
  (trace "Query faction" (sym faction))
  (let [response (q :get-faction {:factionSymbol (sym faction)})]
    (swap! state assoc-in [:factions (keyword (sym faction))] response)
    response))

;;;; fleet

(defmethod refresh :ships [_ _]
  (trace "Query ships")
  (:ships (swap! state assoc :ships
                 (util/index-by (comp keyword :symbol) (q :get-my-ships)))))

(defmethod refresh :ship [_ ship]
  (trace "Query ship" (sym ship))
  (get-in (swap! state assoc-in [:ships (keyword (sym ship))]
                 (q :get-my-ship {:shipSymbol (sym ship)}))
          [:ships (keyword (sym ship))]))

(defmethod refresh :cargo [_ ship]
  (trace "Query cargo for ship" (sym ship))
  (let [response (q :get-ship-cargo {:shipSymbol (sym ship)})]
    (swap! state assoc-in [:ships (keyword (sym ship)) :cargo] response)
    response))

(defmethod refresh :cooldown [_ ship]
  (trace "Query cooldown for ship" (sym ship))
  (let [response (q :get-ship-cooldown {:shipSymbol (sym ship)})]
    (swap! state assoc-in [:ships (keyword (sym ship)) :cooldown] response)
    response))

(defmethod refresh :nav [_ ship]
  (trace "Query nav for ship" (sym ship))
  (let [response (q :get-ship-nav {:shipSymbol (sym ship)})]
    (swap! state assoc-in [:ships (keyword (sym ship)) :nav] response)
    response))

;;;; contracts

(defmethod refresh :contracts [_ _]
  (trace "Query contracts")
  (:contracts (swap! state assoc :contracts (util/index-by (comp keyword :id)
                                                           (q :get-contracts)))))

(defmethod refresh :contract [_ {:keys [id] :as contract}]
  (trace "Query contract" id)
  (let [response (q :get-contract {:contractId id})]
    (swap! state assoc-in [:contracts (keyword id)] response)
    response))

;;;; systems

(defmethod refresh :systems [_ _]
  (trace "Query systems")
  (:systems (swap! state update :systems merge
                   (util/index-by (comp keyword :symbol) (q :get-systems)))))

(defmethod refresh :system [_ system]
  (trace "Query system" (sym system))
  (let [response (q :get-system {:systemSymbol (sym system)})]
    (swap! state assoc-in [:systems (keyword (sym system))] response)
    response))

(defmethod refresh :waypoints [_ system]
  (trace "Query waypoints for system" (sym system))
  (let [response (q :get-system-waypoints {:systemSymbol (sym system)})]
    (swap! state update :waypoints merge (util/index-by (comp keyword :symbol) response))
    response))

(defmethod refresh :waypoint [_ waypoint]
  (trace "Query waypoint" (sym waypoint))
  (let [response (q :get-waypoint {:systemSymbol (util/parse-system (sym waypoint))
                                   :waypointSymbol (sym waypoint)})]
    (swap! state update-in [:waypoints (keyword (sym waypoint))] merge response)
    response))

(defmethod refresh :market [_ waypoint]
  (trace "Query market at waypoint" (sym waypoint))
  (let [symbol (sym waypoint)
        response (q :get-market {:systemSymbol (util/parse-system symbol)
                                 :waypointSymbol symbol})]
    (swap! state assoc-in [:markets (keyword symbol)] response)
    response))

(defmethod refresh :shipyard [_ waypoint]
  (trace "Query shipyard at waypoint" (sym waypoint))
  (let [symbol (sym waypoint)
        response (q :get-shipyard {:systemSymbol (util/parse-system symbol)
                                   :waypointSymbol symbol})]
    (swap! state assoc-in [:shipyards (keyword symbol)] response)
    response))

(defmethod refresh :jumpgate [_ waypoint]
  (trace "Query jump gate at waypoint" (sym waypoint))
  (let [{:keys [connectedSystems] :as response}
        (q :get-jump-gate {:systemSymbol (util/parse-system (sym waypoint))
                           :waypointSymbol (sym waypoint)})]
    (swap! state update-in [:jumpgates (keyword (sym waypoint))] merge response)
    ;; also merge the minimal info we get about connected systems into
    ;; systems
    (->> connectedSystems
         (map #(dissoc % :distance))
         (util/index-by (comp keyword :symbol))
         (swap! state update :systems util/deep-merge))
    response))

;;;; agents

(defmethod refresh :agent [_ _]
  (:agent (swap! state assoc :agent
                 (q :get-my-agent))))

;;; GET api

(defn refresh!
  ([entity] (refresh! entity nil))
  ([entity {:keys [symbol] :as emap}]
   (refresh entity emap)))

#_(def ship (refresh! :ship ship))

;;; POST/PATCH (actions)

;; TODO: use `(when-let [response (q ...` consistently

;;;; fleet

(defn purchase-ship! [shipyard ship-type]
  (trace "Purchase ship" ship-type "from" (sym shipyard))
  (when-let [{:keys [agent ship transaction] :as response}
             (q :purchase-ship {:waypointSymbol (sym shipyard)
                                :shipType ship-type})]
    (swap! state #(-> %
                      (update :agent util/deep-merge agent)
                      (update :ships assoc-in [:ships (keyword (sym ship))] ship)
                      ;; TODO: maybe index transaction too
                      (update :transactions conj transaction)))
    response))

(defn orbit! [ship]
  (let [symbol (sym ship)]
    (trace "Moving ship" symbol "to orbit")
    (when-let [response (q :orbit-ship {:shipSymbol symbol})]
      ;; TODO: update state
      response)))

#_(orbit! ship)

;; TODO: implement refine

(defn chart! [ship]
  (trace "Charting waypoint from ship" (sym ship))
  (when-let [{:keys [chart waypoint] :as response}
             (q :create-chart {:shipSymbol (sym ship)})]
    ;; TODO: update state
    response))

(defn dock! [ship]
  (trace "Docking ship" (sym ship))
  (when-let [response
             (q :dock-ship {:shipSymbol (sym ship)})]
    ;; TODO: update state
    response))

#_(dock! ship)

(defn survey!
  ([ship]
   (survey! ship {:on-cooldown #(debug "Ship" (sym ship) "completed cooldown after survey")}))
  ([ship options]
   (trace "Ship" (sym ship) "surveys")
   (when-let [{:keys [surveys cooldown] :as response}
              (q :create-survey {:shipSymbol (sym ship)})]
     (swap! state assoc-in [:ships (keyword (sym ship)) :cooldown] cooldown)
     (doseq [{:keys [symbol] :as survey} surveys]
       (swap! state update-in [:surveys (keyword symbol)] conj survey))
     (when-let [f (:on-cooldown options)]
       (schedule! (:expiration cooldown) f))
     response)))

#_(survey! ship)

(defn extract!
  "A survey can be passed in options `{:survey ...}`"
  ([ship]
   (extract! ship {:on-cooldown #(debug "Ship" (sym ship) "completed cooldown after extract")}))
  ([ship options]
   (trace "Ship" (sym ship) "extracts resources")
   (when-let [{:keys [cooldown] :as response}
              (q :extract-resources (-> (into {} (filter last options))
                                        (dissoc :on-cooldown)
                                        (assoc :shipSymbol (sym ship))))]
     ;; TODO: handle full response, update state
     (swap! state assoc-in [:ships (keyword (sym ship)) :cooldown] cooldown)
     (when-let [f (:on-cooldown options)]
       (schedule! (:expiration cooldown) f))
     response)))

#_(extract! "GENQREF-5")

(defn jettison! [ship cargo units]
  (trace "Ship" (sym ship) "jettisons" units "units of" (sym cargo))
  (when-let [response
             (q :jettison {:shipSymbol (sym ship)
                           :symbol (sym cargo)
                           :units units})]
    ;; TODO: update state
    response))

#_(jettison! "GENQREF-5" "ICE_WATER" 4)

(defn jump!
  ([ship system] (jump! ship system
                        {:on-cooldown #(debug "Ship" (sym ship) "completed cooldown after jump")}))
  ([ship system options]
   (trace "Ship" (sym ship) "jumping to system" (sym system))
   (when-let [{:keys [cooldown nav] :as response}
              (q :jump-ship {:shipSymbol (sym ship)
                             :systemSymbol (sym system)})]
     (swap! state assoc-in [:ships (keyword (sym ship)) :cooldown] cooldown)
     (swap! state assoc-in [:ships (keyword (sym ship)) :nav] nav)
     (when-let [f (:on-cooldown options)]
       (schedule! (:expiration cooldown) f))
     response)))

#_(jump! ship "X1-PY76")

(defn navigate!
  ([ship destination] (navigate! ship destination
                                 {:on-arrival #(info "Ship" (sym ship) "has arrived")}))
  ([ship destination options]
   (let [waypoint-symbol (sym destination)]
     (trace "Ship" (sym ship) "navigates to" waypoint-symbol)
     (when-let [{{{:keys [arrival]} :route} :nav :as response}
                (q :navigate-ship {:shipSymbol (sym ship)
                                   :waypointSymbol waypoint-symbol})]
       ;; TODO: update state
       ;; (info "Ship" (sym ship) "in transit to" waypoint-symbol (str "(" (:type destination) ")") "arrival at" arrival)
       (when-let [f (:on-arrival options)]
         (schedule! arrival f))
       response))))

#_(navigate! ship asteroid-field)

(defn flight-mode! [ship mode]
  (let [mode (-> mode name str/upper-case)]
    (trace "Ship" (sym ship) "sets flight mode to" mode)
    (when-let [response
               (q :patch-ship-nav {:shipSymbol (sym ship)
                                   :flightMode mode})]
      ;; TODO: update state
      response)))

#_(flight-mode! ship "BURN")

;; TODO: implement warp-ship

;; TODO: implement sell-cargo

;; TODO: add option :on-cooldown
(defn scan-systems! [ship]
  (trace "Ship" (sym ship) "scans systems")
  (when-let [{:keys [cooldown systems] :as response}
             (q :create-ship-system-scan {:shipSymbol (sym ship)})]
    (swap! state assoc-in [:ships (keyword (sym ship)) :cooldown] cooldown)
    (->> systems
         (map #(dissoc % :distance))
         (util/index-by (comp keyword :symbol))
         (swap! state update :systems util/deep-merge))
    ;; TODO: maybe use distance, but maybe not because it can easily be calculated
    response))

#_(scan-systems! ship)

;; TODO: add option :on-cooldown
(defn scan-waypoints! [ship]
  (trace "Ship" (sym ship) "scans waypoints")
  (when-let [{:keys [cooldown waypoints] :as response}
             (q :create-ship-waypoint-scan {:shipSymbol (sym ship)})]
    (swap! state assoc-in [:ships (keyword (sym ship)) :cooldown] cooldown)
    (->> waypoints
         (util/index-by (comp keyword :symbol))
         (swap! state update :waypoints util/deep-merge))
    response))

#_(scan-waypoints! ship)

;; TODO: add option :on-cooldown
(defn scan-ships! [ship]
  (trace "Ship" (sym ship) "scans ships")
  (when-let [response
             (q :create-ship-ship-scan {:shipSymbol (sym ship)})]
    ;; TODO: update state
    response))

#_(scan-ships! ship)

(defn refuel! [ship]
  (info "Ship" (sym ship) "refuels")
  (when-let [response
             (q :refuel-ship {:shipSymbol (sym ship)})]
    ;; TODO: update state
    response))

#_(refuel! ship)

(defn purchase-cargo! [ship trade units]
  (trace "Ship" (sym ship) "purchases" units "units of" trade)
  (when-let [response
             (q :purchase-cargo {:shipSymbol (sym ship)
                                 :symbol trade
                                 :units units})]
    ;; TODO: update state
    ;; (info "Ship" (sym ship) "purchased" units "units of" trade "for" (-> response :transaction :totalPrice))
    response))

#_(purchase-cargo! ship "ANTIMATTER" 1)

;; NOTE: this requires the swagger spec to be fixed, the query
;; parameter need to be called `shipSymbol2`
(defn transfer! [from to trade units]
  (trace "Ship" (sym from) "transfers" units "of" trade "to ship" (sym to))
  (when-let [{:keys [cargo] :as response}
             (q :transfer-cargo {:shipSymbol2 (sym from)
                                 :shipSymbol (sym to)
                                 :tradeSymbol trade
                                 :units units})]
    ;; TODO: update state
    ;; (info "Ship" (sym from) "transfered" units "of" trade "to ship" (sym to))
    response))

;; TODO: does this work?
(defn negotiate-contract! [ship]
  (trace "Ship" (sym ship) "negotiates a contract")
  (when-let [{:keys [contract] :as response}
             (q :negotiate-contract {:shipSymbol (sym ship)})]
    ;; TODO: update state
    response))

;;;; contracts

(defn deliver-contract! [ship contract trade units]
  (let [id (or (:id contract) contract)]
    (trace "Ship" (sym ship) "delivers" units "units of" trade "for contract" id)
    (when-let [response (q :deliver-contract {:contractId id
                                              :shipSymbol (sym ship)
                                              :tradeSymbol trade
                                              :units units})]
      ;; TODO: update state
      response)))

(defn accept-contract! [contract]
  (let [id (or (:id contract) contract)]
    (trace "Accepting contract" id)
    (when-let [{:keys [contract agent] :as response}
               (q :accept-contract {:contractId id})]
      ;; TODO: update state
      response)))

(defn fulfill-contract! [contract]
  (let [id (or (:id contract) contract)]
    (trace "Fullfilling contract" id)
    (when-let [{:keys [contract agent] :as response}
               (q :fulfill-contract {:contractId id})]
      ;; TODO: update state
      response)))

#_(fulfill-contract! (-> @state :contracts vals first))
