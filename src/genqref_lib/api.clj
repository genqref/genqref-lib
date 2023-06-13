(ns genqref-lib.api
  "- https://api.spacetraders.io/v2
   - https://docs.spacetraders.io/
   - https://spacetraders.stoplight.io/docs/spacetraders/
   - https://twitter.com/SpaceTradersAPI"
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [martian.core :as martian]
            [martian.httpkit :as martian-http]
            [duratom.core :refer [duratom]]
            [cheshire.core :as json]
            [taoensso.timbre :as timbre :refer [trace debug info warn error]]
            [org.httpkit.client :as http]
            [genqref-lib.time :as t]
            [genqref-lib.util :as util :refer [sym]]
            [genqref-lib.scheduler :refer [schedule!]]
            [slingshot.slingshot :refer [try+ throw+]])
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
      deref
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
               (:data (:body @(martian/response-for public-api :register options)))]
      (let [{:keys [resetDate]} (status)]
        (swap! tokens assoc resetDate token)
        (reset! state {:resetDate resetDate
                       :agent agent
                       :contracts {(keyword (:id contract)) contract}
                       :factions {(keyword (sym faction)) faction}
                       :ships {(keyword (sym ship)) ship}})
        (init!)
        (info "Successfully registered and reinitialized.")
        response))))

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

(def ^:dynamic *reset-fn*)

(defn init!
  ([] (init! nil))
  ([reset-fn]
   (alter-var-root #'*api* (constantly (api (token))))
   (alter-var-root #'*reset-fn* (constantly reset-fn))
   "done"))

#_(init!)

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
  (let [signature (second (re-find #"Survey ([0-9A-Z-]+) has" message))
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
           (-> response deref :body :data))
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
               ;; 4216 - Failed to purchase ship. Agent has insufficient funds.
               4216 (do)
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

(defn waypoints-of-system [system]
  (->> @state
       :waypoints
       vals
       (filter #(-> %
                    :systemSymbol
                    (= (sym system))))))

;;; hooks

(defonce ^:private hooks (atom {}))

(defn register-hooks [hook-map]
  (swap! hooks merge hook-map))

(defn regexify [x]
  (if (= (type x) java.util.regex.Pattern)
    x
    (re-pattern (name x))))

#_(regexify #"hello")
#_(regexify "hello")
#_(regexify :hello)

(defn- call-hooks [stage subject & arguments]
  ;;  stage is one of :updated, :before, :after, :failure
  (let [stage-subject (str (name stage) "-" (name subject))]
    (doseq [[pattern hook-fn] @hooks]
      (when (re-matches (regexify pattern) stage-subject)
        ;; first try it with all arguments
        (loop [args (concat [stage subject] arguments)]
          (when-not
              (try
                (apply hook-fn args)
                :success
                (catch clojure.lang.ArityException e
                  (if (nil? args)
                    (do
                      (error "Hook" stage-subject "was never called, due to too many arguments.")
                      :failure)
                    false)))
            ;; if it fails try it with one argument less
            (recur (butlast args))))))))

#_(call-hooks :after :jump "GENQREF-1" :asdf)

;;; GET (refresh, query the api and populate state)

(defmulti refresh (fn [entity _] entity))

;; TODO: in all refresh methods use util/deep-merge instead of assoc,
;; assoc-in or merge, to be less destructive of existing information

;;;; factions

(defmethod refresh :factions [_ _]
  (trace "Query factions")
  (let [factions (q :get-factions)]
    (swap! state update :factions
           util/deep-merge (util/index-by (comp keyword :symbol) factions))
    (call-hooks :updated :factions factions)
    factions))

(defmethod refresh :faction [_ faction]
  (trace "Query faction" (sym faction))
  (let [faction (q :get-faction {:factionSymbol (sym faction)})]
    (swap! state update-in [:factions (keyword (sym faction))]
           util/deep-merge faction)
    (call-hooks :updated :faction faction)
    faction))

;;;; fleet

(defmethod refresh :ships [_ _]
  (trace "Query ships")
  (let [ships (q :get-my-ships)]
    (swap! state update :ships
           util/deep-merge (util/index-by (comp keyword :symbol) ships))
    (call-hooks :updated :ships ships)
    ships))

(defmethod refresh :ship [_ ship]
  (trace "Query ship" (sym ship))
  (let [ship (q :get-my-ship {:shipSymbol (sym ship)})]
    (swap! state update-in [:ships (keyword (sym ship))]
           util/deep-merge ship)
    (call-hooks :updated :ship ship)
    ship))

#_(def ship (refresh! :ship "GENQREF-1"))

(defmethod refresh :cargo [_ ship]
  (trace "Query cargo for ship" (sym ship))
  (let [cargo (q :get-ship-cargo {:shipSymbol (sym ship)})]
    (swap! state update-in [:ships (keyword (sym ship)) :cargo]
           util/deep-merge cargo)
    (call-hooks :updated :cargo ship cargo)
    cargo))

(defmethod refresh :cooldown [_ ship]
  (trace "Query cooldown for ship" (sym ship))
  (let [cooldown (q :get-ship-cooldown {:shipSymbol (sym ship)})]
    (swap! state update-in [:ships (keyword (sym ship)) :cooldown]
           util/deep-merge cooldown)
    (call-hooks :updated :cooldown ship cooldown)
    cooldown))

(defmethod refresh :nav [_ ship]
  (trace "Query nav for ship" (sym ship))
  (let [nav (q :get-ship-nav {:shipSymbol (sym ship)})]
    (swap! state update-in [:ships (keyword (sym ship)) :nav]
           util/deep-merge nav)
    (call-hooks :updated :nav ship nav)
    nav))

;;;; contracts

(defmethod refresh :contracts [_ _]
  (trace "Query contracts")
  (let [contracts (q :get-contracts)]
    (swap! state update :contracts
           util/deep-merge (util/index-by (comp keyword :id) contracts))
    (call-hooks :updated :contracts contracts)
    contracts))

(defmethod refresh :contract [_ {:keys [id] :as contract}]
  (trace "Query contract" id)
  (let [contract (q :get-contract {:contractId id})]
    (swap! state update-in [:contracts (keyword id)]
           util/deep-merge contract)
    (call-hooks :updated :contract contract)
    contract))

;;;; systems

(defmethod refresh :systems [_ _]
  (trace "Query systems")
  (let [systems (q :get-systems)]
    (swap! state update :systems
           util/deep-merge (util/index-by (comp keyword :symbol) systems))
    (call-hooks :updated :systems systems)
    systems))

(defmethod refresh :system [_ system]
  (trace "Query system" (sym system))
  (let [system (q :get-system {:systemSymbol (sym system)})]
    (swap! state update-in [:systems (keyword (sym system))]
           util/deep-merge system)
    (call-hooks :updated :system system)
    system))

(defmethod refresh :waypoints [_ system]
  (trace "Query waypoints for system" (sym system))
  (let [waypoints (q :get-system-waypoints {:systemSymbol (sym system)})]
    (swap! state update :waypoints
           util/deep-merge (util/index-by (comp keyword :symbol) waypoints))
    (call-hooks :updated :waypoints system waypoints)
    waypoints))

(defmethod refresh :waypoint [_ waypoint]
  (trace "Query waypoint" (sym waypoint))
  (let [waypoint (q :get-waypoint {:systemSymbol (util/parse-system (sym waypoint))
                                   :waypointSymbol (sym waypoint)})]
    (swap! state update-in [:waypoints (keyword (sym waypoint))]
           util/deep-merge waypoint)
    (call-hooks :updated :waypoint waypoint)
    waypoint))

(defmethod refresh :market [_ waypoint]
  (trace "Query market at waypoint" (sym waypoint))
  (let [symbol (sym waypoint)
        market (q :get-market {:systemSymbol (util/parse-system symbol)
                                 :waypointSymbol symbol})]
    (swap! state update-in [:markets (keyword symbol)]
           util/deep-merge market)
    (call-hooks :updated :market waypoint market)
    market))

(defmethod refresh :shipyard [_ waypoint]
  (trace "Query shipyard at waypoint" (sym waypoint))
  (let [symbol (sym waypoint)
        shipyard (q :get-shipyard {:systemSymbol (util/parse-system symbol)
                                   :waypointSymbol symbol})]
    (swap! state update-in [:shipyards (keyword symbol)]
           util/deep-merge shipyard)
    (call-hooks :updated :shipyard waypoint shipyard)
    shipyard))

(defmethod refresh :jumpgate [_ waypoint]
  (trace "Query jump gate at waypoint" (sym waypoint))
  (let [{:keys [connectedSystems] :as jumpgate}
        (q :get-jump-gate {:systemSymbol (util/parse-system (sym waypoint))
                           :waypointSymbol (sym waypoint)})]
    (swap! state update-in [:jumpgates (keyword (sym waypoint))] util/deep-merge jumpgate)
    ;; also merge the minimal info we get about connected systems into
    ;; systems
    (->> connectedSystems
         (map #(dissoc % :distance))
         (util/index-by (comp keyword :symbol))
         (swap! state update :systems util/deep-merge))
    (call-hooks :updated :jumpgate waypoint jumpgate)
    jumpgate))

;;;; agents

(defmethod refresh :agent [_ _]
  (trace "Query agent")
  (let [agent (q :get-my-agent)]
    (swap! state update :agent
           util/deep-merge (q :get-my-agent))
    (call-hooks :updated :agent agent)
    agent))

;;; GET api

(defn refresh!
  ([entity] (refresh! entity nil))
  ([entity {:keys [symbol] :as emap}]
   (refresh entity emap)))

#_(def ship (refresh! :ship ship))

;;; POST/PATCH (actions)

;;;; fleet

(defn purchase-ship! [shipyard ship-type]
  (trace "Purchase ship" ship-type "from" (sym shipyard))
  (call-hooks :before :purchase-ship shipyard ship-type)
  (when-let [{:keys [agent ship transaction] :as response}
             (q :purchase-ship {:waypointSymbol (sym shipyard)
                                :shipType ship-type})]
    (swap! state #(-> %
                      (update :agent util/deep-merge agent)
                      (update :ships assoc-in [:ships (keyword (sym ship))] ship)
                      ;; TODO: maybe index transaction too
                      (update :transactions conj transaction)))
    (call-hooks :after :purchase-ship shipyard agent ship transaction)
    response))

(defn orbit! [ship]
  (let [symbol (sym ship)]
    (trace "Moving ship" symbol "to orbit")
    (call-hooks :before :orbit ship)
    (when-let [response (q :orbit-ship {:shipSymbol symbol})]
      ;; TODO: update state
      (call-hooks :after :orbit ship)
      response)))

#_(orbit! ship)

;; TODO: implement refine

(defn chart! [ship]
  (trace "Charting waypoint from ship" (sym ship))
  (call-hooks :before :chart ship)
  (when-let [{:keys [chart waypoint] :as response}
             (q :create-chart {:shipSymbol (sym ship)})]
    ;; TODO: update state
    (call-hooks :after :chart ship chart waypoint)
    response))

(defn dock! [ship]
  (trace "Docking ship" (sym ship))
  (call-hooks :before :dock ship)
  (when-let [response
             (q :dock-ship {:shipSymbol (sym ship)})]
    ;; TODO: update state
    (call-hooks :after :dock ship)
    response))

#_(dock! ship)

(defn survey!
  ([ship]
   (survey! ship {:on-cooldown #(debug "Ship" (sym ship) "completed cooldown after survey")}))
  ([ship options]
   (trace "Ship" (sym ship) "surveys")
   (call-hooks :before :survey ship options)
   (when-let [{:keys [surveys cooldown] :as response}
              (q :create-survey {:shipSymbol (sym ship)})]
     (swap! state assoc-in [:ships (keyword (sym ship)) :cooldown] cooldown)
     (doseq [{:keys [symbol] :as survey} surveys]
       (swap! state update-in [:surveys (keyword symbol)] conj survey))
     (when-let [f (:on-cooldown options)]
       (schedule! (:expiration cooldown) f))
     (call-hooks :after :survey ship options surveys cooldown)
     response)))

#_(survey! ship)

(defn extract!
  "A survey can be passed in options `{:survey ...}`"
  ([ship]
   (extract! ship {:on-cooldown #(debug "Ship" (sym ship) "completed cooldown after extract")}))
  ([ship options]
   (trace "Ship" (sym ship) "extracts resources")
   (call-hooks :before :extract ship options)
   (when-let [{:keys [cooldown] :as response}
              (q :extract-resources (-> (into {} (filter last options))
                                        (dissoc :on-cooldown)
                                        (assoc :shipSymbol (sym ship))))]
     ;; TODO: handle full response, update state
     (swap! state assoc-in [:ships (keyword (sym ship)) :cooldown] cooldown)
     (when-let [f (:on-cooldown options)]
       (schedule! (:expiration cooldown) f))
     (call-hooks :after :extract ship options cooldown)
     response)))

#_(extract! "GENQREF-5")

(defn jettison! [ship cargo units]
  (trace "Ship" (sym ship) "jettisons" units "units of" (sym cargo))
  (call-hooks :before :jettison ship cargo units)
  (when-let [response
             (q :jettison {:shipSymbol (sym ship)
                           :symbol (sym cargo)
                           :units units})]
    ;; TODO: update state
    (call-hooks :after :jettison ship cargo units)
    response))

#_(jettison! "GENQREF-5" "ICE_WATER" 4)

(defn jump!
  ([ship system] (jump! ship system
                        {:on-cooldown #(debug "Ship" (sym ship) "completed cooldown after jump")}))
  ([ship system options]
   (trace "Ship" (sym ship) "jumping to system" (sym system))
   (call-hooks :before :jump ship system)
   (when-let [{:keys [cooldown nav] :as response}
              (q :jump-ship {:shipSymbol (sym ship)
                             :systemSymbol (sym system)})]
     (swap! state assoc-in [:ships (keyword (sym ship)) :cooldown] cooldown)
     (swap! state assoc-in [:ships (keyword (sym ship)) :nav] nav)
     (when-let [f (:on-cooldown options)]
       (schedule! (:expiration cooldown) f))
     (call-hooks :after :jump ship system nav cooldown)
     response)))

#_(jump! ship "X1-PY76")

(defn navigate!
  ([ship destination] (navigate! ship destination
                                 {:on-arrival #(info "Ship" (sym ship) "has arrived")}))
  ([ship destination options]
   (let [waypoint-symbol (sym destination)]
     (trace "Ship" (sym ship) "navigates to" waypoint-symbol)
     (call-hooks :before :navigate ship destination options)
     (when-let [{{{:keys [arrival]} :route} :nav :as response}
                (q :navigate-ship {:shipSymbol (sym ship)
                                   :waypointSymbol waypoint-symbol})]
       ;; TODO: update state
       ;; (info "Ship" (sym ship) "in transit to" waypoint-symbol (str "(" (:type destination) ")") "arrival at" arrival)
       (when-let [f (:on-arrival options)]
         (schedule! arrival f))
       ;; TODO: add more details
       (call-hooks :after :navigate ship destination options)
       response))))

#_(navigate! ship asteroid-field)

(defn flight-mode! [ship mode]
  (let [mode (-> mode name str/upper-case)]
    (trace "Ship" (sym ship) "sets flight mode to" mode)
    (call-hooks :before :flight-mode ship mode)
    (when-let [response
               (q :patch-ship-nav {:shipSymbol (sym ship)
                                   :flightMode mode})]
      ;; TODO: update state
      (call-hooks :after :flight-mode ship mode)
      response)))

#_(flight-mode! ship "BURN")

(defn warp! [ship destination]
  (trace "Ship" (sym ship) "warping to" (sym destination))
  (call-hooks :before :warp ship destination)
  (when-let [response
             ;; TODO: implement
             (q :warp-ship {})]
    ;; TODO: update state
    (call-hooks :after :warp ship destination response)
    response))

(defn sell-cargo! [ship trade-symbol units]
  (trace "Ship" (sym ship) "selling" units "units of" trade-symbol)
  (call-hooks :before :sell-cargo ship trade-symbol units)
  (when-let [response
             (q :sell-cargo {:shipSymbol (sym ship)
                             :symbol trade-symbol
                             :units units})]
    ;; TODO: update state
    (call-hooks :after :sell-cargo ship trade-symbol units response)
    response))

;; TODO: add option :on-cooldown
(defn scan-systems! [ship]
  (trace "Ship" (sym ship) "scans systems")
  (call-hooks :before :scan-systems ship)
  (when-let [{:keys [cooldown systems] :as response}
             (q :create-ship-system-scan {:shipSymbol (sym ship)})]
    (swap! state assoc-in [:ships (keyword (sym ship)) :cooldown] cooldown)
    (->> systems
         (map #(dissoc % :distance))
         (util/index-by (comp keyword :symbol))
         (swap! state update :systems util/deep-merge))
    ;; TODO: maybe use distance, but maybe not because it can easily be calculated
    (call-hooks :after :scan-systems ship systems cooldown)
    response))

#_(scan-systems! ship)

;; TODO: add option :on-cooldown
(defn scan-waypoints! [ship]
  (trace "Ship" (sym ship) "scans waypoints")
  (call-hooks :before :scan-waypoints ship)
  (when-let [{:keys [cooldown waypoints] :as response}
             (q :create-ship-waypoint-scan {:shipSymbol (sym ship)})]
    (swap! state assoc-in [:ships (keyword (sym ship)) :cooldown] cooldown)
    (->> waypoints
         (util/index-by (comp keyword :symbol))
         (swap! state update :waypoints util/deep-merge))
    (call-hooks :after :scan-waypoints ship waypoints cooldown)
    response))

#_(scan-waypoints! ship)

;; TODO: add option :on-cooldown
(defn scan-ships! [ship]
  (trace "Ship" (sym ship) "scans ships")
  (call-hooks :before :scan-ships ship)
  (when-let [response
             (q :create-ship-ship-scan {:shipSymbol (sym ship)})]
    ;; TODO: update state
    (call-hooks :after :scan-ships ship response)
    response))

#_(scan-ships! ship)

(defn refuel! [ship]
  (trace "Ship" (sym ship) "refuels")
  (call-hooks :before :refuel ship)
  (when-let [response
             (q :refuel-ship {:shipSymbol (sym ship)})]
    ;; TODO: update state
    (call-hooks :after :refuel ship response)
    response))

#_(refuel! ship)

(defn purchase-cargo! [ship trade units]
  (trace "Ship" (sym ship) "purchases" units "units of" trade)
  (call-hooks :before :purchase-cargo ship trade units)
  (when-let [response
             (q :purchase-cargo {:shipSymbol (sym ship)
                                 :symbol trade
                                 :units units})]
    ;; TODO: update state
    ;; (info "Ship" (sym ship) "purchased" units "units of" trade "for" (-> response :transaction :totalPrice))
    (call-hooks :after :purchase-cargo ship trade units response)
    response))

#_(purchase-cargo! ship "ANTIMATTER" 1)

;; NOTE: this requires the swagger spec to be fixed, the query
;; parameter need to be called `shipSymbol2`
(defn transfer! [from to trade units]
  (trace "Ship" (sym from) "transfers" units "of" trade "to ship" (sym to))
  (call-hooks :before :transfer from to trade units)
  (when-let [{:keys [cargo] :as response}
             (q :transfer-cargo {:shipSymbol2 (sym from)
                                 :shipSymbol (sym to)
                                 :tradeSymbol trade
                                 :units units})]
    ;; TODO: update state
    ;; (info "Ship" (sym from) "transfered" units "of" trade "to ship" (sym to))
    (call-hooks :after :transfer from to trade units cargo response)
    response))

;; TODO: does this work?
(defn negotiate-contract! [ship]
  (trace "Ship" (sym ship) "negotiates a contract")
  (call-hooks :before :negotiate-contract ship)
  (when-let [{:keys [contract] :as response}
             (q :negotiate-contract {:shipSymbol (sym ship)})]
    ;; TODO: update state
    (call-hooks :before :negotiate-contract ship contract response)
    response))

;;;; contracts

(defn deliver-contract! [ship contract trade units]
  (let [id (or (:id contract) contract)]
    (trace "Ship" (sym ship) "delivers" units "units of" trade "for contract" id)
    (call-hooks :before :deliver-contract ship contract trade units)
    (when-let [response (q :deliver-contract {:contractId id
                                              :shipSymbol (sym ship)
                                              :tradeSymbol trade
                                              :units units})]
      ;; TODO: update state
      (call-hooks :after :deliver-contract ship contract trade units response)
      response)))

(defn accept-contract! [contract]
  (let [id (or (:id contract) contract)]
    (trace "Accepting contract" id)
    (call-hooks :before :accept-contract contract)
    (when-let [{:keys [contract agent] :as response}
               (q :accept-contract {:contractId id})]
      ;; TODO: update state
      (call-hooks :after :accept-contract contract agent)
      response)))

(defn fulfill-contract! [contract]
  (let [id (or (:id contract) contract)]
    (trace "Fullfilling contract" id)
    (call-hooks :before :fulfill-contract contract)
    (when-let [{:keys [contract agent] :as response}
               (q :fulfill-contract {:contractId id})]
      ;; TODO: update state
      (call-hooks :after :fulfill-contract contract agent)
      response)))

#_(fulfill-contract! (-> @state :contracts vals first))
