;; * Namespace

(ns genqref-lib.api
  "- https://api.spacetraders.io/v2
   - https://docs.spacetraders.io/
   - https://spacetraders.stoplight.io/docs/spacetraders/
   - https://twitter.com/SpaceTradersAPI
   - https://tradiverse.github.io/agent-stats/
   Hourly resets!
   - https://st-universe.se1.serial-experiments.com/v2/"
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.core.async :refer [>!! <! go go-loop chan]]
            [martian.core :as martian]
            [martian.httpkit :as martian-http]
            [duratom.core :refer [duratom]]
            [cheshire.core :as json]
            [taoensso.timbre :as timbre :refer [trace debug info warn error fatal]]
            [org.httpkit.client :as http]
            [genqref-lib.time :as t]
            [genqref-lib.util :as util :refer [sym]]
            [genqref-lib.algo.tsp :as tsp]
            [genqref-lib.scheduler :refer [schedule!]]
            [slingshot.slingshot :refer [try+ throw+]]
            [genqref-lib.throttling :as throttling])
  (:gen-class))

;; * Decalarations

(declare dock!)
(declare orbit!)
(declare refuel!)
(declare state)
(declare refresh!)
(declare query!)

(declare ^:dynamic *reset-fn*)

(def swagger-spec "spacetraders.v2.json")
;; TODO: make it work with the bundled spec
;; (def swagger-spec (io/resource "spacetraders.v2.json"))

;; * Public API

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
  [callsign]
  (get @tokens [callsign (:resetDate (status))]))

#_(token)

(declare state)
(declare init!)

(defn register! [options]
  "Registers a new agent and stores its token. This will fail if there
  already is a token for the current run. On success this will reset
  state (the local cache) and populate it with the data received from
  registration."
  (let [callsign (:symbol options)]
    (if (token callsign)
      (warn "Abort. There is already a token for callsign" callsign "(for this reset)")
      (when-let [{:keys [token agent contract faction ship] :as response}
                 (:data (:body @(martian/response-for public-api :register options)))]
        ;; diligence
        (when-let [unused (not-empty (dissoc response :token :agent :contract :faction :ship))]
          (warn "UNUSED BY register!" (prn-str unused)))
        ;; update tokens and state
        (let [{:keys [resetDate]} (status)]
          (swap! tokens assoc [callsign resetDate] token)
          (reset! state {:token-key [callsign resetDate]
                         :agent agent
                         :contracts {(keyword (:id contract)) contract}
                         :factions {(keyword (sym faction)) faction}
                         :ships {(keyword (sym ship)) ship}})
          ;; initialize api
          (init! callsign)
          (info "Successfully registered and reinitialized.")
          response)))))

#_(register! {:faction "COSMIC"
              :symbol "CALLSIGN"
              :email "EMAIL"})

;; * Main API Setup
;; ** Authorized API

(defn- add-authentication-header [token]
  {:name ::add-authentication-header
   :enter (fn [ctx]
            (assoc-in ctx [:request :headers "Authorization"] (str "Bearer " token)))})

(defn api [token]
  (debug "initializing api")
  (martian-http/bootstrap-openapi swagger-spec
                                  {:interceptors (concat martian/default-interceptors
                                                         [(add-authentication-header token)]
                                                         martian-http/default-interceptors)}))

(def ^:dynamic *api*)

(def ^:dynamic *reset-fn*)

(defn init!
  ([callsign] (init! callsign nil))
  ([callsign reset-fn]
   (alter-var-root #'*api* (constantly (api (token callsign))))
   (debug "initialized" (-> *api* :handlers count) "handlers")
   (alter-var-root #'*reset-fn* (constantly reset-fn))
   "done"))

#_(init!)

;; ** Error Handling

;; NOTE: the whole error handling is a mess and must be refactored
;; TODO: use a interceptor pattern (sieppari) for error recovery

;; TODO: move to util, maybe rename to waypoint
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
  (error message)
  (let [signature (second (re-find #"Survey ([0-9A-Z-]+) has" message))
        waypoint (parse-waypoint signature)]
    (swap! state update-in [:surveys (keyword waypoint)]
           (partial remove #(-> % :signature (= signature))))
    (swap! state update-in [:surveys waypoint]
           (partial remove #(-> % :signature (= signature))))
    (info "Removed survey" signature "with depleted deposits")))

(defn- remove-survey* [survey]
  (let [signature (:signature survey)
        waypoint (parse-waypoint signature)]
    (swap! state update-in [:surveys (keyword waypoint)]
           (partial remove #(-> % :signature (= signature))))
    (swap! state update-in [:surveys waypoint]
           (partial remove #(-> % :signature (= signature))))
    (info "Removed invalid survey" signature)))

#_(deref (martian/response-for *api* :get-my-agent))

(defn safe-deref [x]
  (try
    (deref x)
    (catch Exception e
      x)))

;; https://docs.spacetraders.io/api-guide/response-errors
;; TODO: handle network issues with retrying
;; TODO: catch registration required after reset
;; TODO: rename q, q* and q** to request, request* and request**
;; TODO: write all requests to a request log for later analysis
(defn- q* [args]
  (let [result
        (let [response (safe-deref (apply (partial martian/response-for *api*) args))]
          (cond
            ;; all good
            (-> response :status #{200 201 204})
            (if-let [data (-> response :body :data)]
              ;; in out app stack the data is autmatically parsed
              data
              ;; in out test stack it needs to be parse here
              (-> response :body (json/parse-string keyword) :data))

            ;; This will happen after a reset. The current token will
            ;; be invalid. That is what the *reset-fn* is for.
            (-> response :status #{401})
            (let [{:keys [message code data]} (-> response :body :error)]
              (error "Auth Error" code message)
              (if (fn? *reset-fn*)
                (do
                  (info "To recover from this error we will now call the registered reset function.")
                  (*reset-fn*))
                (do
                  (fatal "No reset function to recover from this error. Best to shut down at this point.")
                  (System/exit 1))))

            (-> response :status #{400 409})
            (let [{{:keys [code message]} :error} (-> response :body)]
              (error "API Error" code message)
              (case code
                ;; Failed to execute jump. System ... is out of range.
                ;; ?
                ;;
                ;; 400 - The jump drive / gate has a range of 500
                ;; units. Ship transfer failed. Both ships must be
                ;; docked or both ships must be in orbit.
                400 (do)
                ;; 4000 - Ship action is still on cooldown for 25
                ;; second(s)
                4000 {:retry (-> response :body :error :data :cooldown :remainingSeconds)}
                ;; 4202 - Navigate request failed. Destination
                ;; X1-VS75-93799Z is outside the X1-XN54 system.
                4202 (do)
                ;; 4203 - Navigate request failed. Ship GENQREF-3
                ;; requires 56 more fuel for navigation.
                4203 (do
                       ;; FIXME: this creates an endless loop if the
                       ;; destination in farther away than the ship
                       ;; can hold fuel for, or if the local market
                       ;; doesn't provide fuel
                       (dock! (:shipSymbol (second args)))
                       (refuel! (:shipSymbol (second args)))
                       (orbit! (:shipSymbol (second args)))
                       {:retry 0})
                ;; 4204 - Navigate request failed. Ship GENQREF-3 is
                ;; currently located at the destination.
                4204 (do)
                ;; 4205 - Ship extract failed. PLANET is not a valid
                ;; location for extracting minerals.
                4205 (throw message)
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
                4221 (remove-survey* (-> args second :survey))
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
                ;; Ship GENQREF-2 is currently locked while processing
                ;; another request. Concurrent requests to control a
                ;; ship are not supported.
                409 (do)
                ;; 4224 - Ship extract failed. Survey X1-VS75-67965Z-97E75E has been exhausted.
                4224 (remove-survey message)
                ;; default
                (error "Unknown error:" message)))

            ;; Exceeded the rate limit. This should never happen as
            ;; we're activley throttling request toi what's allowed.
            (-> response :status #{429})
            (let [{:keys [error]} (-> response :body)
                  timeout (-> error :data :retryAfter)]
              (warn ">>> We're going too fast (429), timeout for" timeout "seconds. <<<")
              ;; FIXME: factor 2 to recorver from being to eager
              {:retry (* 2 timeout)})

            ;; The server encountered a temporary error and could not
            ;; complete your request. Please try again in 30 seconds.
            (-> response :status #{502})
            (do
              (warn ">>> Encountered a server error (502). Retry in 30s, as advised. <<<")
              {:retry 30})

            ;; This is serious. Probably a deployment. If so we'll get
            ;; a 401, after the 503 is gone, see above.
            (-> response :status #{503})
            (do
              (warn ">>> Service unavailable (503). Propbably a deployment. Retry in 2min. <<<")
              {:retry 120})))]
    (if (contains? result :retry)
      (do
        (Thread/sleep (* 1000 (or (:retry result) 5)))
        (recur args))
      result)))

;; ** Throttling

(def q** (throttling/throttle-fn q* {:regular-count 2
                                     :regular-timeout 1000
                                     :burst-count 10
                                     :burst-timeout 10000}))

(def request-channel (chan))

(def consume-request-log
  (go
    (while true
      (let [[req t0 t1] (<! request-channel)]
        (info "REQ" req (format "%.2fs" (float (/ (- t1 t0) 1000))))))))

;; ** Convenience and logging

(defn q [& args]
  (let [now #(System/currentTimeMillis)
        t0 (now)
        result (q** args)]
    ;; (debug "REQ" (first args) (format "%.2fs" (float (/ (- (now) t0) 1000))))
    (>!! request-channel [(first args) t0 (now)])
    result))

;; * State (local cache)

(def state (duratom :local-file
                    :file-path "state.json"
                    :init {:genqref "rule"}
                    ;; TODO: use util/json-rw instead
                    :rw {:read (comp #(json/parse-string % true) slurp)
                         :write #(spit %1 (json/generate-string %2))}))

;; * Util

(defn xy [{:keys [x y]}]
  (when (and x y)
    (str "[" x "," y "]")))

(defn role [ship]
  (-> ship :registration :role str/lower-case keyword))

#_(role ship)

(defn ship-has? [ship mount]
  ((->> ship :mounts (map :symbol) set) mount))

#_(ship-has? ship "MOUNT_SURVEYOR_I")

(defn get-waypoint [waypoints symbol]
  (first (filter #(-> % :symbol (= symbol)) waypoints)))

(defn min-sec [datetime]
  (last (re-find #"(\d\d:\d\d)\." datetime)))

(defn remaining-capacity
  "Given a SHIP return its remaining cargo capacity"
  [{{:keys [capacity units]} :cargo :as ship}]
  (- capacity units))

(defn remaining-units
  "Given a CONTRACT return the remaining units"
  [contract]
  (let [{:keys [unitsRequired unitsFulfilled]} (-> contract :terms :deliver first)]
    (- unitsRequired unitsFulfilled)))

(defn units-in-cargo
  "For a SHIP return the units of given GOOD in its inventory"
  [ship good]
  (->> ship :cargo :inventory (filter #(-> % :symbol (= good))) first :units))

;; * Operations On State

(defn markets-selling
  ([good]
   (markets-selling good (vals (:markets @state))))
  ([good markets-or-waypoints]
   (->> markets-or-waypoints
        ;; map to markets
        (map #(get-in @state [:markets (keyword (sym %))]))
        ;; remove the entries that either have no market or we're missing market data for
        (remove nil?)
        ;; remove markets that don't sell the good
        (filter (comp #(contains? % good) set (partial map :symbol) :tradeGoods))
        ;; sort by purchase price, cheapest first
        (sort-by (comp :purchasePrice first (partial filter #(-> % :symbol (= good))) :tradeGoods)))))

;; NOTE: multiple arities solely for testability (which is likely an anti-pattern)
(defn- lookup-and-merge-coordinates
  ([market]
   (lookup-and-merge-coordinates market (:waypoints @state)))
  ([market waypoints]
   (as-> (keyword (sym market)) $
     (get waypoints $)
     (select-keys $ [:x :y])
     (merge market $))))

;; TODO: take price into account
;; TODO: don't assume everything is in one system
(defn best-market-for [good waypoint]
  (->> (markets-selling good)
       (map lookup-and-merge-coordinates)
       (tsp/closest waypoint)))

(defn system-where-ship [ship]
  (-> @state :systems (get (keyword (-> ship :nav :systemSymbol)))))

(defn ship-by-name [name]
  (-> @state :ships (get (keyword name))))

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

(defn jumpgate-by-system [system]
  (let [jumpgates (->> @state :jumpgates (filter #(-> % first name util/parse-system (= system))) vals)]
    ;; (assert (<= (count jumpgates) 1) (prn-str jumpgates))
    (first jumpgates)))

(defn jumpgates-of-system [system]
  (->> @state
       :jumpgates
       vals
       (filter #(-> % :systemSymbol (= system)))))

(defn active-contracts []
  (->> @state :contracts vals (filter :accepted) (remove :fulfilled) not-empty))

(defn active-contract []
  (let [contracts (active-contracts)]
    (assert (>= 1 (count contracts)) "Once upon the time you could only have one contract.")
    (first contracts)))

#_(active-contracts)

(defn closest-market-to-waypoint [waypoint]
  (let [waypoint (get-in @state [:waypoints (keyword (sym waypoint))])]
    (->> waypoint
         :systemSymbol
         waypoints-of-system
         (waypoints-with-traits ["MARKETPLACE"])
         ;; FIXME: planets and their orbitals have a distance of 0
         (sort-by #(util/distance % waypoint))
         first
         :symbol
         keyword
         (conj [:markets])
         (get-in @state))))

#_(def ship (refresh! :ship "GENQREF-1"))
#_(def waypoint (-> ship :nav :waypointSymbol))
#_(closest-market-to-waypoint waypoint)

(defn jumpgate-where-ship! [ship]
  (let [{:keys [waypointSymbol]} (:nav ship)]
    (get-in @state [:jumpgates (keyword waypointSymbol)] (refresh! :jumpgate waypointSymbol))))

;; * Hooks

(defonce ^:private hooks (atom {}))

(defn register-hooks [hook-map]
  (swap! hooks merge hook-map))

(defn reset-hooks! []
  (reset! hooks {}))

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

;;; `query!` queries the api (bad)

(defmulti query (fn [entity _] entity))

;; TODO: pass map as 3rd param to call-hooks in refresh fns

;; ** Agents

(defmethod query :agents [_ _]
  (trace "Query agents")
  (let [agents (q :get-agents)]
    (swap! state update :agents
           util/deep-merge (util/index-by (comp keyword :symbol) agents))
    (call-hooks :updated :agents agents)
    agents))

(defmethod query :agent [_ agent]
  (trace "Query agent" (sym agent))
  (let [agent (q :get-agent {:agentSymbol (sym agent)})]
    (swap! state update-in [:agents (keyword (sym agent))]
           util/deep-merge agent)
    (call-hooks :updated :agent agent)
    agent))

;; ** Factions

(defmethod query :factions [_ _]
  (trace "Query factions")
  (let [factions (q :get-factions)]
    (swap! state update :factions
           util/deep-merge (util/index-by (comp keyword :symbol) factions))
    (call-hooks :updated :factions factions)
    factions))

(defmethod query :faction [_ faction]
  (trace "Query faction" (sym faction))
  (let [faction (q :get-faction {:factionSymbol (sym faction)})]
    (swap! state update-in [:factions (keyword (sym faction))]
           util/deep-merge faction)
    (call-hooks :updated :faction faction)
    faction))

;; ** Fleet

(defmethod query :ships [_ _]
  (trace "Query ships")
  (let [limit 20
        ships (loop [result [] page 1]
                (let [batch (q :get-my-ships {:limit limit :page page})
                      result (concat result batch)]
                  (if (< (count batch) limit)
                    result
                    (recur result (inc page)))))]
    (swap! state update :ships
           util/deep-merge (util/index-by (comp keyword :symbol) ships))
    (call-hooks :updated :ships ships)
    ships))

#_(count (query! :ships))

(defmethod query :ship [_ ship]
  (trace "Query ship" (sym ship))
  (let [ship (q :get-my-ship {:shipSymbol (sym ship)})]
    (swap! state update-in [:ships (keyword (sym ship))]
           util/deep-merge ship)
    (call-hooks :updated :ship ship)
    ship))

#_(def ship (query! :ship "GENQREF-1"))

(defmethod query :cargo [_ ship]
  (trace "Query cargo for ship" (sym ship))
  (let [cargo (q :get-ship-cargo {:shipSymbol (sym ship)})]
    (swap! state update-in [:ships (keyword (sym ship)) :cargo]
           util/deep-merge cargo)
    (call-hooks :updated :cargo ship cargo)
    cargo))

(defmethod query :cooldown [_ ship]
  (trace "Query cooldown for ship" (sym ship))
  (let [cooldown (q :get-ship-cooldown {:shipSymbol (sym ship)})]
    (swap! state update-in [:ships (keyword (sym ship)) :cooldown]
           util/deep-merge cooldown)
    (call-hooks :updated :cooldown ship cooldown)
    cooldown))

(defmethod query :nav [_ ship]
  (trace "Query nav for ship" (sym ship))
  (let [nav (q :get-ship-nav {:shipSymbol (sym ship)})]
    (swap! state update-in [:ships (keyword (sym ship)) :nav]
           util/deep-merge nav)
    (call-hooks :updated :nav ship nav)
    nav))

(defmethod query :mounts [_ ship]
  (trace "Query mounts for ship" (sym ship))
  (let [mounts (q :get-mounts {:shipSymbol (sym ship)})]
    (swap! state update-in [:ships (keyword (sym ship))]
           util/deep-merge {:mounts mounts})
    (call-hooks :updated :mounts ship mounts)
    mounts))

;; ** Contracts

(defmethod query :contracts [_ _]
  (trace "Query contracts")
  (let [contracts (q :get-contracts)]
    (swap! state update :contracts
           util/deep-merge (util/index-by (comp keyword :id) contracts))
    (call-hooks :updated :contracts contracts)
    contracts))

(defmethod query :contract [_ {:keys [id] :as contract}]
  (trace "Query contract" id)
  (let [contract (q :get-contract {:contractId id})]
    (swap! state update-in [:contracts (keyword id)]
           util/deep-merge contract)
    (call-hooks :updated :contract contract)
    contract))

;; ** Systems

(defmethod query :systems [_ _]
  (trace "Query systems")
  (let [systems (q :get-systems)]
    (swap! state update :systems
           util/deep-merge (util/index-by (comp keyword :symbol) systems))
    (call-hooks :updated :systems systems)
    systems))

(defmethod query :system [_ system]
  (trace "Query system" (sym system))
  (let [system (q :get-system {:systemSymbol (sym system)})]
    (swap! state update-in [:systems (keyword (sym system))]
           util/deep-merge system)
    (call-hooks :updated :system system)
    system))

;; TODO: this should be paged like :ships
(defmethod query :waypoints [_ system]
  (trace "Query waypoints for system" (sym system))
  (let [waypoints (q :get-system-waypoints {:systemSymbol (sym system) :limit 20})]
    (swap! state update :waypoints
           util/deep-merge (util/index-by (comp keyword :symbol) waypoints))
    (call-hooks :updated :waypoints system waypoints)
    waypoints))

(defmethod query :waypoint [_ waypoint]
  (trace "Query waypoint" (sym waypoint))
  (let [waypoint (q :get-waypoint {:systemSymbol (util/parse-system (sym waypoint))
                                   :waypointSymbol (sym waypoint)})]
    (swap! state update-in [:waypoints (keyword (sym waypoint))]
           util/deep-merge waypoint)
    (call-hooks :updated :waypoint waypoint)
    waypoint))

(defmethod query :market [_ waypoint]
  (trace "Query market at waypoint" (sym waypoint))
  (let [symbol (sym waypoint)
        market (q :get-market {:systemSymbol (util/parse-system symbol)
                               :waypointSymbol symbol})]
    (swap! state update-in [:markets (keyword symbol)]
           util/deep-merge market)
    (call-hooks :updated :market waypoint market)
    market))

(defmethod query :shipyard [_ waypoint]
  (trace "Query shipyard at waypoint" (sym waypoint))
  (let [symbol (sym waypoint)
        shipyard (q :get-shipyard {:systemSymbol (util/parse-system symbol)
                                   :waypointSymbol symbol})]
    (swap! state update-in [:shipyards (keyword symbol)]
           util/deep-merge shipyard)
    (call-hooks :updated :shipyard waypoint shipyard)
    shipyard))

(defmethod query :jumpgate [_ waypoint]
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

;; ** Agents

(defmethod query :my-agent [_ _]
  (trace "Query agent")
  (let [agent (q :get-my-agent)]
    (swap! state update :agent
           util/deep-merge agent)
    (call-hooks :updated :agent agent)
    agent))

#_(query! :agent)

;; * GET api

(defn query!
  ([entity] (query! entity nil))
  ([entity emap] (query entity emap)))

#_(def ship (query! :ship ship))

;;; `refresh!` queries the local cache (good) with implicit fallback to `query!`

(defmulti refresh (fn [entity _] entity))

(defmethod refresh :agents [_ _]
  (or (-> @state :agents vals)
      (query! :agents)))

(defmethod refresh :agent [_ agent]
  (or (-> @state :agents (get (keyword (sym agent))))
      (query! :agent agent)))

(defmethod refresh :factions [_ _]
  (or (-> @state :factions vals)
      (query! :factions)))

(defmethod refresh :faction [_ faction]
  (or (-> @state :factions (get (keyword (sym faction))))
      (query! :faction faction)))

(defmethod refresh :ships [_ _]
  (or (-> @state :ships vals)
      (query! :ships)))

(defmethod refresh :ship [_ ship]
  (or (-> @state :ships (get (keyword (sym ship))))
      (query! :ship ship)))

(defmethod refresh :cargo [_ ship]
  (or (:cargo (refresh :ship ship))
      (query! :cargo ship)))

(defmethod refresh :cooldown [_ ship]
  (or (:cooldown (refresh :ship ship))
      (query! :cooldown ship)))

(defmethod refresh :nav [_ ship]
  (or (:nav (refresh :ship ship))
      (query! :nav ship)))

(defmethod refresh :mounts [_ ship]
  (or (:mounts (refresh :ship ship))
      (query! :mounts ship)))

(defmethod refresh :contracts [_ _]
  (or (-> @state :contracts vals)
      (query! :contracts)))

(defmethod refresh :contract [_ contract]
  (or (-> @state :contracts (get (keyword (get contract :id contract))))
      (query! :contract contract)))

(defmethod refresh :systems [_ _]
  (or (-> @state :systems vals)
      (query! :systems)))

(defmethod refresh :system [_ system]
  (or (-> @state :systems (get (keyword (sym system))))
      (query! :system system)))

(defmethod refresh :waypoints [_ system]
  (if system
    (or (->> @state :waypoints vals (filter #(-> % :systemSymbol #{(sym system)})) not-empty)
        (query! :waypoints system))
    (-> @state :waypoints vals)))

(defmethod refresh :waypoint [_ waypoint]
  (or (-> @state :waypoints (get (keyword (sym waypoint))))
      (query! :waypoint waypoint)))

(defmethod refresh :markets [_ _]
  (or (-> @state :markets vals)
      (throw "There is no endpoint to query multiple markets")))

(defmethod refresh :market [_ waypoint]
  (or (-> @state :markets (get (keyword (sym waypoint))))
      (query! :market waypoint)))

(defmethod refresh :shipyards [_ _]
  (or (-> @state :shipyards vals)
      (throw "There is no endpoint to query multiple shipyards")))

(defmethod refresh :shipyard [_ waypoint]
  (or (-> @state :shipyards (get (keyword (sym waypoint))))
      (query! :shipyard waypoint)))

(defmethod refresh :jumpgates [_ _]
  (or (-> @state :jumpgates vals)
      (throw "There is no endpoint to query multiple jumpgates")))

(defmethod refresh :jumpgate [_ waypoint]
  (or (-> @state :jumpgates (get (keyword (sym waypoint))))
      (query! :jumpgate waypoint)))

(defn refresh!
  ([entity] (refresh! entity nil))
  ([entity emap] (refresh entity emap)))

;; * POST/PATCH (actions)

;; TODO: don't just lazily return the response, instead return updated
;; entities like in `dock!`

;; TODO: the after callback should receive the same values as the return

;; TODO: mulitple swaps on state should be combined into one

;; ** Fleet

(defn purchase-ship! [shipyard ship-type]
  (trace "About to purchase" ship-type "at" (sym shipyard))
  (call-hooks :before :purchase-ship {:shipyard shipyard
                                      :ship-type ship-type})
  (if-let [{:keys [agent ship transaction] :as response}
           (q :purchase-ship {:waypointSymbol (sym shipyard)
                              :shipType ship-type})]
    (do
      ;; diligence
      (when-let [unused (not-empty (dissoc response :agent :ship :transaction))]
        (warn "UNUSED BY purchase-ship!" (prn-str unused)))
      ;; tracing
      (debug "Purchased" ship-type "at" (sym shipyard))
      ;; update state
      (swap! state #(-> %
                        (update :agent util/deep-merge agent)
                        ;; this is a new ship, so assoc-in is ok in this case
                        (update :ships assoc (keyword (sym ship)) ship)
                        ;; TODO: index transaction too, for mergability
                        (update :transactions conj (assoc transaction :reason "purchase-ship"))))
      ;; callbacks
      (call-hooks :after :purchase-ship {:shipyard shipyard
                                         :agent agent
                                         :ship ship
                                         :transaction transaction})
      ;; return
      response)
    (do
      (debug "Failed to purchase" ship-type "at" (sym shipyard))
      (call-hooks :failed :purchase-ship {:shipyard shipyard
                                          :ship-type ship-type}))))

(defn orbit! [ship]
  (trace "Ship" (sym ship) "is about to move to orbit")
  (call-hooks :before :orbit {:ship ship})
  (if-let [{:keys [nav] :as response}
           (q :orbit-ship {:shipSymbol (sym ship)})]
    (do
      ;; diligence
      (when-let [x (not-empty (dissoc response :nav))]
        (warn "UNUSED BY orbit!" (prn-str x)))
      ;; tracing
      (debug "Ship" (sym ship) "moved to orbit")
      ;; update state
      (swap! state update-in [:ships (keyword (sym ship)) :nav]
             util/deep-merge nav)
      ;; callbacks
      (call-hooks :after :orbit {:ship ship
                                 :nav nav})
      ;;return
      {:nav nav
       :ship (-> @state :ships (get (keyword (sym ship))))})
    (do
      (debug "Ship" (sym ship) "failed to move to orbit")
      (call-hooks :failed :orbit {:ship ship}))))

#_(orbit! ship)

(defn- list-goods [goods]
  (->> goods
       (map (fn [{:keys [units tradeSymbol]}] (str units " " tradeSymbol)))
       (str/join ", ")))

(defn refine!
  ([ship product] (refine! ship product {}))
  ([ship product {:keys [on-cooldown] :as options}]
   (trace "Ship" (sym ship) "is about to refine" product)
   (call-hooks :before :refine {:ship ship
                                :product product})
   (if-let [{:keys [cargo cooldown produced consumed] :as response}
            (q :ship-refine {:shipSymbol (sym ship)
                             :produce product})]
     (do
       ;; diligence
       (when-let [x (not-empty (dissoc response :cargo :cooldown :produced :consumed))]
         (warn "UNUSED BY refine!" (prn-str x)))
       ;; tracing
       (debug "Ship" (sym ship) "refined" (list-goods consumed) "into" (list-goods produced))
       ;; update state
       (swap! state update-in [:ships (keyword (sym ship))]
              util/deep-merge {:cargo cargo
                               :cooldown cooldown})
       ;; TODO: maybe persist produced and consumed
       ;; scheduling
       (if-let [expiration (:expiration cooldown)]
         (schedule! expiration
                    #(do
                       (debug "Ship" (sym ship) "completed cooldown after refining" product)
                       (call-hooks :completed :refine {:ship ship
                                                       :product product
                                                       :cargo cargo
                                                       :cooldown cooldown
                                                       :produced produced
                                                       :consumed consumed})
                       (when on-cooldown (on-cooldown))))
         (error "Schedule failed no expiration in" (prn-str response)))
       ;; callbacks
       (call-hooks :after :refine {:ship ship
                                   :product product
                                   :cargo cargo
                                   :cooldown cooldown
                                   :produced produced
                                   :consumed consumed})
       ;;return
       response)
     (do
       (debug "Ship" (sym ship) "failed to refine" product)
       (call-hooks :failed :refine {:ship ship
                                    :product product})))))

(defn chart! [ship]
  (trace "Ship" (sym ship) "is about to chart waypoint")
  (call-hooks :before :chart {:ship ship})
  (if-let [{:keys [chart waypoint] :as response}
           (q :create-chart {:shipSymbol (sym ship)})]
    (do
      ;; diligence
      (when-let [unused (not-empty (dissoc response :x))]
        (warn "UNUSED BY chart!" (prn-str unused)))
      ;; tracing
      (debug "Ship" (sym ship) "charted waypoint")
      ;; update state
      ;; TODO
      ;; callbacks
      (call-hooks :after :chart {:ship ship
                                 :chart chart
                                 :waypoint waypoint})
      ;; return
      response)
    (do
      (debug "Ship" (sym ship) "failed to chart waypoint")
      (call-hooks :failed :chart {:ship ship}))))

(defn dock! [ship]
  (trace "Ship" (sym ship) "is about to dock")
  (call-hooks :before :dock {:ship ship})
  (if-let [{:keys [nav] :as response}
           (q :dock-ship {:shipSymbol (sym ship)})]
    (do
      ;; diligence
      (when-let [unused (not-empty (dissoc response :nav))]
        (warn "UNUSED BY dock!" (prn-str unused)))
      ;; tracing
      (debug "Ship" (sym ship) "docked at" (:waypointSymbol nav))
      ;; update state
      (swap! state update-in [:ships (keyword (sym ship)) :nav]
             util/deep-merge nav)
      ;; callbacks
      (call-hooks :after :dock {:ship ship
                                :nav nav})
      ;; return
      {:nav nav
       :ship (get-in @state [:ships (keyword (sym ship))])
       :waypoint (get-in @state [:waypoints (keyword (:waypointSymbol nav))])})
    (do
      (debug "Ship" (sym ship) "failed to dock")
      (call-hooks :failed :dock {:ship ship}))))

#_(dock! ship)

(defn survey!
  ([ship] (survey! ship {}))
  ([ship {:keys [on-cooldown] :as options}]
   (trace "Ship" (sym ship) "is about to conduct a survey")
   (call-hooks :before :survey {:ship ship
                                :options options})
   (if-let [{:keys [surveys cooldown] :as response}
            (q :create-survey {:shipSymbol (sym ship)})]
     (do
       ;; diligence
       (when-let [unused (not-empty (dissoc response :surveys :cooldown))]
         (warn "UNUSED BY survey!" (prn-str unused)))
       ;; tracing
       (debug "Ship" (sym ship) "conducted a survey and found...")
       (doseq [survey surveys]
         (debug ">" (:size survey) "deposits of" (str/join ", " (map :symbol (:deposits survey)))))
       ;; update state
       (swap! state update-in [:ships (keyword (sym ship)) :cooldown]
              util/deep-merge cooldown)
       (doseq [{:keys [symbol] :as survey} surveys]
         (swap! state update-in [:surveys (keyword symbol)] conj survey))
       ;; scheduling
       (if-let [expiration (:expiration cooldown)]
         (schedule! expiration
                    #(do
                       (debug "Ship" (sym ship) "completed cooldown after survey")
                       (call-hooks :completed :survey {:ship ship
                                                       :options options
                                                       :surveys surveys
                                                       :cooldown cooldown})
                       (if on-cooldown
                         (on-cooldown)
                         (warn "WARNING: SHIP" (sym ship) "DID NOT SPECIFIY ACTION AFTER SURVEY"))))
         (error "Schedule failed no expiration in" (prn-str response)))
       ;; callbacks
       (call-hooks :after :survey {:ship ship
                                   :options options
                                   :surveys surveys
                                   :cooldown cooldown})
       ;; return
       response)
     (do
       (debug "Ship" (sym ship) "failed to conduct a survey")
       (call-hooks :failed :survey {:ship ship
                                    :options options})))))

#_(survey! ship)

(defn extract!
  "A survey can be passed in options `{:survey ...}`"
  ([ship] (extract! ship {}))
  ([ship {:keys [on-cooldown] :as options}]
   (trace "Ship" (sym ship) "is about to extract resources")
   (when-not on-cooldown
     (warn "WARNING: SHIP" (sym ship) "EXTRACTS WITHOUT AFTER COOLDOWN ACTION"))
   (call-hooks :before :extract {:ship ship
                                 :options options})
   (if-let [{:keys [extraction cooldown cargo] :as response}
            (q :extract-resources (-> (into {} (filter last options))
                                      (dissoc :on-cooldown)
                                      (assoc :shipSymbol (sym ship))))]
     (do
       ;; diligence
       (when-let [unused (not-empty (dissoc response :extraction :cooldown :cargo))]
         (warn "UNUSED BY extract!" (prn-str unused)))
       ;; tracing
       (when (:survey options)
         (debug "Ship" (sym ship) "used survey" (-> options :survey :signature)))
       (debug "Ship" (sym ship) "extracted" (-> extraction :yield :units) "units of" (-> extraction :yield :symbol))
       ;; update state
       (swap! state update-in [:ships (keyword (sym ship))]
              util/deep-merge {:cargo cargo
                               :cooldown cooldown})
       ;; scheduling
       (if-let [expiration (:expiration cooldown)]
         (schedule! expiration
                    #(do
                       (debug "Ship" (sym ship) "completed cooldown after extract")
                       (call-hooks :completed :extract {:ship ship
                                                        :options options
                                                        :extraction extraction
                                                        :cooldown cooldown
                                                        :cargo cargo})
                       (if on-cooldown
                         (on-cooldown)
                         (warn "WARNING: SHIP" (sym ship) "DID NOT SPECIFIY ACTION AFTER EXTRACT"))))
         (error "Schedule failed. No expiration in" (prn-str response)))
       ;; callbacks
       (call-hooks :after :extract {:ship ship
                                    :options options
                                    :extraction extraction
                                    :cooldown cooldown
                                    :cargo cargo})
       ;; return
       response)
     (do
       (debug "Ship" (sym ship) "failed to extract resources")
       (call-hooks :failed :extract {:ship ship
                                     :options options})))))

#_(extract! "GENQREF-5")

(defn jettison! [ship item units]
  (trace "Ship" (sym ship) "is about to jettison" units "units of" (sym item))
  (call-hooks :before :jettison {:ship ship
                                 :item item
                                 :units units})
  (if-let [{:keys [cargo] :as response}
           (q :jettison {:shipSymbol (sym ship)
                         :symbol (sym item)
                         :units units})]
    (do
      ;; diligence
      (when-let [unused (not-empty (dissoc response :cargo))]
        (warn "UNUSED BY jettison!" (prn-str unused)))
      ;; tracing
      (debug "Ship" (sym ship) "jettisoned" units "units of" (sym item))
      ;; update state
      (swap! state update-in [:ships (keyword (sym ship)) :cargo]
             util/deep-merge cargo)
      ;; callbacks
      (call-hooks :after :jettison {:ship ship
                                    :item item
                                    :units units
                                    :cargo cargo})
      ;; return
      response)
    (do
      (debug "Ship" (sym ship) "failed to jettison" units "units of" (sym item))
      (call-hooks :failed :jettison {:ship ship
                                     :item item
                                     :units units}))))

#_(jettison! "GENQREF-5" "ICE_WATER" 4)

(defn jump!
  ([ship system] (jump! ship system {}))
  ([ship system {:keys [on-cooldown] :as options}]
   (trace "Ship" (sym ship) "is about to jump to system" (sym system))
   (call-hooks :before :jump {:ship ship
                              :system system})
   (if-let [{:keys [cooldown nav] :as response}
            (q :jump-ship {:shipSymbol (sym ship)
                           :systemSymbol (sym system)})]
     (do
       ;; diligence
       (when-let [unused (not-empty (dissoc response :cooldown :nav))]
	 (warn "UNUSED BY jump!" (prn-str unused)))
       ;; tracing
       (debug "Ship" (sym ship) "jumped to system" (sym system) (xy system))
       ;; update state
       (swap! state update-in [:ships (keyword (sym ship))]
              util/deep-merge {:cooldown cooldown
                               :nav nav})
       ;; scheduling
       (if-let [expiration (:expiration cooldown)]
         (schedule! expiration
                    #(do
                       (debug "Ship" (sym ship) "completed cooldown after jump")
                       (call-hooks :completed :jump {:ship ship
                                                     :system system
                                                     :nav nav
                                                     :cooldown cooldown})
                       (when on-cooldown (on-cooldown))))
         (error "Schedule failed. No expiration in" (prn-str response)))
       ;; callbacks
       (call-hooks :after :jump {:ship ship
                                 :system system
                                 :nav nav
                                 :cooldown cooldown})
       ;; return
       response)
     (do
       (debug "Ship" (sym ship) "failed to jump to system" (sym system))
       (call-hooks :failed :jump {:ship ship
                                  :system system})))))

#_(jump! ship "X1-PY76")

(defn navigate!
  ([ship destination] (navigate! ship destination {}))
  ([ship destination {:keys [on-arrival] :as options}]
   (trace "Ship" (sym ship) "navigates to" (sym destination))
   (call-hooks :before :navigate {:ship ship
                                  :destination destination
                                  :options options})
   (if-let [{:keys [nav fuel] :as response}
            (q :navigate-ship {:shipSymbol (sym ship)
                               :waypointSymbol (sym destination)})]
     (do
       ;; diligence
       (when-let [unused (not-empty (dissoc response :nav :fuel))]
         (warn "UNUSED BY navigate!" (prn-str unused)))
       ;; tracing
       (debug "Ship" (sym ship) "in transit to" (sym destination)
              (str "(" (:type destination) ")") "arrival at" (-> nav :route :arrival))
       ;; update state
       (swap! state update-in [:ships (keyword (sym ship))]
              util/deep-merge {:nav nav
                               :fuel fuel})
       ;; TODO: use fuel
       ;; scheduling
       (if-let [arrival (-> nav :route :arrival)]
         (schedule! arrival
                    #(do
                       (debug "Ship" (sym ship) "has arrived")
                       (call-hooks :completed :navigate {:ship ship
                                                         :destination destination
                                                         :nav nav})
                       (when on-arrival (on-arrival))))
         (error "Schedule failed. No arrival in" (prn-str response)))
       ;; callbacks
       (call-hooks :after :navigate {:ship ship
                                     :destination destination
                                     :nav nav})
       ;; return
       response)
     (do
       (debug "Ship" (sym ship) "failed to navigate to waypoint" (sym destination))
       (call-hooks :failed :navigate {:ship ship
                                      :destination destination})))))

#_(navigate! ship asteroid-field)

(defn flight-mode! [ship mode]
  (let [mode (-> mode name str/upper-case)]
    (trace "Ship" (sym ship) "is about to set flight mode to" mode)
    (call-hooks :before :flight-mode {:ship ship
                                      :mode mode})
    ;; NOTE: things are different here as the response is the
    ;; updated nav (e.g. the usual diligence doesn't make sense)
    (if-let [nav
             (q :patch-ship-nav {:shipSymbol (sym ship)
                                 :flightMode mode})]
      (do
        ;; tracing
        (debug "Ship" (sym ship) "set flight mode to" mode)
        ;; update state
        (swap! state update-in [:ships (keyword (sym ship)) :nav]
               util/deep-merge nav)
        ;; callbacks
        (call-hooks :after :flight-mode {:ship ship
                                         :mode mode})
        ;; return
        {:nav nav
         ;; for convenience we're also returning the updated ship
         :ship (update ship :nav util/deep-merge nav)})
      (do
        (debug "Ship" (sym ship) "failed to set flight mode to" mode)
        (call-hooks :failed :flight-mode {:ship ship
                                          :mode mode})))))

#_(flight-mode! ship :burn)

(defn warp!
  ([ship destination] (warp! ship destination {}))
  ([ship destination {:keys [on-arrival] :as options}]
   (trace "Ship" (sym ship) "is about to warp to" (sym destination))
   (call-hooks :before :warp {:ship ship
                              :destination destination})
   (if-let [{:keys [nav] :as response}
            ;; TODO: implement
            (q :warp-ship {})]
     (do
       ;; diligence
       (when-let [unused (not-empty (dissoc response :nav))]
         (warn "UNUSED BY flight-mode!" (prn-str unused)))
       ;; tracing
       (debug "Ship" (sym ship) "is warping to" (sym destination) "arrival at" (-> nav :route :arrival))
       ;; update state
       (swap! state update-in [:ships (keyword (sym ship)) :nav]
              util/deep-merge nav)
       ;; scheduling
       (if-let [arrival (-> nav :route :arrival)]
         (schedule! arrival
                    #(do
                       (debug "Ship" (sym ship) "has arrived")
                       (call-hooks :completed :warp {:ship ship
                                                     :destination destination
                                                     :nav nav
                                                     :response response})
                       (when on-arrival (on-arrival))))
         (error "Schedule failed. No arrival in" (prn-str response)))
       ;; callbacks
       (call-hooks :after :warp {:ship ship
                                 :destination destination
                                 :nav nav
                                 :response response})
       ;; return
       response)
     (do
       (debug "Ship" (sym ship) "failed to warp to" (sym destination))
       (call-hooks :failed :flight-mode {:ship ship
                                         :destination destination})))))

(defn sell-cargo! [ship trade-symbol units]
  (trace "Ship" (sym ship) "is about to sell" units "units of" trade-symbol)
  (call-hooks :before :sell-cargo {:ship ship
                                   :trade-symbol trade-symbol
                                   :units units})
  (if-let [{:keys [transaction agent cargo] :as response}
           (q :sell-cargo {:shipSymbol (sym ship)
                           :symbol trade-symbol
                           :units units})]
    (do
      ;; diligence
      (when-let [unused (not-empty (dissoc response :transaction :agent :cargo))]
        (warn "UNUSED BY sell-cargo!" (prn-str unused)))
      ;; tracing
      (debug "Ship" (sym ship) "sold" units "units of" trade-symbol "for" (:totalPrice transaction))
      ;; update state
      (swap! state update :agent
             util/deep-merge agent)
      (swap! state update-in [:ships (keyword (sym ship)) :cargo]
             util/deep-merge cargo)
      (swap! state update :transactions conj
             (assoc transaction :reason "sell-cargo"))
      ;; callbacks
      (call-hooks :after :sell-cargo {:ship ship
                                      :trade-symbol trade-symbol
                                      :units units
                                      :response response})
      ;; return
      response)
    (do
      (debug "Ship" (sym ship) "failed to sell" units "units of" trade-symbol)
      (call-hooks :failed :sell-cargo {:ship ship
                                       :trade-symbol trade-symbol
                                       :units units}))))

(defn scan-systems!
  ([ship] (scan-systems! ship {}))
  ([ship {:keys [on-cooldown] :as options}]
   (trace "Ship" (sym ship) "is about to scan systems")
   (call-hooks :before :scan-systems {:ship ship
                                      :options options})
   (if-let [{:keys [cooldown systems] :as response}
            (q :create-ship-system-scan {:shipSymbol (sym ship)})]
     (do
       ;; diligence
       (when-let [unused (not-empty (dissoc response :cooldown :systems))]
         (warn "UNUSED BY scan-systems!" (prn-str unused)))
       ;; tracing
       (debug "Ship" (sym ship) "scanned" (count systems) "systems")
       ;; update state
       (swap! state update-in [:ships (keyword (sym ship)) :cooldown]
              util/deep-merge cooldown)
       ;; TODO: maybe use distance, but maybe not because it can easily be calculated
       (->> systems
            (map #(dissoc % :distance))
            (util/index-by (comp keyword :symbol))
            (swap! state update :systems util/deep-merge))
       ;; scheduling
       (if-let [expiration (:expiration cooldown)]
         (schedule! expiration
                    #(do
                       (debug "Ship" (sym ship) "completed cooldown after scanning systems")
                       (call-hooks :completed :scan-systems {:ship ship
                                                             :systems systems
                                                             :cooldown cooldown})
                       (when on-cooldown (on-cooldown))))
         (error "Schedule failed. No expiration in" (prn-str response)))
       ;; callbacks
       (call-hooks :after :scan-systems {:ship ship
                                         :systems systems
                                         :cooldown cooldown})
       ;; return
       response)
     (do
       (debug "Ship" (sym ship) "failed to scan systems")
       (call-hooks :failed :scan-systems {:ship ship})))))

#_(scan-systems! ship)

(defn scan-waypoints!
  ([ship] (scan-waypoints! ship {}))
  ([ship {:keys [on-cooldown] :as options}]
   (trace "Ship" (sym ship) "is about to scan waypoints")
   (call-hooks :before :scan-waypoints {:ship ship
                                        :options options})
   (if-let [{:keys [cooldown waypoints] :as response}
            (q :create-ship-waypoint-scan {:shipSymbol (sym ship)})]
     (do
       ;; diligence
       (when-let [unused (not-empty (dissoc response :cooldown :waypoints))]
         (warn "UNUSED BY scan-waypoints!" (prn-str unused)))
       ;; tracing
       (debug "Ship" (sym ship) "scanned" (count waypoints) "waypoints")
       ;; update state
       (swap! state update-in [:ships (keyword (sym ship)) :cooldown]
              util/deep-merge cooldown)
       (->> waypoints
            (util/index-by (comp keyword :symbol))
            (swap! state update :waypoints util/deep-merge))
       ;; scheduling
       (if-let [expiration (:expiration cooldown)]
         (schedule! expiration
                    #(do
                       (debug "Ship" (sym ship) "completed cooldown after scanning waypoints")
                       (call-hooks :completed :scan-waypoints {:ship ship
                                                               :waypoints waypoints
                                                               :cooldown cooldown})
                       (when on-cooldown (on-cooldown))))
         (error "Schedule failed. No expiration in" (prn-str response)))
       ;; callbacks
       (call-hooks :after :scan-waypoints {:ship ship
                                           :waypoints waypoints
                                           :cooldown cooldown})
       ;; return
       response)
     (do
       (debug "Ship" (sym ship) "failed to scan waypoints")
       (call-hooks :failed :scan-waypoints {:ship ship})))))

#_(scan-waypoints! ship)

(defn scan-ships!
  ([ship] (scan-ships! ship {}))
  ([ship {:keys [on-cooldown] :as options}]
   (trace "Ship" (sym ship) "is about to scan ships")
   (call-hooks :before :scan-ships {:ship ship
                                    :options options})
   (if-let [{:keys [cooldown ships] :as response}
            (q :create-ship-waypoint-scan {:shipSymbol (sym ship)})]
     (do
       ;; diligence
       (when-let [unused (not-empty (dissoc response :cooldown :ships))]
         (warn "UNUSED BY scan-ships!" (prn-str unused)))
       ;; tracing
       (debug "Ship" (sym ship) "scanned" (count ships) "ships")
       ;; update state
       (swap! state update-in [:ships (keyword (sym ship)) :cooldown]
              util/deep-merge cooldown)
       ;; TODO: store ships somewhere
       ;; scheduling
       (if-let [expiration (:expiration cooldown)]
         (schedule! expiration
                    #(do
                       (debug "Ship" (sym ship) "completed cooldown after scanning ships")
                       (call-hooks :completed :scan-ships {:ship ship
                                                           :ships ships
                                                           :cooldown cooldown})
                       (when on-cooldown (on-cooldown)))))
       ;; callbacks
       (call-hooks :after :scan-ships {:ship ship
                                       :ships ships
                                       :cooldown cooldown})
       ;; return
       response)
     (do
       (debug "Ship" (sym ship) "failed to scan ships")
       (call-hooks :failed :scan-ships {:ship ship})))))

#_(scan-ships! ship)

(defn refuel! [ship]
  (trace "Ship" (sym ship) "refuels")
  (call-hooks :before :refuel {:ship ship})
  (if-let [{:keys [agent fuel transaction] :as response}
           (q :refuel-ship {:shipSymbol (sym ship)})]
    (do
      ;; diligence
      (when-let [unused (not-empty (dissoc response :agent :fuel :transaction))]
        (warn "UNUSED BY refuel!" (prn-str unused)))
      ;; tracing
      (debug "Ship" (sym ship) "refueled")
      ;; update state
      (swap! state update :agent
             util/deep-merge agent)
      (swap! state update-in [:ships (keyword (sym ship)) :fuel]
             util/deep-merge fuel)
      (swap! state update :transactions conj
             (assoc transaction :reason "refuel-ship"))
      ;; callbacks
      (call-hooks :after :refuel {:ship (get-in @state [:ships (keyword (sym ship))])
                                  :agent agent
                                  :fuel fuel
                                  :transaction transaction
                                  :response response})
      ;; return
      response)
    (do
      (debug "Ship" (sym ship) "failed to refuel")
      (call-hooks :failed :refuel {:ship ship}))))

#_(refuel! ship)

(defn purchase-cargo! [ship trade units]
  (trace "Ship" (sym ship) "is about to purchase" units "units of" trade)
  (call-hooks :before :purchase-cargo {:ship ship
                                       :trade trade
                                       :units units})
  (if-let [{:keys [cargo agent transaction] :as response}
           (q :purchase-cargo {:shipSymbol (sym ship)
                               :symbol trade
                               :units units})]
    (do
      ;; diligence
      (when-let [unused (not-empty (dissoc response :agent :cargo :transaction))]
        (warn "UNUSED BY purchase-cargo!" (prn-str unused)))
      ;; tracing
      (debug "Ship" (sym ship) "purchased" units "units of" trade "for" (:totalPrice transaction))
      ;; update state
      (swap! state update :agent
             util/deep-merge agent)
      (swap! state update-in [:ships (keyword (sym ship)) :cargo]
             util/deep-merge cargo)
      (swap! state update :transactions conj
             (assoc transaction :reason "purchase-cargo"))
      ;; callbacks
      (call-hooks :after :purchase-cargo {:ship ship
                                          :trade trade
                                          :units units
                                          :agent agent
                                          :cargo cargo
                                          :transaction transaction
                                          :response response})
      ;; return
      response)
    (do
      (debug "Ship" (sym ship) "failed to purchase" units "units of" trade)
      (call-hooks :failed :purchase-cargo {:ship ship
                                           :trade trade
                                           :units units}))))

#_(purchase-cargo! ship "ANTIMATTER" 1)

;; NOTE: this requires the swagger spec to be fixed, the query
;; parameter need to be called `shipSymbol2`
(defn transfer! [from to trade units]
  (trace "Ship" (sym from) "is about to transfer" units "of" trade "to ship" (sym to))
  (call-hooks :before :transfer {:from from
                                 :to to
                                 :trade trade
                                 :units units})
  (if-let [{:keys [cargo] :as response}
           (q :transfer-cargo {:shipSymbol2 (sym from)
                               :shipSymbol (sym to)
                               :tradeSymbol trade
                               :units units})]
    (do
      ;; diligence
      (when-let [unused (not-empty (dissoc response :x))]
        (warn "UNUSED BY transfer!" (prn-str unused)))
      ;; tracing
      (debug "Ship" (sym from) "transfered" units "of" trade "to ship" (sym to))
      ;; update state
      ;; TODO
      ;; callbacks
      (call-hooks :after :transfer {:from from
                                    :to to
                                    :trade trade
                                    :units units
                                    :cargo cargo
                                    :response response})
      response)
    (do
      (debug "Ship" (sym from) "failed to transfer" units "units of" trade "to ship" (sym to))
      (call-hooks :failed :transfer {:from from
                                     :to to
                                     :trade trade
                                     :units units}))))

;; ** Contracts

(defn negotiate-contract! [ship]
  (trace "Ship" (sym ship) "is about to negotiate a contract")
  (call-hooks :before :negotiate-contract {:ship ship})
  (if-let [{:keys [contract] :as response}
           (q :negotiate-contract {:shipSymbol (sym ship)})]
    (do
      ;; diligence
      (when-let [unused (not-empty (dissoc response :contract))]
        (warn "UNUSED BY negotiate-contract!" (prn-str unused)))
      ;; tracing
      (debug "Ship" (sym ship) "negotiated a contract:" (prn-str contract))
      ;; update state
      ;; this is a new contract, hence assoc-in is fine
      (swap! state assoc-in [:contracts (keyword (:id contract))] contract)
      ;; callbacks
      (call-hooks :after :negotiate-contract {:ship ship
                                              :contract contract
                                              :response response})
      ;; return
      response)
    (do
      (debug "Ship" (sym ship) "failed to negotiate contract")
      (call-hooks :failed :negotiate-contract {:ship ship}))))

(defn deliver-contract! [ship contract trade units]
  (let [id (or (:id contract) contract)]
    (trace "Ship" (sym ship) "delivers" units "units of" trade "on contract" id)
    (call-hooks :before :deliver-contract {:ship ship
                                           :contract contract
                                           :trade trade
                                           :units units})
    (if-let [{:keys [contract cargo] :as response}
             (q :deliver-contract {:contractId id
                                   :shipSymbol (sym ship)
                                   :tradeSymbol trade
                                   :units units})]
      (do
        ;; diligence
        (when-let [unused (not-empty (dissoc response :contract :cargo))]
          (warn "UNUSED BY deliver-contract!" (prn-str unused)))
        ;; tracing
        (debug "Ship" (sym ship) "delivered" units "units of" trade "on contract" id)
        ;; update state
        (swap! state update-in [:contracts (keyword (:id contract))]
               util/deep-merge contract)
        (swap! state update-in [:ships (keyword (sym ship)) :cargo]
               util/deep-merge cargo)
        ;; callbacks
        (call-hooks :after :deliver-contract {:ship ship
                                              :contract contract
                                              :trade trade
                                              :units units
                                              :cargo cargo
                                              :response response})
        ;; return
        response)
      (do
        (debug "Ship" (sym ship) "failed to deliver" units "units of" trade "on contract" id)
        (call-hooks :failed :deliver-contract {:ship ship
                                               :contract contract
                                               :trade trade
                                               :units units})))))

(defn accept-contract! [contract]
  (let [id (or (:id contract) contract)]
    (trace "About to accept contract" id)
    (call-hooks :before :accept-contract {:contract contract})
    (if-let [{:keys [contract agent] :as response}
             (q :accept-contract {:contractId id})]
      (do
        ;; diligence
        (when-let [unused (not-empty (dissoc response :contract :agent))]
          (warn "UNUSED BY accept-contract!" (prn-str unused)))
        ;; tracing
        ;; TODO: output details
        (debug "Accepted contract" id)
        ;; update state
        (swap! state update-in [:contracts (keyword id)]
               util/deep-merge contract)
        (swap! state update :agent
               util/deep-merge agent)
        ;; callbacks
        (call-hooks :after :accept-contract {:contract contract
                                             :agent agent
                                             :response response})
        ;; return
        response)
      (do
        (debug "Failed to accept contract" id)
        (call-hooks :failed :accept-contract {:contract contract})))))

(defn fulfill-contract! [contract]
  (let [id (or (:id contract) contract)]
    (trace "About to fulfill contract" id)
    (call-hooks :before :fulfill-contract {:contract contract})
    (if-let [{:keys [contract agent] :as response}
             (q :fulfill-contract {:contractId id})]
      (do
        ;; diligence
        (when-let [unused (not-empty (dissoc response :contract :agent))]
          (warn "UNUSED BY fulfill-contract!" (prn-str unused)))
        ;; tracing
        (debug "Fulfilled contracts" id)
        ;; update state
        (swap! state update-in [:contracts (keyword (:id contract))]
               util/deep-merge contract)
        (swap! state update :agent
               util/deep-merge agent)
        ;; callbacks
        (call-hooks :after :fulfill-contract {:contract contract
                                              :agent agent
                                              :response response})
        ;; return
        response)
      (do
        (debug "Failed to fulfill contract" id)
        (call-hooks :failed :fulfill-contract {:contract contract})))))

#_(fulfill-contract! (-> @state :contracts vals first))

;; ** Mounts

(defn install-mount! [ship mount]
  (trace "About to install mount" (sym mount) "into ship" (sym ship))
  (call-hooks :before :install-mount {:ship ship
                                      :mount mount})
  (if-let [{:keys [agent mounts cargo transaction] :as response}
           (q :install-mount {:shipSymbol (sym ship)
                              :symbol (sym mount)})]
    (do
      ;; diligence
      (when-let [unused (not-empty (dissoc response :agent :mounts :cargo :transaction))]
        (warn "UNUSED BY install-mount!" (prn-str unused)))
      ;; tracing
      (debug "Installed mount" (sym mount) "into ship" (sym ship))
      ;; update state
      (swap! state update-in [:ships (keyword (sym ship))]
             util/deep-merge {:mounts mounts :cargo cargo})
      (swap! state update :agent
             util/deep-merge agent)
      ;; TODO: maybe we need to add the information that this was from install-mount
      (swap! state update :transactions conj
             (assoc transaction :reason "install-mount"))
      ;; callbacks
      (call-hooks :after :install-mount {:agent agent
                                         :mounts mounts
                                         :cargo cargo
                                         :transaction transaction
                                         :response response})
      ;; return
      {:ship (get-in @state [:ships (keyword (sym ship))])
       :response response})
    (do
      (debug "Failed to install mount" (sym mount) "into ship" (sym ship))
      (call-hooks :failed :install-mount {:ship ship
                                          :mount mount}))))

(defn remove-mount! [ship mount]
  (trace "About to remove mount" (sym mount) "from ship" (sym ship))
  (call-hooks :before :remove-mount {:ship ship
                                     :mount mount})
  (if-let [{:keys [agent mounts cargo transaction] :as response}
           (q :remove-mount {:shipSymbol (sym ship)
                             :symbol (sym mount)})]
    (do
      ;; diligence
      (when-let [unused (not-empty (dissoc response :agent :mounts :cargo :transaction))]
        (warn "UNUSED BY remove-mount!" (prn-str unused)))
      ;; tracing
      (debug "Removed mount" (sym mount) "from ship" (sym ship))
      ;; update state
      (swap! state update-in [:ships (keyword (sym ship))]
             util/deep-merge {:mounts mounts :cargo cargo})
      (swap! state update :agent
             util/deep-merge agent)
      ;; TODO: add info that this was remove-mount
      (swap! state update :transactions conj
             (assoc transaction :reason "remove-mount"))
      ;; callbacks
      (call-hooks :after :remove-mount {:agent agent
                                        :mounts mounts
                                        :cargo cargo
                                        :transaction transaction
                                        :response response})
      ;; return
      {:ship (get-in @state [:ships (keyword (sym ship))])
       :response response})
    (do
      (debug "Failed to remove mount" (sym mount) "from ship" (sym ship))
      (call-hooks :failed :remove-mount {:ship ship
                                         :mount mount}))))

;;; Convenience

;; TODO: make it use refresh! instead of query!
(defn sell-all-cargo!
  ([ship] (sell-all-cargo! ship {}))
  ([ship {:keys [except] :or {except []}}]
   (debug "Ship" (sym ship) "visiting market")
   (let [ship (refresh! :ship ship)
         ;; market (refresh! :market {:symbol (-> ship :nav :waypointSymbol)})
         inv (->> ship :cargo :inventory)
         inv² (remove #(->> % :symbol (contains? (set except))) inv)]
     (doseq [{:keys [symbol units] :as item} inv²]
       (sell-cargo! ship symbol units)))))

"Loaded api."
