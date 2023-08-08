(ns genqref-lib.api-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [clojure.walk :as walk]
            [genqref-lib.api :as api :refer :all]
            [martian.core :as martian]
            [martian.interceptors :refer [inject]]
            [martian.httpkit :as martian-http]
            [martian.vcr :as vcr]
            [clojure.edn :as edn]
            [tripod.context :as tc]))

(defn- uuid []
  (.toString (java.util.UUID/randomUUID)))

(defn- gen-sign [prefix]
  (str prefix "-"
       (-> (uuid)
           (str/split #"-")
           first)))

#_(gen-sign "ZAP-QA")

(defn- random-faction []
  (first (shuffle ["QUANTUM" "VOID" "DOMINION" "GALACTIC" "COSMIC"])))

#_(random-faction)

(defn register []
  (register! {:symbol (gen-sign "QA")
              :faction (random-faction)
              :email "zap-qa@example.com"}))

#_(->> (register) :token (spit "test-token.txt"))

(def vcr-opts {:store {:kind :file
                       :root-dir "test-resources/vcr"
                       :pprint? true}
               ;;:extra-requests :repeat-last
               })

;; (defn playback [{:keys [on-missing-response] :as opts}]
;;   (let [counters (atom {})]
;;     {:name ::playback
;;      :enter (fn [ctx]
;;               #dbg
;;               (let [request-count (#'vcr/inc-counter! counters ctx)]
;;                 (if-let [response (vcr/load-response opts (assoc ctx ::vcr/request-count request-count))]
;;                   (dissoc (assoc ctx :response response) ::tc/queue)
;;                   ctx)))}))
;;
;; (defmethod vcr/load-response :file [{:keys [extra-requests] :as opts} ctx]
;;   (let [file (#'vcr/response-file opts ctx)]
;;     (if (.exists file)
;;       (edn/read-string (slurp file))
;;       (some->
;;        (condp = extra-requests
;;          :repeat-last (#'vcr/last-response opts ctx)
;;          :cycle (#'vcr/cycled-response opts ctx)
;;          nil)
;;        slurp
;;        edn/read-string))))

;; (defn record-api []
;;   (martian-http/bootstrap-openapi
;;    "spacetraders.v2.json" ;; use a bundled resource instead
;;    {:interceptors
;;     (-> [(#'api/add-authentication-header (slurp "test-token.txt"))]
;;         (concat martian-http/default-interceptors)
;;         (inject (vcr/playback vcr-opts)
;;                 :before (:name martian-http/perform-request)))}))
;;
;; (defn playback-api []
;;   (martian-http/bootstrap-openapi
;;    "spacetraders.v2.json" ;; use a bundled resource instead
;;    {:interceptors
;;     (-> [(#'api/add-authentication-header (slurp "test-token.txt"))]
;;         (concat martian-http/default-interceptors)
;;         (inject (vcr/playback vcr-opts)
;;                 :replace (:name martian-http/perform-request)))}))

(defn test-api []
  (martian-http/bootstrap-openapi
   "spacetraders.v2.json" ;; use a bundled resource instead
   {:interceptors
    (-> [(#'api/add-authentication-header (slurp "test-token.txt"))]
        (concat martian-http/default-interceptors)
        (inject (vcr/playback vcr-opts)
                :before (:name martian-http/perform-request))
        (inject (vcr/record vcr-opts)
                :before (:name martian-http/perform-request)))}))

;; (defn list-to-vec [x]
;;   (if (sequential? x) (vec x) x))
;;
;; (defn lists-to-vecs [x]
;;   (walk/postwalk list-to-vec x))

(use-fixtures :each (fn [f]
                      (binding [*api* (test-api)]
                        (f))))

(comment
  (binding [*api* (test-api)]
    (refresh! :ships))
  )

;; (defn playback [{:keys [on-missing-response] :as opts}]
;;   (let [counters (atom {})]
;;     {:name ::playback
;;      :enter (fn [ctx]
;;               #dbg
;;               (let [request-count (#'vcr/inc-counter! counters ctx)]
;;                 (if-let [response (vcr/load-response opts (assoc ctx ::vcr/request-count request-count))]
;;                   (assoc ctx :response response)
;;                   (condp = on-missing-response
;;                     :throw-error (let [message (str "No response stored for request " (#'vcr/request-op ctx) " " (#'vcr/request-key ctx))]
;;                                    (throw #?(:clj (Exception. message)
;;                                              :cljs (js/Error. message))))
;;                     :generate-404 (assoc ctx :response {:status 404})
;;                     ctx))))}))
;;
;; ;; guerilla patching
;; (alter-var-root #'vcr/playback (constantly playback))

;; TODO: this has no business here, move it somewhere else
(defn api-doc
  "Prints a brief api doc to stdout"
  [api]
  (println "--------------------------------------------------------------------------------")
  (println "Printing information for" (count (:handlers api)) "handlers:")
  (->> api
       :handlers
       (map (fn [{:keys [route-name method path-parts]}]
              [route-name method (apply str path-parts)]))
       (sort-by last)
       (map (partial apply format "%-30s %-6s %s"))
       (map println)
       doall)
  (println "--------------------------------------------------------------------------------"))

;; (deftest refresh!-agents-test
;;   (is (= #{}
;;          (-> (refresh! :agents)
;;              first
;;              keys
;;              set))))

;; (deftest refresh!-agent-test
;; (deftest refresh!-factions-test
;; (deftest refresh!-faction-test

(deftest refresh!-ships-test
  (is (= #{:reactor
           :cargo
           :frame
           :mounts
           :modules
           :symbol
           :crew
           :fuel
           :engine
           :registration
           :nav}
         (-> (refresh! :ships)
             first
             keys
             set))))


;; (deftest refresh!-ship-test
;; (deftest refresh!-cargo-test
;; (deftest refresh!-cooldown-test
;; (deftest refresh!-nav-test
;; (deftest refresh!-mounts-test
;; (deftest refresh!-contracts-test
;; (deftest refresh!-contract-test
;; (deftest refresh!-systems-test
;; (deftest refresh!-system-test
;; (deftest refresh!-waypoints-test
;; (deftest refresh!-waypoint-test
;; (deftest refresh!-market-test
;; (deftest refresh!-shipyard-test
;; (deftest refresh!-jumpgate-test
;; (deftest refresh!-my-agent-test

;;(deftest purchase-ship!-test
;;  (let [{:keys [agent ship transaction] :as response} (purchase-ship! ...

(deftest orbit!-test
  (let [ship (-> @state :ships vals first)
        {:keys [ship] :as response} (orbit! ship)]
    (is (= #{:ship :nav} (-> response keys set)))
    (is (= "IN_ORBIT" (-> ship :nav :status)))))

;; (deftest refine!-test
;; (deftest chart!-test

(deftest dock!-test
  (let [ship (-> @state :ships vals first)
        {:keys [ship] :as response} (dock! ship)]
    (is (= #{:ship :nav :waypoint} (-> response keys set)))
    (is (= "DOCKED" (-> ship :nav :status)))))

;; (deftest survey!-test
;; (deftest extract!-test
;; (deftest jettison!-test
;; (deftest jump!-test
;; (deftest navigate!-test
;; (deftest flight-mode!-test
;; (deftest warp!-test
;; (deftest sell-cargo!-test
;; (deftest scan-systems!-test
;; (deftest scan-waypoints!-test
;; (deftest scan-ships!-test
;; (deftest refuel!-test
;; (deftest purchase-cargo!-test
;; (deftest transfer!-test
;; (deftest negotiate-contract!-test
;; (deftest deliver-contract!-test
;; (deftest accept-contract!-test
;; (deftest fulfill-contract!-test
;; (deftest install-mount!-test
;; (deftest remove-mount!-test

;; (deftest sell-all-cargo!-test
