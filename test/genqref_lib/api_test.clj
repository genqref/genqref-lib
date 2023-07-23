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

(defn record-api []
  (martian-http/bootstrap-openapi
   "spacetraders.v2.json" ;; use a bundled resource instead
   {:interceptors
    (-> [(#'api/add-authentication-header (slurp "test-token.txt"))]
        (concat martian-http/default-interceptors)
        (inject (vcr/playback vcr-opts)
                :before (:name martian-http/perform-request)))}))

(defn playback-api []
  (martian-http/bootstrap-openapi
   "spacetraders.v2.json" ;; use a bundled resource instead
   {:interceptors
    (-> [(#'api/add-authentication-header (slurp "test-token.txt"))]
        (concat martian-http/default-interceptors)
        (inject (vcr/playback vcr-opts)
                :replace (:name martian-http/perform-request)))}))

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

(binding [*api* (test-api)]
  (refresh! :ships))

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

(deftest get-ships-test
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
