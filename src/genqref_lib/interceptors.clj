(ns genqref-lib.interceptors
  (:require [sieppari.core :as s]
            [taoensso.timbre :as timbre]
            [genqref-lib.interceptors
             [diligence :as diligence]
             [log-for-humans :as log-for-humans]
             [maintain-cache :as maintain-cache]
             [report-to-syndicate :as report-to-syndicate]]))

;;; interceptor facilitation

(def ^:private interceptors (atom []))

(defn register! [& is]
  (swap! interceptors concat (flatten is)))

;; TODO: fix the test stack and replace this with `(comp :data :body)`
(defn unpack-data-with-testhack [{:keys body}]
  ;; in out app stack the data is autmatically parsed
  (or (-> body :data)
      ;; in out test stack it needs to be parse here
      (-> body (json/parse-string keyword) :data)))

(def success? #{200 201 204})

(defn throw-api-error [{:keys [status message code data]}]
  (throw (ex-info message (assoc data :code code :status status))))

(def request-handler [{:keys [operation params]}]
  (let [{:keys [status] :as response}
        (safe-deref (martian/response-for *api* operation params))]
    (if (success? status)
      (unpack-data-with-testhack response)
      (-> response :body :error (assoc :status status) throw-api-error))))

;; NOTE: this is the previous q
(defn request [operation params]
  (let [req {:operation operation :params params}]
    (s/execute (concat @interceptors [request-handler]) req)))

;;; interceptors

(def log-errors
  {:error
   (fn [{:keys [error] :as ctx}]
     (timbre/error "API Error" (-> error ex-data :code) (ex-message error))
     ctx)})

(defn auto-reset [reset-fn]
  {:error
   (fn [{:keys [error] :as ctx}]
     (when (-> error ex-data :status #{401})
       (timbre/warn "Try to recover from auth error by triggering reset function.")
       ;; TODO: maybe terminate interceptor chain at this point
       (reset-fn))
     ctx)})

(def stalling-retry
  {:error
   (fn [{:keys [error] {:keys [operation params]} :request :as ctx}]
     ;; stall current thread for timeout
     (util/sleep
      (case (-> error ex-data :status)
        ;; Exceeded the rate limit.
        429 (-> error ex-data :retryAfter (or 2))
        ;; "The server encountered a temporary error and could
        ;; not complete your request. Please try again in 30
        ;; seconds."
        502 30
        ;; Service unavailable (503)
        503 120))
     ;; repeat the request (recursively) and use its response
     (assoc ctx :response (request operation params)))})

;; TODO: scheduled-retry for some recoverable errors

(comment
  (report-to-syndicate/set-token! "asdf")

  (def standard-interceptors
    [stalling-retry
     (auto-reset full-reset!)
     log-errors
     (log-for-humans/interceptor {:before-fn log/trace
                                  :after-fn log/debug
                                  :error-fn log/error})
     (diligence/interceptor log/warn)
     report-to-syndicate/interceptor
     (maintain-cache/interceptor state)])

  (register! standard-interceptors)

  ;; TODO: write macro for this an generate it
  (defn purchase-ship! [shipyard ship-type]
    (request :purchase-ship {:shipyard shipyard
                             :ship-type ship-type}))

  (defn orbit!* [ship]
    (request :orbit-ship {:ship ship}))

  (defn dock!* [ship]
    (request :dock-ship {:ship ship}))
  )
