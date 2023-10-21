(ns genqref-lib.interceptors.report-to-syndicate
  (:require [org.httpkit.client :as http]
            [cheshire.core :as json]))

(def ^:dynamic *token* nil)

(defn set-token! [token]
  (alter-var-root #'*token* (constantly token)))

(def ^:dynamic *endpoint* "https://api.genqref.com/")

(defn set-endpoint! [endpoint]
  (alter-var-root #'*endpoint* (constantly endpoint)))

(defn- url [ctx resource]
  (str *endpoint* ctx "/observations/" (name resource)))

(defn- api-post [ctx resource payload]
  (let [payload (-> payload vector flatten)]
    (http/post (url ctx resource)
               {:headers {"Content-type" "application/json"
                          "Authorization" (str "Token " *token*)}
                :body (json/generate-string payload)})))

(defmulti transform (fn [key _] key))

(defmethod transform :default [_ _]
  nil)

(defmethod transform :transactions [_ transactions]
  transactions)

(def interceptor
  {:leave
   (fn [{:keys [response] :as ctx}]
     (future
       (doseq [[key value] response]
         (when-let [value (transform key value)]
           (api-post key value)))))})
