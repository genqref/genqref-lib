(ns genqref-lib.time
  (:import java.time.format.DateTimeFormatter
           java.time.LocalDateTime
           java.time.ZoneId))

(defn now-ts []
  (quot (System/currentTimeMillis) 1000))

#_(now-ts)

(def iso8601  (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"))

(defn now []
  (.format (LocalDateTime/now (ZoneId/of "UTC")) iso8601))

#_(now)
