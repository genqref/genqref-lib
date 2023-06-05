(ns genqref-lib.time
  (:import java.time.format.DateTimeFormatter
           java.time.LocalDateTime
           java.time.ZoneId
           java.time.ZoneOffset))

(defn now-ts []
  (quot (System/currentTimeMillis) 1000))

#_(now-ts)

(def iso8601  (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"))

(defn now []
  (.format (LocalDateTime/now (ZoneId/of "UTC")) iso8601))

#_(now)

(defn in [seconds]
  (.format (LocalDateTime/ofEpochSecond (+ (now-ts) seconds) 0 (ZoneOffset/of "+0")) iso8601))

#_(in 1800)
