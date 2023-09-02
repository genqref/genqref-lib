(ns genqref-lib.scheduler
  (:require [genqref-lib.time :as t]
            [genqref-lib.util :as util]
            [taoensso.timbre :as timbre :refer [debug trace]])
  (:gen-class))

;; TODO: maybe make this a duratom
(defonce ^:private schedule (atom {}))

(defn- due-action? [[[ts _] f]]
  (pos? (compare (t/now) ts)))

(defonce scheduler
  (util/safe-future
   (do
     (debug "Starting scheduler at UTC:" (t/now))
     (while true
       (util/sleep 1)
       (let [due (filter due-action? @schedule)]
         (doseq [[token f] due]
           (do
             (trace "Perform for" token)
             (util/safe-future (f))
             (swap! schedule dissoc token))))))))

;; TODO: this works but should probably be a macro instead so we can
;; retain a human readable and more important seriazable version of
;; `f`
(defn schedule! [time f]
  (trace "Scheduled for" time)
  (swap! schedule assoc [time (rand)] f))

(defn cancel-everything! []
  (reset! schedule {}))

#_(cancel-everything!)
