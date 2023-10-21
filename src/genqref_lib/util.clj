(ns genqref-lib.util
  (:require [clojure.string :as str]
            [cheshire.core :as json]
            [taoensso.timbre :as timbre :refer [error]]
            [genqref-lib.time :as t]))

;;; generic clojure lang stuff

(defn index-by [key coll]
  (reduce #(assoc %1 (key %2) %2) {} coll))

(defn sleep [seconds]
  (Thread/sleep (* 1000 seconds)))

(defmacro safe-future [body]
  `(future
     (try ~body
          (catch Exception e#
            (error "Error:" e#)))))

(defn deep-merge [& maps]
  (apply merge-with (fn [& args]
                      (if (every? map? args)
                        (apply deep-merge args)
                        (last args)))
         maps))

(defn map-kv [f coll]
  (reduce-kv #(conj %1 (f %2 %3)) [] coll))

;;; generic math stuff

(defn square [x]
  (* x x))

(defn sqrt [x]
  (Math/sqrt x))

(defn distance [waypoint¹ waypoint²]
  (sqrt (+ (square (- (:x waypoint¹) (:x waypoint²)))
           (square (- (:y waypoint¹) (:y waypoint²))))))

(defn cartesian [colls]
  (if (empty? colls)
    '(())
    (for [more (cartesian (rest colls))
          x (first colls)]
      (cons x more))))

;;; duratom specific

(def json-rw
  {:read (comp #(json/parse-string % true) slurp)
   :write #(spit %1 (json/generate-string %2))})

;;; space traders specific

(defn sym [x]
  (or (:symbol x) x))

;; TODO: rename to `system`
(defn parse-system [waypoint]
  (->> (str/split (sym waypoint) #"-")
       butlast
       (str/join "-")))

(defn valid-survey? [{:keys [expiration] :as survey}]
  (pos? (compare expiration (t/now))))

(defn full? [{:keys [cargo] :as ship}]
  (and (= (-> cargo :capacity)
          (-> cargo :units))
       (->> cargo :inventory (map (juxt :symbol :units)))))

;; TODO: maybe rename to `remaining-cpacity`
(defn capacity [{:keys [cargo] :as ship}]
  (- (cargo :capacity) (cargo :units)))

(defn units [ship resource-symbol]
  (or (->> ship
           :cargo
           :inventory
           (filter #(-> % :symbol (= resource-symbol)))
           first
           :units) 0))

(defn arrival [ship]
  (-> ship :nav :route :arrival))
