(ns genqref-lib.interceptors.maintain-cache
  "the design is optimized to have as few swap!s as possible, can be
  used with anything that behaves like an atom, i.e. is swapable, e.g.
  a duratom"
  (:require [genqref-lib.util :as util :refer [sym]]))

(defmulti update-cache (fn [key _] key))

(defmethod update-cache :default [_ _]
  identity)

(defmethod update-cache :agent [_ agent]
  #(update % :agent util/deep-merge agent))

(defmethod update-cache :ship [_ ship]
  ;; this is a new ship, so assoc-in is ok in this case
  #(update % :ships assoc (keyword (sym ship)) ship))

(defn interceptor [atomic]
  {:leave (fn [{:keys [request response] :as ctx}]
            (->> response
                 (util/map-kv update-cache)
                 (apply comp) ;; ◕_◕
                 (swap! atomic))
            ctx)})
