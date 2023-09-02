(ns genqref-lib.helper
  (:require [genqref-lib.api :refer [state]]
            [genqref-lib.util :as util :refer [sym]]))

(defn markets-in-system [system]
  (->> @state :markets vals (filter #(-> % :symbol util/parse-system #{(sym system)}))))
