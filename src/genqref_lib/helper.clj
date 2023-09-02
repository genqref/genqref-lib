(ns genqref-lib.helper
  (:require [genqref-lib.api :refer [state]]
            [genqref-lib.util :as util :refer [sym]]))

(defn markets-in-system [system]
  ;; why are there nils in there?
  (->> @state :markets vals (remove nil?) (filter #(-> % :symbol util/parse-system #{(sym system)}))))
