(ns genqref-lib.throttling
  "https://github.com/SpaceTradersAPI/api-docs/wiki/Ratelimit"
  (:require [clojure.core.async :refer [>! <! >!! <!! go chan sliding-buffer alts! timeout]]))

(defn- fill [channel n token]
  (go
    (dotimes [i n]
      (>! channel (if (zero? i) token :other)))))

(defn throttle-chan [input-channel {:keys [regular-count regular-timeout burst-count burst-timeout]}]
  (let [output-channel (chan)
        fill-regular-channel (chan)
        fill-burst-channel (chan)
        regular-channel (chan (sliding-buffer regular-count))
        burst-channel (chan (sliding-buffer burst-count))]
    (go
      (while true
        (fill regular-channel regular-count :regular)
        (<! fill-regular-channel)
        (<! (timeout regular-timeout))))
    (go
      (while true
        (fill burst-channel burst-count :burst)
        (<! fill-burst-channel)
        (<! (timeout burst-timeout))))
    (go
      (while true
        (let [msg (<! input-channel)
              [token _] (alts! [regular-channel burst-channel] :priority true)]
          (>! output-channel msg)
          (case token
            :regular (>! fill-regular-channel true)
            :burst (>! fill-burst-channel true)
            :default))))
    output-channel))

(defn throttle-fn [f opts]
  (let [in (chan 1)
        out (throttle-chan in opts)]
    (fn [& args]
      (>!! in true)
      (<!! out)
      (apply f args))))

#_(defn f [x]
    (println "Hey I'm function call" x))
#_(def f* (throttle-fn f {:regular-count 2
                          :regular-timeout 1000
                          :burst-count 10
                          :burst-timeout 10000}))
#_(f* 1)
#_(dotimes [x 35] (f* x))
