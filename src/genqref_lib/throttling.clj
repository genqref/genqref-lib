(ns genqref-lib.throttling
  "https://github.com/SpaceTradersAPI/api-docs/wiki/Ratelimit"
  (:require [clojure.core.async :refer [>! <! >!! <!! go chan sliding-buffer alts! timeout]]
            [taoensso.timbre :refer [info]]))

(defn- fill [channel n token]
  ;; (info "<<< FILLING CHANNEL WITH" n token "TOKENS")
  (go
    (dotimes [i n]
      (>! channel (if (zero? i) token :other)))))

(defn throttle-chan [input-channel {:keys [regular-count regular-timeout burst-count burst-timeout grace] :or {grace 1}}]
  (let [output-channel (chan)
        fill-regular-channel (chan)
        fill-burst-channel (chan)
        done-channel (chan)
        regular-channel (chan (sliding-buffer regular-count))
        burst-channel (chan (sliding-buffer burst-count))]
    (go
      (while true
        (let [token (<! done-channel)]
          ;; (info ">>> RECEIVED TOKEN" token "FROM DONE CHANNEL")
          (case token
            :regular (>! fill-regular-channel true)
            :burst (>! fill-burst-channel true)
            :default))))
    (go
      (while true
        ;; (info "+++ TIMEOUT REACHED TO REFILL regular")
        (fill regular-channel regular-count :regular)
        (<! fill-regular-channel)
        ;; (info "+++ STARTING TIMER" regular-timeout "TO REFILL regular")
        (<! (timeout (* grace regular-timeout)))))
    (go
      (while true
        ;; (info "+++ TIMEOUT REACHED TO REFILL burst")
        (fill burst-channel burst-count :burst)
        (<! fill-burst-channel)
        ;; (info "+++ STARTING TIMER" burst-timeout "TO REFILL burst")
        (<! (timeout (* grace burst-timeout)))))
    (go
      (while true
        ;; (info "--- WAITING FOR REQUEST")
        (<! input-channel)
        ;; (info "--- RECEIVED REQUEST")
        (let [[token ch] (alts! [regular-channel burst-channel] :priority true)]
          ;; (info "--- ACQUIRED TOKEN" token)
          (>! output-channel token))))
    [output-channel done-channel]))

(def queue (atom 0))

(defn throttle-fn [f opts]
  (let [in (chan 1)
        [out done] (throttle-chan in opts)]
    (fn [& args]
      (swap! queue inc)
      (let [coid (rand-int 1000000)]
      ;; (info "[" coid "] ==================================== QUEUED:" @queue)
      ;; (info "[" coid "] SENDING REQUEST TOKEN...")
      (>!! in true)
      ;; (info "[" coid "] SENT REQUEST TOKEN, WAITING...")
      (let [token (<!! out)
            ;; _ (info "[" coid "] DONE WAITING, RECEIVED TOKEN:" token)
            result (apply f args)]
        ;; (info "[" coid "] FN CALL COMPLETE, SENT DONE TOKEN:" token)
        (>!! done token)
        (swap! queue dec)
        result)))))

#_(defn f [x]
    (println "Hey I'm function call" x))
#_(def f* (throttle-fn f {:regular-count 2
                          :regular-timeout 1000
                          :burst-count 10
                          :burst-timeout 10000}))
#_(f* 1)
#_(dotimes [x 35] (f* x))
