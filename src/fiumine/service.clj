(ns fiumine.service
  "Web service."
  (:require [ring.adapter.jetty :as jetty]
            [ring.util.io :as io]
            [fiumine.publisher :as pub]))

(defn- make-handler [publisher]
  (defn- handler [request]
    (let [pages @(pub/subscribe publisher)]
      {:status 200
       :headers {"Content-Type" "application/ogg"}
       :body (io/piped-input-stream 
               (fn [out]
                 (loop [pages pages]
                   (when-let [page (first pages)]
                     (.write out (:page-data page))
                     (recur (rest pages))))))})))

(defn start [publisher port]
  (jetty/run-jetty (make-handler publisher) {:port port :max-threads 1000}))
