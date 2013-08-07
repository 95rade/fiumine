(ns fiumine.service
  "Web service."
  (:require [ring.adapter.jetty :as jetty]
            [ring.util.io :as io]
            [fiumine.publisher :as publisher]))

(defn- handler [request]
  (let [pages @(publisher/subscribe)]
    {:status 200
     :headers {"Content-Type" "application/ogg"}
     :body (io/piped-input-stream 
             (fn [out]
               (loop [pages pages]
                 (when-let [page (first pages)]
                   (.write out (:page-data page))
                   (recur (rest pages))))))}))

(defn start [port]
  (jetty/run-jetty handler {:port port :max-threads 5000}))
