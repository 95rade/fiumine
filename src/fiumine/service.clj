(ns fiumine.service
  "Web service."
  (:require [ring.adapter.jetty :as jetty]
            [ring.util.io :as io]
            [fiumine.publisher :as publisher]))

(defn- make-handler [published]
  (defn- handler [request]
    (let [pages @(publisher/subscribe published)]
      {:status 200
       :headers {"Content-Type" "application/ogg"}
       :body (io/piped-input-stream 
               (fn [out]
                 (loop [pages pages]
                   (when-let [page (first pages)]
                     (.write out (:page-data page))
                     (recur (rest pages))))))})))

(defn start [published port]
  (jetty/run-jetty (make-handler published) {:port port :max-threads 5000}))
