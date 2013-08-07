(ns fiumine.service
  "Web service."
  (:require [ring.adapter.jetty :as jetty]
            [ring.util.io :as io]
            [fiumine.publisher :as pub]))

(defn- stream-pages
  "Returns a function to be called by Ring's piped input stream implementation
   which writes pages to the response output stream."
  [pages]
  (fn [out]
    (try
      (fn [out]
        (loop [pages pages]
          (when-let [page (first pages)]
            (.write out (:page-data page))
            (recur (rest pages)))))
      (catch Exception e 
        (prn e) 
        (throw e)))))

(defn- make-handler [publisher]
  (fn [request]
    (let [pages @(pub/subscribe publisher)]
      {:status 200
       :headers {"Content-Type" "application/ogg"}
       :body (io/piped-input-stream (stream-pages pages))})))
    
(defn start [publisher port]
  (jetty/run-jetty (make-handler publisher) {:port port :max-threads 1000}))
