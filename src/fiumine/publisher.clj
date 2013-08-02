(ns fiumine.publisher
  "Publishes ogg/vorbis stream to any listening subscribers."
  (:require [fiumine.oggvorbis :as ogg]))

; Current header pages will always be stored in this atom
(def header-pages (atom nil))

; Current audio page will be continuously refreshed at this atom
(def audio-page (atom nil))

(defn sleep [millis]
  "Stub for unit tests."
  (Thread/sleep millis))

(defn- grab-headers
  "Grabs headers and first audio page from a stream. Returns 
  [header-pages first-page]."
  [stream]
  (loop [headers []]
    (let [page (ogg/ogg-page stream)
          packet (-> page ogg/vorbis-packets first)]
      (if (ogg/audio? packet)
        [headers page]
        (recur (conj headers page))))))

(defn publish-stream
  "Publishes stream to global vars, assumes subscribers have registered watches 
   on 'audio-page'. Spins up a new thread."
  [stream]
  ; Start by grabbing header pages
  (let [[headers first-page] (grab-headers stream)
        info (-> headers first ogg/vorbis-packets first ogg/vorbis-id)
        framerate (:framerate info)]
    (reset! header-pages headers)
    (future ; Pump out the audio in another thread
      (loop [page first-page last-position (:position page)]
        (when page
          (let [position (:position page)
                samples (- position last-position)
                time-millis (* 1000 (/ samples framerate))]
            (reset! audio-page page)
            (sleep time-millis)
            (recur (ogg/ogg-page stream) position)))))))
