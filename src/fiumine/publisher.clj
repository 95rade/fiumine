(ns fiumine.publisher
  "Publishes ogg/vorbis stream to any listening subscribers."
  (:require [fiumine.oggvorbis :as ogg])
  )

; Current header blocks will always be stored in this atom
(def header-blocks (atom nil))

; Current audio block will be continuously refreshed at this atom
(def audio-block (atom nil))

(defn- publish-stream
  "Publishes stream to global vars, assumes subscribers have registered watches 
   on 'audio-block'. Spins up a new thread."
  [stream]
  ; Start by grabbing header blocks
  (loop [headers []]
    (let [page (ogg/ogg-page stream)]
      (if (ogg/audio? page)
        (do
          (reset! header-blocks headers)
          (reset! audio-block page))
        (recur (conj header-blocks page)))))

  ; Pump out the audio in another thread
  (future 
    (loop []
      (when-let [page ogg/ogg-page stream]
        (do
          (reset! audio-block page)
          (recur))))))
