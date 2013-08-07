(ns fiumine.station
  "Composes an ogg/vorbis stream from a sequence of audio files."
  (:require [fiumine.utils :as utils]
            [me.raynes.conch.low-level :as sh]))

(defn pipe-stream
  [from to]
  (let [buffer (byte-array 1024)]
    (loop []
      (let [n (.read from buffer)]
        (when (not= n -1)
          (.write to buffer 0 n)
          (recur))))))

(defn start-station
  "Takes a sequence of audio files as an input and returns an input stream which
   provides an ogg/vorbis stream."
  [files]
  (let [encoder (sh/proc "oggenc" "--raw" "-")]
    (future
      (try
        (loop [remaining files]
          (when (not (empty? remaining))
            (let [file (first remaining)
                  path (.getAbsolutePath file)
                  decoder (sh/proc "sox" path "-t" "raw" "-")]
              (println path)
              (pipe-stream (:out decoder) (:in encoder)))
            (recur (rest remaining))))
        (.close (:in encoder))
        (catch Throwable e 
          (prn e)
          (throw e))))
    (:out encoder)))
