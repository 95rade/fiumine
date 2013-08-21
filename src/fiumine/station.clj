(ns fiumine.station
  "Composes an ogg/vorbis stream from a sequence of audio files."
  (:require [clojure.java.io :as io]
            [fiumine.metadata :as md]
            [fiumine.utils :as utils]
            [me.raynes.conch.low-level :as sh]))

(defn pipe-stream
  [from to]
  (let [buffer (byte-array 4096)]
    (loop []
      (let [n (.read from buffer)]
        (when (not= n -1)
          (.write to buffer 0 n)
          (recur))))))

(defn start-station
  "Takes a sequence of audio files as an input and returns an input stream which
   provides an ogg/vorbis stream."
  [files]
  (let [out (java.io.PipedInputStream.)
        pipe-in (java.io.PipedOutputStream. out)]
    (future
      (try
        (loop [remaining files]
          (when (not (empty? remaining))
            (let [file (first remaining)
                  info (md/metadata file)
                  path (.getPath file)
                  decoder (sh/proc "sox" path "-t" "raw" "-")
                  encoder (sh/proc "oggenc" "--raw" "--quiet" 
                                   "-c" (str "title=" (:title info))
                                   "-c" (str "artist=" (:artist info))
                                   "-c" (str "album=" (:album info)) "-")]
              (println (format "%s by %s (%s)" (:title info) (:artist info)
                               (:album info)))
              (future 
                (io/copy (:out decoder) (:in encoder) :buffer-size 4096)
                (.close (:in encoder)))
              (pipe-stream (:out encoder) pipe-in))
            (recur (rest remaining))))
        (.close pipe-in)
        (catch Throwable e
          (.printStackTrace e)
          (throw e))))
    (java.io.BufferedInputStream. out)))

