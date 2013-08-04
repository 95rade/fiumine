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
  (let [pipe-in (java.io.PipedOutputStream.)
        pipe-out (java.io.PipedInputStream. pipe-in)]
    (future
      (loop [remaining files]
        (when (not (empty? remaining))
          (let [file (first remaining)
                path (.getAbsolutePath file)
                converter (sh/proc "sox" path "-t" "ogg" "-")]
            (pipe-stream (:out converter) pipe-in))
          (recur (rest remaining)))))
    pipe-out))
