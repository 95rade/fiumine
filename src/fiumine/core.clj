(ns fiumine.core
  "Simple music streamer."
  (:require [fiumine.station :as station]
            [fiumine.utils :as utils])
  (:gen-class))

(defn get-all-files
  "Recursively get all files in folder, returns a tree structure."
  [folder]
  (defn visit [f]
    (if (.isDirectory f)
      (get-all-files f)
      f))
  (map visit (filter (partial not= folder) (file-seq folder))))

(def audio-file-extensions #{".flac" ".ogg" ".mp3"})

(defn audio-file?
  "Returns true if file's extension matches a known audio type."
  [file]
  (contains? audio-file-extensions (utils/file-extension file)))

(defn get-audio-files
  "For a given folder find all the files that look like audio files."
  [folder]
  (filter audio-file? (flatten (get-all-files folder))))

(defn -main
  [path & args]
  (let [folder (clojure.java.io/file path)
        audio-files (shuffle (get-audio-files folder))
        station (station/start-station audio-files)]
      (clojure.java.io/copy station (clojure.java.io/output-stream "fufu"))))
