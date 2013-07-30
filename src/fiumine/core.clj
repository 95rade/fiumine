(ns fiumine.core
  "Simple music streamer."
  (:gen-class))

(declare audio-file?)
(declare file-extension)
(declare get-audio-files)

(defn -main
  [path & args]
  (let [folder (clojure.java.io/file path)]
    (doseq [fname (get-audio-files folder)]
      (println fname))))

(defn get-all-files
  "Recursively get all files in folder, returns a tree structure."
  [folder]
  (defn visit [f]
    (if (.isDirectory f)
      (get-all-files f)
      f))
  (map visit (filter (partial not= folder) (file-seq folder))))

(defn get-audio-files
  "For a given folder find all the files that look like audio files."
  [folder]
  (filter audio-file? (flatten (get-all-files folder))))

(def audio-file-extensions #{".flac" ".ogg" ".mp3"})

(defn audio-file?
  "Returns true if file's extension matches a known audio type."
  [file]
  (contains? audio-file-extensions (file-extension (.getName file))))

(defn file-extension
  "Returns a file's file extension or nil if none is found."
  [fname]
  (re-find #"\.[^.]+$" (clojure.string/lower-case fname)))

