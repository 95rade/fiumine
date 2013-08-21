(ns fiumine.metadata
  "Retrieve metadata about audio files."
  (:require [me.raynes.conch :as sh]
            [fiumine.utils :as utils]))

(sh/programs id3v2)
(sh/programs metaflac)
(sh/programs vorbiscomment)

(defn- id3value
  [line]
  (subs line (+ (.indexOf line ":") 2)))

(defn- mp3-metadata
  [file]
  (reduce (fn [md line]
    (cond
      (.startsWith line "TIT2") (assoc md :title (id3value line))
      (.startsWith line "TPE1") (assoc md :artist (id3value line))
      (.startsWith line "TALB") (assoc md :album (id3value line))
      :else md))
    {} (id3v2 "--list" (.getPath file) {:seq true})))

(defn- vorbis-metadata
  [file]
  (reduce 
    (fn [md line]
      (let [lower (clojure.string/lower-case line)]
        (cond
          (.startsWith lower "title=") (assoc md :title (subs line 6))
          (.startsWith lower "artist=") (assoc md :artist (subs line 7))
          (.startsWith lower "album=") (assoc md :album (subs line 6))
          :else md)))
        {} (vorbiscomment (.getPath file) {:seq true})))

(defn- flac-metadata
  [file]
  (reduce 
    (fn [md line]
      (let [lower (clojure.string/lower-case line)]
        (cond
          (.startsWith lower "title=") (assoc md :title (subs line 6))
          (.startsWith lower "artist=") (assoc md :artist (subs line 7))
          (.startsWith lower "album=") (assoc md :album (subs line 6))
          :else md)))
        {} (map #(clojure.string/replace % #".+comment\[\d\]: " "") 
                (metaflac "--list" (.getPath file) {:seq true}))))

(defn metadata
  "Given an audio file, returns a map of :artist :album :title."
  [file]
  (let [ext (utils/file-extension file)]
    (cond 
      (= ext ".mp3") (mp3-metadata file)
      (= ext ".ogg") (vorbis-metadata file)
      (= ext ".flac") (flac-metadata file)
      :else (throw (Exception. (format "Unknown audio file type: %s" ext))))))
