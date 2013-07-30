(ns fiumine.oggvorbis
  "Provides some logical chunking of ogg vorbis streams."
  )

(declare scan-for-page!)

(defn oggvorbis-stream
  "???"
  [stream]
  "???"
  )

(def ogg-capture (map int "Oggs"))
(def read-ahead 4096) ; SWAG

(defn scan-for-page!
  "Scans ahead in Ogg Vorbis stream looking for capture sequence"
  [stream]
  (loop [run 0]
    (let [ch (.read stream)]
      (if (= ch -1) 
        stream
        (if (= run 4)
          (do 
            (.reset stream)
            stream)
          (if (= ch (nth ogg-capture run))
            (recur (inc run))
            (do
              (.mark stream read-ahead)
              (recur 0))))))))
