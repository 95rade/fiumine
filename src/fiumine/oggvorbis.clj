(ns fiumine.oggvorbis
  "Provides some logical chunking of ogg vorbis streams."
  )

(declare ogg-page!)

(defn oggvorbis-stream
  "???"
  [stream]
  (when-let [page (ogg-page! stream)]
    (cons page (lazy-seq (oggvorbis-stream stream)))))

(defn scan-for-page!
  "Scans ahead in Ogg Vorbis stream looking for capture sequence. After calling
   this function, the stream will either be at the beginning of an Ogg page, or
   at the end of the stream."
  [stream]
  (.mark stream 4)
  (loop [run 0]
    (let [ch (.read stream)
          capture-sequence (map int "OggS")]
      (if (= ch -1) 
        nil
        (if (= run 4)
          (do 
            (.reset stream)
            stream)
          (if (= ch (nth capture-sequence run))
            (recur (inc run))
            (do
              (.mark stream 4)
              (recur 0))))))))

(def max-ogg-header-size (+ 0xff 28))

(defn ogg-page!
  "Loads a single page from an Ogg stream into a byte array, returning the byte
   array, or nil if at end of stream."
  [stream]
  (if (scan-for-page! stream)
    (do
      (.mark stream max-ogg-header-size)
      ; Skip first 26 bytes
      (dotimes [_ 26] (.read stream))
      (let [page-segments (.read stream)
            data-size (apply + (repeatedly page-segments #(.read stream)))
            page-size (+ data-size page-segments 27)
            buffer (byte-array page-size)]
        (.reset stream)
        (.read stream buffer)
        buffer))
    nil))
