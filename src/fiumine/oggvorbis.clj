(ns fiumine.oggvorbis
  "Provides some logical chunking of ogg vorbis streams."
  )

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
   array, or nil if at end of stream.
  
   See: http://www.xiph.org/vorbis/doc/framing.html"
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

(defn- read-int-little-endian
  "Reads an arbitrary sequence of bytes in little endian (least significant 
   first) byte order and returns an int."
  [bs]
  (loop [remaining bs, shift 0, n 0] 
    (if (empty? remaining) n
      (recur (rest remaining) (+ shift 8) 
             (+ n (bit-shift-left (first remaining) shift))))))

(defn packet-type
  "Assumes page data begins a new packet and returns the integer type of the
   packet. This is not a valid assumption in general, but in practice this is a
   useful function for finding the start of audio data in a stream, since there
   are usually a handful of header pages and then the first audio packet starts
   at the very beginning of the first non-header page."
  [page]
  (let [page-segments (aget page 26)]
    (aget page (+ 27 page-segments))))

(defn audio? 
  "See comment about 'packet-type' above."
  [page]
  (= (packet-type page) 0))
