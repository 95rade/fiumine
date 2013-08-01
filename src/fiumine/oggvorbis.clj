(ns fiumine.oggvorbis
  "Provides some logical chunking of ogg vorbis streams."
  (:require [marshal.core :as marsh])
  )

(defn scan-for-page
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

(def ^:const max-ogg-header-size (+ 0xff 28))

(def ogg-page-struct
  (marsh/struct 
    :capture-pattern (marsh/ascii-string 4)
    :structure-revision marsh/ubyte
    :flags marsh/ubyte
    :position marsh/sint64
    :serial-number marsh/uint32
    :sequence-number marsh/uint32
    :crc-checksum marsh/uint32
    :n-page-segments marsh/ubyte))

(defn ogg-page
  "Loads a single page from an Ogg stream into a byte array, returning the byte
   array, or nil if at end of stream.  Returns a map of the marshalled structure
   for the Ogg page.  The raw data appears as a byte array at the :page-data key
   of the returned struct.  The offset in page data to packet data is found in 
   the :packet-offset key.
  
   See: http://www.xiph.org/vorbis/doc/framing.html"
  [stream]
  (if (scan-for-page stream)
    (do
      (.mark stream max-ogg-header-size)
      (let [page (marsh/read stream ogg-page-struct)
            n-page-segments (:n-page-segments page)
            segments-struct (marsh/struct :segments
                              (marsh/array marsh/ubyte n-page-segments))
            segments (:segments (marsh/read stream segments-struct))
            data-size (apply + segments)
            packet-offset (+ n-page-segments 27)
            page-size (+ data-size packet-offset)
            buffer (byte-array page-size)]
        (.reset stream)
        (.read stream buffer)
        (assoc page 
               :page-data buffer 
               :packet-offset packet-offset
               :segments segments)))
    nil))
