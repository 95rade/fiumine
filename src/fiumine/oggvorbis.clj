(ns fiumine.oggvorbis
  "Provides some logical chunking of ogg vorbis streams."
  (:require [clojure.java.io :as io]
            [marshal.core :as marsh])
  )

(def ^:const ogg-capture-pattern "OggS")
(def ^:const max-ogg-header-size (+ 0xff 28))
(def ^:const ogg-revision 0)

(defn scan-for-page
  "Scans ahead in Ogg Vorbis stream looking for capture sequence. After calling
   this function, the stream will either be at the beginning of an Ogg page, or
   at the end of the stream."
  [stream]
  (.mark stream 4)
  (loop [run 0]
    (let [ch (.read stream)
          capture-sequence (map int ogg-capture-pattern)]
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

(def ogg-page-struct
  (marsh/struct 
    :capture-pattern (marsh/ascii-string 4)
    :ogg-revision marsh/ubyte
    :flags marsh/ubyte
    :position marsh/sint64
    :serial-number marsh/uint32
    :sequence-number marsh/uint32
    :crc-checksum marsh/uint32
    :n-page-segments marsh/ubyte))

(defprotocol OggPageProtocol
  (verify-crc [self]))

(defrecord OggPage [capture-pattern
                    ogg-revision
                    flags 
                    position 
                    serial-number
                    sequence-number
                    crc-checksum
                    n-page-segments
                    page-data
                    packet-offset
                    segments
                    frames]
  OggPageProtocol
  (verify-crc [self] true))

(defn read-page
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
      (let [page (merge (map->OggPage {}) (marsh/read stream ogg-page-struct))
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
        (let [page (assoc page :page-data buffer 
                               :packet-offset packet-offset
                               :segments segments)]
          (assert (= (:capture-pattern page) ogg-capture-pattern) "Not an Ogg page")
          (assert (= (:ogg-revision page) ogg-revision) "Unknown Ogg revision")
          (assert (verify-crc page) "Corrupt Ogg page")
          page)))
    nil))

(defn- partition-packet-segments
  [segments]
  (loop [partitions [] part [] remaining segments]
    (if (empty? remaining) 
      (filter (complement empty?) (conj partitions part))
      (let [segment (first remaining)]
        (if (= segment 0xff)
          (recur partitions (conj part segment) (rest remaining))
          (recur (conj partitions (conj part segment)) [] (rest remaining)))))))

(defn vorbis-packets
  "Marshals vorbis packets from an ogg page."
  [page]
  (let [partitions (partition-packet-segments (:segments page))
        sizes (map #(apply + %) partitions)
        stream (io/input-stream (:page-data page))]
    (.skip stream (:packet-offset page))
    (map (fn [size] 
           (let [buffer (byte-array size)] 
             (.read stream buffer)
             buffer)) sizes)))

(defn audio?
  "Returns true if a vorbis packet is an audio packet."
  [packet]
  ; Least significant bit is zero for audio
  (even? (first packet)))

(def vorbis-id-struct
  (marsh/struct
    :type marsh/ubyte
    :sentinel (marsh/ascii-string 6)
    :version marsh/uint32
    :channels marsh/ubyte
    :framerate marsh/uint32))

(defn vorbis-id
  "Decodes id header, returning a map with the id structure.
   See: http://xiph.org/vorbis/doc/Vorbis_I_spec.html#x1-600004.2"
  [packet]
  (let [stream (io/input-stream packet)
        id (marsh/read stream vorbis-id-struct)]
    (assert (= (:type id) 1))
    (assert (= (:sentinel id) "vorbis"))
    (assert (= (:version id) 0))
    id))
