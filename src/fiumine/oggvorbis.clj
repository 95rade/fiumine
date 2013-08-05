(ns fiumine.oggvorbis
  "Provides some logical chunking of ogg vorbis streams."
  (:require [clojure.java.io :as io]
            [fiumine.oggcrc :as crc]
            [marshal.core :as marsh])
  (:import java.io.ByteArrayOutputStream))

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

(defn verify-crc 
  "Calculates the CRC checksum for a page and verifies that it matches the
   the value of :crc-checksum in the page. Returns a boolean."
  [page] 
  ; Make copy of page data and set crc-checksum field to all zeros
  (let [bs (byte-array (:page-data page))]
    (aset-byte bs 22 0)
    (aset-byte bs 23 0)
    (aset-byte bs 24 0)
    (aset-byte bs 25 0)
    (= (:crc-checksum page) (crc/calculate-crc bs))))

(defn- marshal-page
  "Writes page data from the page dict back to the raw data byte array."
  [page data]
  (let [stream (ByteArrayOutputStream.)]
    (marsh/write stream ogg-page-struct page)
    (let [marshaled (.toByteArray stream)]
      (dorun 
        (map #(aset-byte data % (aget marshaled %)) 
             (range (alength marshaled)))))))

(defn modify-page
  "Updates the page from the given dictionary and recalculates the CRC 
  checksum."
  [page dict]
  (let [data (byte-array (:page-data page))
        page (merge page dict {:crc-checksum 0 :page-data data})]
    (marshal-page page data)
    (let [crc-checksum (crc/calculate-crc data)
          page (assoc page :crc-checksum crc-checksum)]
      (marshal-page page data)
      page)))

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
