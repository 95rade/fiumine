(ns fiumine.oggvorbis
  "Provides some logical chunking of ogg vorbis streams."
  (:require [clojure.java.io :as io]
            [fiumine.oggcrc :as crc]
            [marshal.core :as marsh])
  (:import java.io.ByteArrayOutputStream))

(def ^:const ogg-capture-pattern "OggS")
(def ^:const max-ogg-header-size (+ 0xff 28))
(def ^:const ogg-revision 0)

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

(def vorbis-id-struct
  (marsh/struct
    :type marsh/ubyte
    :sentinel (marsh/ascii-string 6)
    :version marsh/uint32
    :channels marsh/ubyte
    :framerate marsh/uint32))

(defn- read-into
  "Reads until byte array is full or end of stream is reached."
  [buffer stream]
  (let [len (alength buffer)]
    (loop [pos 0]
      (when (< pos len)
        (let [remainder (- len pos)
              n (.read stream buffer pos remainder)]
          (when (not= n -1)
            (recur (+ pos n))))))))

(defn- scan-for-page
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

(defn- verify-crc 
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

(defn- read-page
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
        (read-into buffer stream)
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

(defn packets
  "Marshals vorbis packets from an ogg page. Does not take into account packets
   split across pages.  The only packet we're interested in deocding, so far,
   is the id packet which seems to always be the one and only packet in the
   first page.  Anything more sophisticated will require extracting a packet
   stream from a page stream, taking into account that packets might span 
   page boundaries."
  [page]
  (let [partitions (partition-packet-segments (:segments page))
        sizes (map #(apply + %) partitions)
        stream (io/input-stream (:page-data page))]
    (.skip stream (:packet-offset page))
    (map (fn [size] 
           (let [buffer (byte-array size)] 
             (read-into buffer stream)
             buffer)) sizes)))

(defn audio?
  "Returns true if a vorbis packet is an audio packet."
  [packet]
  ; Least significant bit is zero for audio
  (even? (first packet)))

(defn header?
  "Returns true if page is a header page. The most surefire way to know is if 
   the position field is set to 0.  This is actuall safer than examining the
   packets to see if they are audio, since we don't yet take into account 
   packets which span page boundaries, so we might end up examining an 
   incomplete packet and get a spurious result."
  [page]
  (= (:position page) 0))

(def not-header? (complement header?))

(defn get-info
  "Decodes id header, returning a map with the id structure.
   See: http://xiph.org/vorbis/doc/Vorbis_I_spec.html#x1-600004.2"
  [packet]
  (let [stream (io/input-stream packet)
        id (marsh/read stream vorbis-id-struct)]
    (assert (= (:type id) 1))
    (assert (= (:sentinel id) "vorbis"))
    (assert (= (:version id) 0))
    (dissoc id :type :sentinel :version)))

(defn- grab-headers
  "Grabs headers and first audio page from a stream. Returns 
  [header-pages first-page]."
  [stream]
  (loop [headers []]
    (let [page (read-page stream)]
      (if (not-header? page)
        [headers page]
        (recur (conj headers page))))))

(defn- stream-pages
  "Lazy sequence of pages from stream."
  [stream]
  (when-let [page (read-page stream)]
    (cons page (lazy-seq (stream-pages stream)))))

(defn read-stream
  "Provides a structure for reading a stream a page at a time."
  [stream]
  (let [[headers first-page] (grab-headers stream)
        info (-> headers first packets first get-info)
        pages (concat (conj headers first-page) (stream-pages stream))]
    (assoc info :headers (ref headers)
                :next-headers (ref [])
                :pages (ref pages) 
                :position (ref 0))))

(defn next-page
  [ogg-stream]
  "Gets the next page from the ogg-stream, or nil if at end of the stream."
  ; Transactional memory is probably overkill given intended usage.  Not sure
  ; what the culture would mandate here.  Technically, we do require 
  ; consistency, so it's not a bad thing to guarantee it, although a particular 
  ; ogg-stream should never be consumed by more than one consumer.
  (dosync
    (let [pages (:pages ogg-stream)
          position (:position ogg-stream)
          headers (:headers ogg-stream)
          next-headers (:next-headers ogg-stream)]
      (if (empty? @pages)
        nil
        (let [page (first @pages)]
          (alter pages rest)
          (if (header? page)
            (do 
              (alter next-headers conj page)
              (ref-set position 0)
              (assoc page :frames 0))
            (let [frames (- (:position page) @position)]
              (when (not (empty? @next-headers))
                (ref-set headers @next-headers)
                (ref-set next-headers []))
              (ref-set position (:position page))
              (assoc page :frames frames))))))))
