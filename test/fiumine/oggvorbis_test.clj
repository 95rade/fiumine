(ns fiumine.oggvorbis-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [fiumine.oggvorbis :refer :all])
  (:import (java.io ByteArrayInputStream)))

(defn char-seq
  "Read a stream character by character."
  [stream]
  (let [ch (.read stream)]
    (when (not= ch -1)
      (cons (char ch) (lazy-seq (char-seq stream))))))

(defn oggvorbis-stream
  "Stream as sequence. Dangerous wrt memory. For testing only."
  [stream]
  (when-let [page (read-page stream)]
    (cons page (lazy-seq (oggvorbis-stream stream)))))

(deftest test-scan-for-page
  (testing "Scan for beginning of ogg page."
    (let [data "foobarOggSbazboo"
          stream (ByteArrayInputStream. (byte-array (map byte data)))]
      (scan-for-page stream)
      (is (= "OggSbaz" (apply str (take 7 (char-seq stream))))))))

(deftest test-ogg-page
  (testing "Test reading a page leaves stream on a page boundary."
    (let [url (io/resource "fiumine/test.ogg")
          stream (io/input-stream url)
          page (read-page stream)
          header (fn [bs] (apply str (map char (take 4 bs))))]
      (is (= "OggS" (header (:page-data page))))
      (is (= "OggS" (header (char-seq stream))))))

  (testing "Test that we get right number of pages."
    (let [url (io/resource "fiumine/test.ogg")
          stream (io/input-stream url)
          ogg (oggvorbis-stream stream)
          pages (vec ogg)]
      (is (= 142 (.size pages)))))

  (testing "Test marshaling the page structure."
    (let [url (io/resource "fiumine/test.ogg")
          stream (io/input-stream url)
          pages (take 5 (oggvorbis-stream stream))
          page (first pages)]
      (is (= "OggS" (:capture-pattern page)))
      (is (= 0 (:ogg-revision page)))
      (is (= 2 (:flags page)))
      (is (= 0 (:position page)))
      (is (= 1389714903 (:serial-number page)))
      (is (= 0 (:sequence-number page)))
      (is (= 697906608 (:crc-checksum page)))
      (is (= 1 (:n-page-segments page)))
      (is (= (range 5) (map :sequence-number pages)))
      (is (= [2 0 0 0 0] (map :flags pages)))
      (is (= [0 0 15424 30784 45440] (map :position pages)))
      (is (= [1 28 33 30 33] (map :n-page-segments pages)))))

  (testing "Test get packets."
    (let [url (io/resource "fiumine/test.ogg")
          stream (io/input-stream url)
          ogg (oggvorbis-stream stream)
          packets (flatten (map vorbis-packets ogg))]
      (is (every? (complement audio?) (take 3 packets)))
      (is (every? audio? (nthnext packets 3)))))

  (testing "Marshal vorbis id header structure"
    (let [url (io/resource "fiumine/test.ogg")
          stream (io/input-stream url)
          page (read-page stream)
          packet (first (vorbis-packets page))
          info (vorbis-id packet)]
      (is (= 2 (:channels info)))
      (is (= 44100 (:framerate info))))))

