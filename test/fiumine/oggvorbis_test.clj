(ns fiumine.oggvorbis-test
  (:require [clojure.java.io :refer :all]
            [clojure.test :refer :all]
            [fiumine.oggvorbis :refer :all])
  (:import (java.io ByteArrayInputStream)))

(defn char-seq
  "Read a stream character by character."
  [stream]
  (let [ch (.read stream)]
    (when (not= ch -1)
      (cons (char ch) (lazy-seq (char-seq stream))))))

(deftest test-scan-for-page
  (testing "Scan for beginning of ogg page."
    (let [data "foobarOggSbazboo"
          stream (ByteArrayInputStream. (byte-array (map byte data)))]
      (scan-for-page! stream)
      (is (= "OggSbaz" (apply str (take 7 (char-seq stream))))))))

(deftest test-ogg-page
  (testing "Test reading a page leaves stream on a page boundary."
    (let [url (resource "fiumine/test.ogg")
          stream (input-stream url)
          page (ogg-page! stream)
          header (fn [bs] (apply str (map char (take 4 bs))))]
      (is (= "OggS" (header page)))
      (is (= "OggS" (header (char-seq stream)))))))

(deftest test-oggvorbis-stream
  (testing "Test that we get right number of pages."
    (let [url (resource "fiumine/test.ogg")
          stream (input-stream url)
          ogg (oggvorbis-stream stream)
          pages (vec ogg)]
      (is (= 142 (.size pages))))))


