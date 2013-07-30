(ns fiumine.oggvorbis-test
  (:require [clojure.test :refer :all]
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
    (let [data "foobarOggsbazboo"
          stream (ByteArrayInputStream. (byte-array (map byte data)))]
      (scan-for-page! stream)
      (is (= "Oggsbaz" (apply str (take 7 (char-seq stream))))))))
