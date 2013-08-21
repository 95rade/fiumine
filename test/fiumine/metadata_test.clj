(ns fiumine.metadata-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [fiumine.metadata :as md]
            [fiumine.test-utils :refer :all]))

(deftest test-metadata
  (testing "ogg"
    (let [f (io/file (io/resource "fiumine/test.ogg"))]
      (is (= (md/metadata f) {:title "Introduction" :artist "Amor De Cosmos" 
                              :album "Live at Daniela's House 2011"}))))
  (testing "flac"
    (let [f (io/file (io/resource "fiumine/test.flac"))]
      (is (= (md/metadata f) {:title "Introduction" :artist "Amor De Cosmos" 
                              :album "Live at Daniela's House 2011"}))))
  (testing "mp3"
    (let [f (io/file (io/resource "fiumine/test.mp3"))]
      (is (= (md/metadata f) {:title "Introduction" :artist "Amor De Cosmos" 
                              :album "Live at Daniela's House 2011"})))))
