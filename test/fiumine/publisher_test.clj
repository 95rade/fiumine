(ns fiumine.publisher-test
  (:require [clojure.java.io :as io]
            [clojure.test :refer :all]
            [fiumine.publisher :as pub]))

(defn sha1 [data]
  (.digest (java.security.MessageDigest/getInstance "sha1") data ))

(defn noop [& args])

(defn bslurp 
  "Binary slurp."
  [file]
  (let [buffer (byte-array (.length file))]
    (.read (io/input-stream file) buffer)
    buffer))
        
(deftest test-publish-stream
  (with-redefs-fn
    {#'pub/sleep noop}
    (fn []
      (testing "Publish stream"
        ; Would be nice if clojure.core had a binary slurp
        (let [raw-bytes (bslurp (io/file (io/resource "fiumine/test.ogg")))
              ; Add some crap to the beginning to test seeking to capture pattern
              stream (io/input-stream raw-bytes)
              audio (atom [])]
          (add-watch pub/audio-page :test #(swap! audio conj %4))
          (-> stream pub/publish-stream deref) ; Will wait for publisher to finish
          (remove-watch pub/audio-page :test)
          (let [pages (concat @pub/header-pages @audio)
                seq-nos (map :sequence-number (take 10 pages))
                composed (->> pages (map :page-data) (apply concat) byte-array)]
            (is (= seq-nos (range 10)))
            (is (= (alength raw-bytes) (alength composed)))
            (is (= (vec (sha1 raw-bytes)) (vec (sha1 composed))))))))))

(deftest test-subscribe-stream
  (with-redefs-fn
    {#'pub/sleep noop}
    (fn []
      (testing "Subscribe to stream"
        ; Would be nice if clojure.core had a binary slurp
        (let [raw-bytes (bslurp (io/file (io/resource "fiumine/test.ogg")))
              stream (io/input-stream raw-bytes)
              promise-to-stream (pub/subscribe)
              publisher (pub/publish-stream stream)
              pages @promise-to-stream
              seq-nos (map :sequence-number (take 10 pages))
              composed (->> pages (map :page-data) (apply concat) byte-array)]
          (is (nil? @(pub/subscribe))) ; Test post-eos condition
          (is (= seq-nos (range 10)))
          (is (= (alength raw-bytes) (alength composed)))
          (is (= (vec (sha1 raw-bytes)) (vec (sha1 composed)))))))))
