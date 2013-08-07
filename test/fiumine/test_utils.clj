(ns fiumine.test-utils
  (:require [clojure.java.io :as io]))

(defn sha1 [data]
  (.digest (java.security.MessageDigest/getInstance "sha1") data ))

(defn bslurp 
  "Binary slurp."
  [file]
  (let [buffer (byte-array (.length file))]
    (.read (io/input-stream file) buffer)
    buffer))
