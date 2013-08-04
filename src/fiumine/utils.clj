(ns fiumine.utils "Miscellaneous utilities")

(defn file-extension
  "Returns a file's file extension or nil if none is found."
  [file]
  (re-find #"\.[^.]+$" (clojure.string/lower-case (.getName file))))

