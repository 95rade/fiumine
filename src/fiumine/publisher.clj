(ns fiumine.publisher
  "Publishes ogg/vorbis stream to any listening subscribers."
  )

; Current header blocks will always be stored in this atom
(def header-blocks (atom))

; Current audio block will be continuously refreshed at this atom
(def audio-block (atom))


