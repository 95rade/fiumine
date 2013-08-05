(ns fiumine.publisher
  "Publishes ogg/vorbis stream to any listening subscribers."
  (:require [fiumine.oggvorbis :as ogg]))

; Current header pages will always be stored in this atom
(def header-pages (atom nil))

; Current audio page will be continuously refreshed at this atom
(def audio-page (atom nil))

; Used to signal that stream being published has reached eos
(def streaming (atom nil))

(defn sleep [millis]
  "Stub for unit tests."
  (Thread/sleep millis))

(defn- grab-headers
  "Grabs headers and first audio page from a stream. Returns 
  [header-pages first-page]."
  [stream]
  (loop [headers []]
    (let [page (ogg/read-page stream)
          packet (-> page ogg/vorbis-packets first)]
      (if (ogg/audio? packet)
        [headers page]
        (recur (conj headers page))))))

(defn publish-stream
  "Publishes stream to global vars, assumes subscribers have registered watches 
   on 'audio-page'. Spins up a new thread."
  [stream]
  ; Start by grabbing header pages
  (let [[headers first-page] (grab-headers stream)
        info (-> headers first ogg/vorbis-packets first ogg/vorbis-id)
        framerate (:framerate info)]
    (reset! header-pages headers)
    (reset! streaming true)
    (future ; Pump out the audio in another thread
      (loop [page first-page last-position (:position page)]
        (when page
          (let [position (:position page)
                samples (- position last-position)
                time-millis (* 1000 (/ samples framerate))]
            (reset! audio-page page)
            (sleep time-millis)
            (recur (ogg/read-page stream) position))))
      (reset! audio-page :eos)
      (reset! streaming false))))

(def serial (atom 0))

(def ^:const queue-size 200)

(defn subscribe
  "Creates a blocking queue for publishing pages from stream and subscribes
   a watcher on the 'audio-page' atom for putting pages into the queue.
   Returns a promise which can be dereferenced into a lazy sequence that 
   realizes the queue page by page if and when streaming begins, or nil if
   streaming has already finished."
  []
  (let [id (keyword (str "subscriber" (swap! serial inc)))
        queue (java.util.concurrent.ArrayBlockingQueue. queue-size)
        connected (atom true)]
    ; Watch to execute every time audio-page is swapped
    (when (not (false? @streaming))
      (add-watch audio-page id 
        (fn [id _ _ page]
          (if (zero? (.remainingCapacity queue))
            ; Disconnect watcher if subscriber is no longer pulling pages
            ; Maybe they disconnected.
            (do
              (remove-watch audio-page id)
              (reset! connected false))
            ; Deliver page to queue
            (.add queue page)))))

    ; Lazy sequence to realize stream from queue
    (let [stream-pages 
          (fn stream-pages []
            (when @connected
              (lazy-seq 
                (let [page (.take queue)]
                  (when (not= :eos page)
                    (cons page (stream-pages)))))))
          make-stream #(concat @header-pages (stream-pages))
          promise-to-stream (promise)]
      (cond 
        ; If we're already streaming we can deliver promise immediately
        @streaming
        (deliver promise-to-stream (make-stream))
        ; If we haven't started yet, we'll have to defer the promise until
        ; streaming has begun
        (nil? @streaming)
        (add-watch streaming id
          (fn [id _ _ value]
            (remove-watch streaming id)
            (when value (deliver promise-to-stream (make-stream)))))
        ; Otherwise we've streamed and stopped, deliver nil
        :else (deliver promise-to-stream nil))
      promise-to-stream)))
