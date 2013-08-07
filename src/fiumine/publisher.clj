(ns fiumine.publisher
  "Publishes ogg/vorbis stream to any listening subscribers."
  (:require [fiumine.oggvorbis :as ogg]))

(def serial (atom 0))
(def ^:const queue-size 10)
(def now #(System/currentTimeMillis))

(defn sleep [millis]
  "Stub for unit tests."
  (Thread/sleep millis))

(defn- skip-headers
  "Skips headers at the head of an ogg-stream. Returns first audio page or nil
   if end of stream."
  [ogg-stream]
  (when-let [page (ogg/next-page ogg-stream)]
    (if (ogg/header? page)
      (recur ogg-stream)
      page)))

(defn publish-stream
  "Factory function which returns a publisher structure which can be used to 
   publish an ogg stream to many providers."
  [stream]
  (let [ogg-stream (ogg/read-stream stream)
        headers (:headers ogg-stream)
        audio-page (atom nil)
        streaming (atom true)]
    {:ogg-stream ogg-stream
     :headers headers
     :audio-page audio-page
     :streaming streaming}))

(defn start
  "Begin publishing the stream."
  [publisher]
  (let [ogg-stream (:ogg-stream publisher)
        audio-page (:audio-page publisher)
        streaming (:streaming publisher)
        framerate (:framerate ogg-stream)
        first-page (skip-headers ogg-stream)
        start-time (now)]
    (future ; Pump out the audio in another thread
      (try
        (loop [page first-page]
          (when page
            (let [ogg-time (double (* 1000 (/ (:position page) framerate)))
                  real-time (- (now) start-time)
                  sleep-for (- ogg-time real-time)]
              (reset! audio-page page)
              (when (pos? sleep-for)
                (sleep sleep-for))
              (recur (ogg/next-page ogg-stream)))))
        (reset! audio-page :eos)
        (reset! streaming false)
        (catch Throwable e 
          (prn e)
          (throw e))))))

(defn subscribe
  "Creates a blocking queue for publishing pages from stream and subscribes
   a watcher on the 'audio-page' atom for putting pages into the queue.
   Returns a promise which can be dereferenced into a lazy sequence that 
   realizes the queue page by page if and when streaming begins, or nil if
   streaming has already finished."
  [publisher]
  (let [id (keyword (str "subscriber" (swap! serial inc)))
        queue (java.util.concurrent.ArrayBlockingQueue. queue-size)
        connected (atom true)
        headers (:headers publisher)
        streaming (:streaming publisher)
        audio-page (:audio-page publisher)]
    ; Watch to execute every time audio-page is swapped
    (when (not (false? @streaming))
      (let [sequence-number (atom nil)
            position (atom 0)]
        (add-watch audio-page id 
          (fn [id _ _ page]
            (when (nil? @sequence-number)
              (reset! sequence-number (.size @headers)))
            (if (zero? (.remainingCapacity queue))
              (do
                ; Disconnect watcher if subscriber is no longer pulling pages
                ; Maybe they disconnected.
                (remove-watch audio-page id)
                (reset! connected false))
              (do 
                ; Deliver page to queue after adjusting sequence and position
                (if (= :eos page)
                  (.add queue :eos)
                  (let [next-position (+ @position (:frames page))
                        page (ogg/modify-page page 
                               {:sequence-number @sequence-number
                                :position next-position})]
                    (swap! sequence-number inc)
                    (reset! position next-position)
                    (.add queue page)))))))))

    ; Lazy sequence to realize stream from queue
    (let [stream-pages 
          (fn stream-pages []
            (when @connected
              (lazy-seq 
                (let [page (.take queue)]
                  (when (not= :eos page)
                    (cons page (stream-pages)))))))
          make-stream #(concat @headers (stream-pages))
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
