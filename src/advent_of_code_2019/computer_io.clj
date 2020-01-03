(ns advent-of-code-2019.computer-io
  (:require [clojure.core.async :as async :refer [<! <!! chan close! go put!]]))

;; Protocols

(ns-unmap *ns* 'ComputerIO)
(defprotocol ComputerIO
  (read-from-stdin [this]
    "Returns a tuple [value io], where value is read from stdin and io the updated io state.")
  
  (write-to-stdout [this v]
    "Returns this with value v written to stdout."))

;; TODO Rename to reflect that this is interacting with computer IO from clients, not programs
(ns-unmap *ns* 'BufferedIO)
(defprotocol BufferedIO
  (buffer-to-stdin [this coll]
    "Returns this with coll buffered to stdin.")

  (flush-and-read-stdout [this]
    "Returns the flushed content of stdout.")

  (flush-and-read-stdin [this]
    "Returns the flushed content of stdin."))

;; Basic IO

(ns-unmap *ns* 'BasicBufferedIO)
(defrecord BasicBufferedIO [stdin stdout]
  ComputerIO
  (read-from-stdin [{:keys [stdin] :as this}]
    (let [[x & xs] stdin]
      [x (assoc this :stdin xs)]))

  (write-to-stdout [this v]
    (update this :stdout conj v))

  BufferedIO
  (buffer-to-stdin [this coll]
    (update-in this [:stdin] (fn [x] (concat x coll))))
  
  (flush-and-read-stdout [this]
    (get-in this [:stdout]))

  (flush-and-read-stdin [this]
    (get-in this [:stdin])))



;; Asynchronous IO

;; TODO: Privatise

(defn blocking-read! [ch]
  (<!! (go (<! ch))))

(defn non-blocking-write! [ch v]
  (if-not (put! ch v)
    (throw (RuntimeException. (format "Failed to write value '%s' because channel is closed" v)))))

(defn close-and-consume! [ch]
  (close! ch)
  (<!! (async/reduce conj [] ch)))

(defn put-all! [ch coll]
  (dorun (map (partial non-blocking-write! ch) coll)))

(ns-unmap *ns* 'AsyncBufferedIO)
(defrecord AsyncBufferedIO [stdin stdout]
  ComputerIO
  (read-from-stdin [{:keys [stdin] :as this}]
    (let [value (blocking-read! stdin)]
      [value this]))

  (write-to-stdout [{:keys [stdout] :as this} v]
    (non-blocking-write! stdout v)
    this)

  BufferedIO
  (buffer-to-stdin [this coll]
    (put-all! (get-in this [:stdin]) coll)
    this)

  (flush-and-read-stdout [this]
    (close-and-consume! (get-in this [:stdout])))

  (flush-and-read-stdin [this]
    (close-and-consume! (get-in this [:stdin]))))


