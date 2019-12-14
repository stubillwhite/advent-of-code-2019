(ns advent-of-code-2019.computer
  (:require [clojure.core.async :as async :refer [<! <!! chan close! go put!]]
            [clojure.string :as string]
            [taoensso.timbre :as timbre])
  (:import java.util.UUID))

(timbre/refer-timbre)

(defn- current-instruction [{:keys [ip prg]}]
  (let [instr     (format "%05d" (nth prg ip))
        nth-digit (fn [s n] (->> (nth s n) (str) (Long/parseLong)))
        opcode (->> instr (reverse) (take 2) (reverse) (string/join) (Long/parseLong))
        a-mode (nth-digit instr 2)
        b-mode (nth-digit instr 1)
        c-mode (nth-digit instr 0)]
    {:opcode opcode
     :a-mode a-mode
     :b-mode b-mode
     :c-mode c-mode}))

;; Asynchronous primitives

(defn- blocking-read! [ch]
  (<!! (go (<! ch))))

(defn- non-blocking-write! [ch v]
  (if-not (put! ch v)
    (throw (RuntimeException. (format "Failed to write value '%s' because channel is closed" v)))))

(defn- close-and-consume! [ch]
  (close! ch)
  (<!! (async/reduce conj [] ch)))

(defn- put-all! [ch coll]
  (dorun (map (partial non-blocking-write! ch) coll)))

;; Addressing modes

(defn- assoc-or-grow [m k v]
  (let [curr-size (count m)]
    (if (<= curr-size k)
      (assoc (into [] (concat m (repeat (- k curr-size) 0))) k v)
      (assoc m k v))))

(ns-unmap *ns* 'read-value)
(ns-unmap *ns* 'write-value)
(defmulti read-value  (fn [mode prg x] mode))
(defmulti write-value (fn [mode prg x v] mode))

;; Position mode
(defmethod read-value  0 [mode {:keys [prg]} x]   (nth prg x 0))
(defmethod write-value 0 [mode {:keys [prg]} x v] (assoc-or-grow prg x v))

;; Immediate mode
(defmethod read-value  1 [mode {:keys [prg]} x]   x)
(defmethod write-value 1 [mode {:keys [prg]} x v] (assoc-or-grow prg x v))

;; Relative mode
(defmethod read-value  2 [mode {:keys [rel-base prg]} x]   (nth prg (+ rel-base x) 0))
(defmethod write-value 2 [mode {:keys [rel-base prg]} x v] (assoc-or-grow prg (+ rel-base x) v))

;; Instruction set

(ns-unmap *ns* 'execute-instruction)
(defmulti execute-instruction (fn [computer args mode] (:opcode (current-instruction computer))))

(defmethod execute-instruction 1 [{:keys [ip prg] :as computer} [a b c] [a-mode b-mode c-mode]]
  (assoc computer
         :ip  (+ ip 4)
         :prg (write-value c-mode computer c (+ (read-value a-mode computer a)
                                                (read-value b-mode computer b)))))

(defmethod execute-instruction 2 [{:keys [ip prg] :as computer} [a b c] [a-mode b-mode c-mode]]
  (assoc computer
         :ip  (+ ip 4)
         :prg (write-value c-mode computer c (* (read-value a-mode computer a)
                                                (read-value b-mode computer b)))))

(defmethod execute-instruction 3 [{:keys [id ip prg stdin] :as computer} [a] [a-mode]]
  (let [value (blocking-read! stdin)]
    (debug (format "[%s] Read value %d" id value))
    (assoc computer
           :ip    (+ ip 2)
           :prg   (write-value a-mode computer a value))))

(defmethod execute-instruction 4 [{:keys [id ip prg stdout] :as computer} [a] [a-mode]]
  (let [value (read-value a-mode computer a)]
    (non-blocking-write! stdout value)
    (debug (format "[%s] Wrote value %d" id value))
    (assoc computer
           :ip (+ ip 2))))

(defmethod execute-instruction 5 [{:keys [ip prg] :as computer} [a b] [a-mode b-mode]]
  (assoc computer
         :ip (if (not (zero? (read-value a-mode computer a)))
               (read-value b-mode computer b)
               (+ ip 3))))

(defmethod execute-instruction 6 [{:keys [ip prg] :as computer} [a b] [a-mode b-mode]]
  (assoc computer
         :ip (if (zero? (read-value a-mode computer a))
               (read-value b-mode computer b)
               (+ ip 3))))

(defmethod execute-instruction 7 [{:keys [ip prg] :as computer} [a b c] [a-mode b-mode c-mode]]
  (assoc computer
         :ip  (+ ip 4)
         :prg (write-value c-mode computer c (if (< (read-value a-mode computer a) (read-value b-mode computer b)) 1 0))))

(defmethod execute-instruction 8 [{:keys [ip prg] :as computer} [a b c] [a-mode b-mode c-mode]]
  (assoc computer
         :ip  (+ ip 4)
         :prg (write-value c-mode computer c (if (= (read-value a-mode computer a) (read-value b-mode computer b)) 1 0))))

(defmethod execute-instruction 9 [{:keys [ip rel-base] :as computer} [a] [a-mode]]
  (assoc computer
         :ip       (+ ip 2)
         :rel-base (+ rel-base (read-value a-mode computer a))))

(defmethod execute-instruction 99 [{:keys [id] :as computer} [_] [_]]
  (assoc computer
         :halted? true))

(defn- step [{:keys [id ip prg] :as computer}]
  (let [{:keys [a-mode b-mode c-mode] :as instr} (current-instruction computer)
        [_ a b c] (drop ip prg)]
    (debug (format "[%s] Executing instruction %s %s" id instr [a b c]))
    (execute-instruction computer [a b c] [a-mode b-mode c-mode])))

(defn- take-while-inclusive [f coll]
  (let [[x & xs] coll]
    (if-not (f x)
      (take 1 coll)
      (cons x (lazy-seq (take-while-inclusive f xs))))))

;; Public

(defn initialise-computer
  "Returns an initialised computer with the specified program, and optionally the specified
  stdin and stdout channels. Data can subseqently be loaded onto stdin by using `pipe-to-stdin!`,
  and read from stdin and stdout using `read-stdin` and `read-stdout` respectively."
  ([prg & {:keys [id stdin stdout] :or {stdin  (chan)
                                        stdout (chan)
                                        id     (.toString (UUID/randomUUID))}}]
   {:id       id
    :ip       0
    :rel-base 0
    :prg      (into [] prg)
    :stdin    stdin
    :stdout   stdout
    :halted?  false}))

(defn step-program
  "Returns a lazy seq of all states of the computer until it halts."
  [computer]
  (take-while-inclusive (fn [{:keys [ip halted?]}] (not halted?))
                        (iterate step computer)))

(defn execute-program
  "Returns the program in the final executed state."
  [computer]
  (-> (step-program computer)
      (last)))

(defn pipe-to-stdin!
  "Returns computer with the side-effect of adding the contents of coll to stdin."
  [computer coll]
  (do
    (debug (format "Loading %s with %s" (:id computer) coll))
    (put-all! (:stdin computer) coll)
    computer))

(defn read-stdout
  "Returns the stdout from a halted computer. Will block if the computer is not halted." ;; TODO: Fix text
  [computer]
  (close-and-consume! (:stdout computer)))

(defn read-stdin
  "Returns the stdin from a halted computer. Will block if the computer is not halted."
  [computer]
  (close-and-consume! (:stdin computer)))

