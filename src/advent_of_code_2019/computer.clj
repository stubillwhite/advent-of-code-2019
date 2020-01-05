(ns advent-of-code-2019.computer
  (:require [advent-of-code-2019.computer-io :as cmp-io]
            [advent-of-code-2019.utils :refer [parse-long]]
            [clojure.core.async :refer [chan]]
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
(defmethod read-value 0 [mode {:keys [id prg]} x]
  (let [value (nth prg x 0)]
    (trace (format "[%s] Read (positional) value %d from %d" id value x))
    value))

(defmethod write-value 0 [mode {:keys [id prg]} x v]
  (trace (format "[%s] Write (positional) value %d to %d" id v x))
  (assoc-or-grow prg x v))

;; Immediate mode
(defmethod read-value 1 [mode {:keys [id]} x]
  (trace (format "[%s] Read (immediate) value %d" id x))
  x)

(defmethod write-value 1 [mode {:keys [id prg]} x v]
  (trace (format "[%s] Write (immediate) value %d to %d" id v x))
  (assoc-or-grow prg x v))

;; Relative mode
(defmethod read-value  2 [mode {:keys [id rel-base prg]} x]
  (let [value (nth prg (+ rel-base x) 0)]
    (trace (format "[%s] Read (relative) value %d from %d (%d + %d)" id value (+ rel-base x) rel-base x))
    value))

(defmethod write-value 2 [mode {:keys [id rel-base prg]} x v]
  (trace (format "[%s] Write (relative) value %d to %d (%d + %d)" id v (+ rel-base x) rel-base x))
  (assoc-or-grow prg (+ rel-base x) v))

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

(defmethod execute-instruction 3 [{:keys [id ip prg io] :as computer} [a] [a-mode]]
  (let [[value new-io] (cmp-io/read-from-stdin io)]
    (debug (format "[%s] Read from stdin value %d" id value))
    (assoc computer
           :ip    (+ ip 2)
           :io    new-io
           :prg   (write-value a-mode computer a value))))

(defmethod execute-instruction 4 [{:keys [id ip prg io] :as computer} [a] [a-mode]]
  (let [value (read-value a-mode computer a)]
    (debug (format "[%s] Write to stdout value %d" id value))
    (assoc computer
           :io (cmp-io/write-to-stdout io value)
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

(defn basic-io []
  (cmp-io/->BasicBufferedIO [] []))

(defn async-io [& {:keys [stdin stdout] :or {stdin (chan) stdout (chan)}}]
  (cmp-io/->AsyncBufferedIO stdin stdout))

(defn parse-program
  "Returns the program parsed from the input string."
  [input]
  (->> (string/split input #",")
       (map parse-long)
       (into [])))

(defn initialise-computer
  "Returns an initialised computer with the specified program, and optionally the specified
  stdin and stdout channels. Data can subseqently be loaded onto stdin by using `pipe-to-stdin!`,
  and read from stdin and stdout using `read-stdin` and `read-stdout` respectively."
  ([prg & {:keys [id io] :or {io (basic-io)
                              id (.toString (UUID/randomUUID))}}]
   {:id       id
    :ip       0
    :rel-base 0
    :prg      (into [] prg)
    :io       io
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

(defn buffer-to-stdin [computer coll]
  (update computer :io cmp-io/buffer-to-stdin coll))

(defn flush-and-read-stdout [{:keys [io]}]
  (cmp-io/flush-and-read-stdout io))

(defn flush-and-read-stdin [{:keys [io]}]
  (cmp-io/flush-and-read-stdin io))

