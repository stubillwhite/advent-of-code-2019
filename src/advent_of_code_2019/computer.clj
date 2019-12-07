(ns advent-of-code-2019.computer
  (:require [advent-of-code-2019.utils :refer [def- ]]
            [clojure.string :as string]))

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

(ns-unmap *ns* 'read-value)
(ns-unmap *ns* 'write-value)
(defmulti read-value  (fn [mode prg x] mode))
(defmulti write-value (fn [mode prg x v] mode))

;; Position mode
(defmethod read-value  0 [mode prg x]   (nth prg x))
(defmethod write-value 0 [mode prg x v] (assoc prg x v))

;; Immediate mode
(defmethod read-value  1 [mode prg x]   x)
(defmethod write-value 1 [mode prg x v] (assoc prg x v))

;; Instruction set

(ns-unmap *ns* 'execute-instruction)
(defmulti execute-instruction (fn [computer args mode] (:opcode (current-instruction computer))))

(defmethod execute-instruction 1 [{:keys [ip prg] :as computer} [a b c] [a-mode b-mode c-mode]]
  (assoc computer
         :ip  (+ ip 4)
         :prg (write-value 0 prg c (+ (read-value a-mode prg a)
                                      (read-value b-mode prg b)))))

(defmethod execute-instruction 2 [{:keys [ip prg] :as computer} [a b c] [a-mode b-mode c-mode]]
  (assoc computer
         :ip  (+ ip 4)
         :prg (write-value 0 prg c (* (read-value a-mode prg a)
                                      (read-value b-mode prg b)))))

(defmethod execute-instruction 3 [{:keys [ip prg stdin] :as computer} [a] [a-mode]]
  (assoc computer
         :ip    (+ ip 2)
         :prg   (write-value 0 prg a (first stdin))
         :stdin (rest stdin)))

(defmethod execute-instruction 4 [{:keys [ip prg stdout] :as computer} [a] [a-mode]]
  (assoc computer
         :ip     (+ ip 2)
         :stdout (conj stdout (read-value a-mode prg a))))

(defmethod execute-instruction 5 [{:keys [ip prg] :as computer} [a b] [a-mode b-mode]]
  (assoc computer
         :ip (if (not (zero? (read-value a-mode prg a)))
               (read-value b-mode prg b)
               (+ ip 3))))

(defmethod execute-instruction 6 [{:keys [ip prg] :as computer} [a b] [a-mode b-mode]]
  (assoc computer
         :ip (if (zero? (read-value a-mode prg a))
               (read-value b-mode prg b)
               (+ ip 3))))

(defmethod execute-instruction 7 [{:keys [ip prg] :as computer} [a b c] [a-mode b-mode c-mode]]
  (assoc computer
         :prg (write-value 0 prg c (if (< (read-value a-mode prg a) (read-value b-mode prg b)) 1 0))
         :ip  (+ ip 4)))

(defmethod execute-instruction 8 [{:keys [ip prg] :as computer} [a b c] [a-mode b-mode c-mode]]
  (assoc computer
         :prg (write-value 0 prg c (if (= (read-value a-mode prg a) (read-value b-mode prg b)) 1 0))
         :ip  (+ ip 4)))

(defmethod execute-instruction 99 [{:keys [ip] :as computer} [_] [_]]
  (assoc computer :halted? true))

(defn- step [{:keys [ip prg] :as computer}]
  (let [{:keys [a-mode b-mode c-mode] :as instr} (current-instruction computer)
        [_ a b c] (drop ip prg)]
    (execute-instruction computer [a b c] [a-mode b-mode c-mode])))

;; Public

(defn initialise-computer
  "Returns an initialised computer with the program and stdin bound configured."
  [prg stdin]
  {:ip      0
   :prg     (into [] prg)
   :stdin   stdin
   :stdout  []
   :halted? false})

(defn step-program
  "Returns a lazy seq of all states of the computer until it halts."
  [computer]
  (take-while (fn [{:keys [ip halted?]}] (not halted?))
              (iterate step computer)))

(defn execute-program
  "Returns the program in the final executed state."
  [computer]
  (-> (step-program computer)
      (last)))
