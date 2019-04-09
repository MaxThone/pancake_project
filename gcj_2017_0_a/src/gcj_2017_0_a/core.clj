(ns gcj-2017-0-a.core)

(require '[clojure.string :as str])
; GENERAL COMMENTS ON CODE
; - you should try to just work with lists, not necessarily with vectors.
; - reading/writing files does not necessarily have to be done with java read/write, 
;   can just be done with clojure read-line

; CORE FUNCTIONS
(defn replace_array
  [p_array flip_array idx]
  (if (= (count flip_array) 0)
    ;; Flip array is empty, so no need to do anything:
    p_array
    ;; Flip array contains elements we want to sub:
    (replace_array (assoc p_array idx (get flip_array 0))
                   (subvec flip_array 1)
                   (+ idx 1))))

(defn flip
  [x]
  (if (= \- x) \+ \-))

(defn flip-array
  [P F idx]
  (vec (map flip (subvec P idx (+ idx F)))))

(defn flipping-pancakes
  [P F flips]
  (let [idx (.indexOf P \-)]
    (if (= idx -1)
      (str flips)
      (if (> (+ idx F) (count P))
        (str "IMPOSSIBLE")
        (flipping-pancakes (replace_array P (flip-array P F idx) idx) F (inc flips))))))

; FUNCTIONS HELPING WITH READING/WRITING ETC.

; Read files:
(defn read-per-line [file_path]
  (with-open [rdr (clojure.java.io/reader file_path)]
    (reduce conj [] (line-seq rdr))))

; Write files
(defn write-lines [filename data]
  (with-open [wrt (clojure.java.io/writer filename)]
    (doseq [x data]
      (.write wrt (str x "\n")))))

; wrapper for flipping-pancakes to work with input files
(defn solver
  [vector_unit]
  (let [P (vec (first vector_unit))
        F (Integer. (second vector_unit))
        ] (flipping-pancakes P F 0)))

; TEST CASES
(defn sample-solver
  [P-str F]
  (let [P (vec P-str)]
    (flipping-pancakes P F 0)))

(defn sample
  []
  (println (sample-solver "-+--" 1))
  (println (sample-solver "-+--" 2))
  (println (sample-solver "-+--" 1))
  (println (sample-solver "-+--" 1))
  (println (sample-solver "-+--" 1))
  (println (sample-solver "-+--" 3))
  (println (sample-solver "-+--" 4))
  (println (sample-solver "-+--" 1))
  (println (sample-solver "-+--+--++-----+" 3))
  (println (sample-solver "-+--+++---+++" 3))
  (println (sample-solver "-+--" 2))
  (println (sample-solver "-+--+--+" 1))
  (println (sample-solver "-+--+--+" 3))
  (println (sample-solver "-+--++-+" 3))
  )

(sample)

; READ FROM FILE, WRITE SOLUTION TO FILE
(def input-vector
  (let [split_by_space (fn [x] (str/split x #" "))]
    (map split_by_space (subvec (read-per-line "data/input/A-large-practice.in") 1))))

(def output-vector (map solver input-vector))

(def output-data (map str
                      (repeat (count input-vector) "case #")
                      (range 1 ( + (count input-vector) 1))
                      (repeat (count input-vector) ": ")
                      output-vector))
(write-lines "data/output/large_output.txt" output-data)
