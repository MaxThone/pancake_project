; Script to do pancake flipping with clojure
; Module
(require '[clojure.string :as str])

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

(defn new_flipping_pancakes
  [P F flips]
  (let [idx (.indexOf P \-)
        flip (fn [x] (if (= \- x) \+ \-))
        flip_array (fn [P F idx] (vec (map flip (subvec P idx (+ idx F)))))]
    (if (= idx -1)
      (str flips)
      (if (> (+ idx F) (count P))
        (str "IMPOSSIBLE")
        (new_flipping_pancakes (replace_array P (flip_array P F idx) idx) F (inc flips))))))

; FUNCTIONS HELPING WITH READING/WRITING ETC.

; Read files:
(defn read_per_line [file_path]
  (with-open [rdr (clojure.java.io/reader file_path)]
    (reduce conj [] (line-seq rdr))))

; Write files
(defn write_lines [filename data]
  (with-open [wrt (clojure.java.io/writer filename)]
    (doseq [x data]
      (.write wrt (str x "\n")))))

(defn solver
  [vector_unit]
  (let [P (vec (first vector_unit))
        F (Integer. (second vector_unit))
        ] (new_flipping_pancakes P F 0)))

; TEST CASES
(defn sample
  []
  (println (new_flipping_pancakes "-+--" 1 0))
  (println (new_flipping_pancakes "-+--" 2 0))
  (println (new_flipping_pancakes "-+--" 1 0))
  (println (new_flipping_pancakes "-+--" 1 0))
  (println (new_flipping_pancakes "-+--" 1 0))
  (println (new_flipping_pancakes "-+--" 3 0))
  (println (new_flipping_pancakes "-+--" 4 0))
  (println (new_flipping_pancakes "-+--" 1 0))
  (println (new_flipping_pancakes "-+--+--++-----+" 3 0))
  (println (new_flipping_pancakes "-+--+++---+++" 3 0))
  (println (new_flipping_pancakes "-+--" 2 0))
  (println (new_flipping_pancakes "-+--+--+" 1 0))
  (println (new_flipping_pancakes "-+--+--+" 3 0))
  (println (new_flipping_pancakes "-+--++-+" 3 0))
  )
; READ FROM FILE, WRITE SOLUTION TO FILE
(def input_vector
  (let [split_by_space (fn [x] (str/split x #" "))]
  (map split_by_space (subvec (read_per_line "input_files/qual_2017/A-small-practice.in") 1))))

(def output_vector (map solver input_vector))

(def output_data (map str
                      (repeat (count input_vector) "case #")
                      (range 1 ( + (count input_vector) 1))
                      (repeat (count input_vector) ": ")
                      output_vector))

(write_lines "input_files/qual_2017/large_output.txt" output_data)