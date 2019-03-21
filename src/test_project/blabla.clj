; Script to do pancake flipping with clojure
; Module
(require '[clojure.string :as str])

; FUNCTIONS
; get first index of minus from p_array
(defn get_pancake_batch
  [p_array init f]
  (subvec p_array init (+ init f)))

(defn flip
  [pancake]
  (if (= \- pancake) \+ \-))

(defn flip_pancake_batch
  [p_array]
  (vec (map flip p_array)))

(defn get_minus_index
  [p_array]
  (.indexOf p_array \-))

(defn AllPlus?
  [p_array]
  (let [equals_plus (fn [x] (= x \+))]
  (= (count p_array) (count (filter equals_plus p_array)))))

(defn replace_array
  [p_array flip_array idx]
  (if (= (count flip_array) 0)
    ;; Flip array is empty, so no need to do anything:
    p_array
    ;; Flip array contains elements we want to sub:
    (replace_array (assoc p_array idx (get flip_array 0))
                   (subvec flip_array 1)
                   (+ idx 1))))

(defn one_pancake_flip
  [P F]
  (let [p_array (vec P)
        idx (get_minus_index p_array)
        batch_array (get_pancake_batch p_array idx F)
        flip_array (flip_pancake_batch batch_array)]
    (replace_array p_array flip_array idx)))

(defn flipping_pancakes
  [P F flips]
  (let [p_array (vec P)
        idx (get_minus_index p_array)
        F (Integer. F)]
  (if (AllPlus? p_array)
    (str flips)
    (if (> (+ idx F) (count p_array))
      (str "IMPOSSIBLE")
      (flipping_pancakes (one_pancake_flip P F) F (inc flips))))))
;
(defn solver
  [vector_unit]
  (let [P (first vector_unit)
        F (second vector_unit)
        ](flipping_pancakes P F 0)))

; CHECK FOR TEST CASES
(defn sample
  []
  (println (flipping_pancakes "-+--" 1 0))
  (println (flipping_pancakes "-+--" 2 0))
  (println (flipping_pancakes "-+--" 1 0))
  (println (flipping_pancakes "-+--" 1 0))
  (println (flipping_pancakes "-+--" 1 0))
  (println (flipping_pancakes "-+--" 3 0))
  (println (flipping_pancakes "-+--" 4 0))
  (println (flipping_pancakes "-+--" 1 0))
  (println (flipping_pancakes "-+--+--++-----+" 3 0))
  (println (flipping_pancakes "-+--+++---+++" 3 0))
  (println (flipping_pancakes "-+--" 2 0))
  (println (flipping_pancakes "-+--+--+" 1 0))
  (println (flipping_pancakes "-+--+--+" 3 0))
  (println (flipping_pancakes "-+--++-+" 3 0))
  )

; Read files:
(defn read_per_line [file_path]
  (with-open [rdr (clojure.java.io/reader file_path)]
    (reduce conj [] (line-seq rdr))))

; Write files
(defn write_lines [filename data]
  (with-open [wrt (clojure.java.io/writer filename)]
    (doseq [x data]
      (.write wrt (str x "\n")))))

(defn split_by_space
  [string_unit]
  (str/split string_unit #" "))


(def input_vector (map split_by_space (subvec (read_per_line "input_files/A-large-practice.in") 1)))
(def output_vector (map solver input_vector))

(def output_data (map str
                      (repeat (count input_vector) "case #")
                      (range 1 ( + (count input_vector) 1))
                      (repeat (count input_vector) ": ")
                      output_vector
                      ))

(write_lines "input_files/large_output.txt" output_data)