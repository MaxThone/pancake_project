
(ns gcj-2017-1-a.core)

; Libraries
(require '[clojure.string :as str])
; CORE LOGIC
(defn core-walk
  [vec idx state]
  (if (and (vector? vec) (< idx (count vec)))
    (let [is_q (partial = \?)
          element (get vec idx)
          vector_flag (vector? element)
          has_no_info (if vector_flag (every? is_q element) (is_q element))]
      (if has_no_info
        (if (= state nil)
          (let [rest-of-vec (core-walk vec (inc idx) state)]
            (assoc rest-of-vec idx (get rest-of-vec (inc idx))))
          (core-walk (assoc vec idx state) (inc idx) state))
        (let [replace_element (core-walk element 0 nil)]
          (core-walk(assoc vec idx replace_element) (inc idx) replace_element))))
    vec))

; LOGIC WHEN DEBUGGING WITH INDENTED PRINTS
(def indent 0)
(defn repeatstr
  [s n]
  (apply str (repeat n s)))

(defn iprintln
  [& rest]
  (apply println (repeatstr "  " indent) rest))

; Define Walk-inner
(declare Walk-inner)

(defn Walk
  [& args]
  (do
    (let [old-indent indent]
      (def indent (inc indent))
      (let [result (apply Walk-inner args)]
        (def indent old-indent)
        result))))

(defn Walk-inner
  [vec idx state]
  (iprintln "----")
  (iprintln vec)
  (iprintln idx)
  (iprintln state)
  (if (and (vector? vec) (< idx (count vec)))
    (let [is_q (partial = \?)
          element (get vec idx)
          vector_flag (vector? element)
          has_no_info (if vector_flag (every? is_q element) (is_q element))
          ]
      (iprintln "aaa")
      (if has_no_info
        (if (= state nil)
          (let [rest-of-vec (Walk vec (inc idx) state)]
            (iprintln "bbb")
            (assoc rest-of-vec idx (get rest-of-vec (inc idx))))
          (do (iprintln "ccc")
              (Walk (assoc vec idx state) (inc idx) state)))
        (let [replace_element (Walk element 0 nil)]
          (iprintln "ddd")
          (Walk (assoc vec idx replace_element) (inc idx) replace_element))))
    (do (iprintln "eee") vec)))

; READ/WRITE FUNCTIONS
(defn build-pie-from-input
  [pie n-rows idx]
  (if (= idx n-rows)
    pie
    (let
      [pie_vec (vec (read-line))]
      (build-pie-from-input (conj pie pie_vec) n-rows (inc idx)))))

(defn print-pie-as-lines
  [pie]
  (let [pie-str (clojure.string/join (map str (first pie)))]
  (if (= (count pie) 1)
    (println pie-str)
    (do
      (println pie-str)
      (print-pie-as-lines (subvec pie 1))
      ))))

(defn do-one
  [i]
  (let [n-rows (read-string (read-line))
        input-pie (build-pie-from-input [] n-rows 0)
        output-pie (core-walk input-pie 0 nil)]
    (do
      (println (format "Case #%d:" i))
      (print-pie-as-lines output-pie)
    )))

(defn do-stdin
  []
  (let [T (read-string (read-line))]
    (dotimes [i T]
      (do-one (inc i)))))

(defn do-file
  [rname wname]
  (with-open [rdr (clojure.java.io/reader rname)
              wtr (clojure.java.io/writer wname)]
    (binding [*in* rdr
              *out* wtr]
      (do-stdin))))

(defn -main
  [& [files]]
  (if files
    (let [fname (first files)
          wname (second files)]
    (do-file fname wname))
    (do-stdin)))

(do-file "data/A-large-practice.in" "data/large-output.in")