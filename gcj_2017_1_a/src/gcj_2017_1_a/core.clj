(ns gcj-2017-1-a.core)

(defn core-walk
  [vec idx state]
  (if (and (vector? vec) (< idx (count vec)))
    (let [is_q (partial = \?)
          element (get vec idx)
          vector_flag (vector? element)
          has_no_info (if vector_flag (every? is_q element) (is_q element))]
      (if has_no_info
        (if (= state nil)
          (let [rest-of-vec (Walk vec (inc idx) state)]
            (assoc rest-of-vec idx (get rest-of-vec (inc idx))))
          (core-walk (assoc vec idx state) (inc idx) state))
        (let [replace_element (core-walk element 0 nil)]
          (core-walk(assoc vec idx replace_element) (inc idx) replace_element))))
    vec))

(pprint (core-walk [[\N \?] [\? \L]] 0 nil))
; Logic when using with indented print.
; Define auxillary print functions
(def indent 0)
(defn repeatstr
  [s n]
  (apply str (repeat n s)))

(defn iprintln
  [& rest]
  (apply println (repeatstr "  " indent) rest))

; Define Walk-inner
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

; wrapper around walk-inner that lets us use iprint. Basically a decorator.
(defn Walk
  [& args]
  (do
    (let [old-indent indent]
      (def indent (inc indent))
      (let [result (apply Walk-inner args)]
        (def indent old-indent)
        result))))
