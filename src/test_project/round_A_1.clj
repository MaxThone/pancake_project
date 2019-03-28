; This replaces for one segment of a vector, and it works!
(defn replace_with_letter
  [start_idx end_idx letter cake_vector]
  (if (= start_idx end_idx)
    cake_vector
    (replace_with_letter
      (+ start_idx 1)
      end_idx
      letter
      (assoc cake_vector start_idx letter))))

(defn get_replace_data
  [cake_vector]
  (let [is? (fn [x] (= x \?))
        start_idx (.indexOf cake_vector \?)
        letter (if (= 0 start_idx)
                 (first (filter (complement is?) cake_vector))
                 (get cake_vector (- start_idx 1)))
        idx_letter (.indexOf cake_vector letter)
        next_letter (first
                      (filter(complement is?)
                             (subvec cake_vector (+ (max start_idx idx_letter) 1)
                                     (count cake_vector))))
        idx_next_letter (.indexOf cake_vector next_letter)
        end_idx (if (= -1 idx_next_letter) (count cake_vector) idx_next_letter)]
    [start_idx end_idx letter]))


; do not implement the case with all ?
(defn replace_whole_line
  [cake_vector]
  (let [idx_? (.indexOf cake_vector \?)
        replace_data (get_replace_data cake_vector)]
    (if (= idx_? -1)
    cake_vector
    (replace_whole_line
      (apply replace_with_letter (conj replace_data cake_vector))))))

; We assume that cake vector has at least one letter;
; else we already take care of it somewhere else
; else it will not even enter the function
; so two situations;
; ? comes before letter
; Then we want ? up until the first letter
; letter comes before ?
; then we want the letter before the ?
; and go up until end of vector or next letter.

; shit we still need to do like:
; [\G \G \J \? \?] vs
; [\? \G \? \J \?]
; next letter idx =

; NOTE: Empty lines can just be copies of other lines.

; If no ? then return cake_vector
; Else we need to: get first letter
; Grab start of vector until next letter OR end of line
; put that in segment
(def test_vec [\K \J \? \G \? \M \? \? \Z \?])

(def final_vec (replace_whole_line test_vec))
(print final_vec)

; so how do we handle this for multiple lines?
; We need to handle one case: What if a line is only question marks?
; And then: What if the question marks arrives first?
; What if there are 10 lines of question marks and just one line with a letter?
; Case 1: Two lines and no question marks
; Case 2: Basically it should be this
; have a vector of vectors, which we can probably denote.
; So we assume we get the following format:
; [[line_1][line_2][line_3]]
(def test_pie [[\? \? \? \?] [\K \? \J \?] [\? \? \? \?] [\? \? \? \?] [\? \? \L \?] [\? \? \? \?]])

(def plus-five (partial + 5))
(plus-five 10) ; => (+ 5 10)

(filter odd? '(2 4 6  8 10))

(defn fill_pie
  [pie idx state]
  (let
    [is_q (partial = \?)
     cake_vector (get pie idx)]
    (if (= idx (count pie))
      pie
      (if (every? is_q cake_vector)
        (if
          (= state nil)
          (let [rest-of-pie (fill_pie pie (inc idx) state)]
            (assoc rest-of-pie idx (get rest-of-pie (inc idx))))
          (fill_pie (assoc pie idx state) (inc idx) state))
        (fill_pie (assoc pie idx (replace_whole_line cake_vector)) (inc idx) (replace_whole_line cake_vector))))))