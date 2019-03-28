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

(defn replace_whole_line
  [cake_vector]
  (let [idx_? (.indexOf cake_vector \?)
        replace_data (get_replace_data cake_vector)]
    (if (= idx_? -1)
    cake_vector
    (replace_whole_line
      (apply replace_with_letter (conj replace_data cake_vector))))))

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