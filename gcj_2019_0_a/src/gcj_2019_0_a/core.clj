(ns gcj-2019-0-a.core)

(require '[clojure.string :as str])

; core logic
(defn solve
  [num]
  (let [int-num (Integer/parseInt num)
        new-num (Integer/parseInt (str/replace num #"4" "3"))]
    [new-num (- int-num new-num)]))

; auxiliary logic.
(defn do-one
  [i]
  (let [N (read-line)
        [A B] (solve N)]
    (println (format "Case #%d: %s %s" i A B))))

(defn do-stdin
  []
  (let [T (read-string (read-line))]
    (dotimes [i T]
      (do-one (inc i)))))

(defn do-file
  [fname]
  (with-open [rdr (clojure.java.io/reader fname)]
    (binding [*in* rdr]
      (do-stdin))))

(defn -main
  [& [fname]]
  (if fname
    (do-file fname)
    (do-stdin)))

(-main)