(ns gcj-2019-0-b.core)

(require '[clojure.string :as str])

(defn flip
  [x]
  (if (= \E x) \S \E))

(defn solve
  [input-str]
  (apply str (map flip input-str)))

; auxiliary logic.
(defn do-one
  [i]
  (let [size (read-line)
        input-str (read-line)
        output-str (solve input-str)]
    (println (format "Case #%d: %s" i output-str))))

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
