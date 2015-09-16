(ns exercises.core
  (:gen-class))

(def test-matrix [[0 1 0 0 0]
                  [1 0 0 1 1]
                  [1 1 0 0 1]
                  [0 1 0 0 0]
                  [1 0 0 0 1]])

(defn get-neighbors
  "returns a vector of neighbors (left, down, right, up), nil prunning where necessary"
  [matrix row col]
  (let [left (get-in matrix [row (dec col)])
        down (get-in matrix [(dec row) col])
        right (get-in matrix [row (inc col)])
        up (get-in matrix [(inc row) col])]
    (->> [left down right up]
         (remove nil?)
         vec)))

(defn get-neighbor-count
  "returns an integer representing the number of 1's in the neighborhood"
  [neighbors]
  (reduce + neighbors))

(defn parse-lines
  "takes a vector of strings of numbers and returns a 2d vector containing only integers"
  [vec-of-strings]
  (vec (for [line vec-of-strings]
         (vec (for [char line]
                (-> (str char)
                    Integer/parseInt))))))

(defn parse-input
  "takes repl input in the form of:

    01000
    10011
    11001
    01000
    10001

   and turns it into a vector of vectors of integers"
  [input]
  (try
    (->> input
         clojure.string/split-lines
         (mapv clojure.string/trim)
         parse-lines)
    (catch Exception e (throw e))))

(defn evolve
  [input]
  (try
    (parse-input input)
    (catch Exception e (str "Unable to parse gameboard. " (.getMessage e)))))