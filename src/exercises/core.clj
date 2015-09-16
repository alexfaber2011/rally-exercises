(ns exercises.core
  (:gen-class))

(def test-matrix [[0 1 0 0 0]
                  [1 0 0 1 1]
                  [1 1 0 0 1]
                  [0 1 0 0 0]
                  [1 0 0 0 1]])

(defn get-neighbors
  "returns a list of neighbors (left, down, right, up), nil prunning where necessary"
  [matrix row col]
  (let [left (get-in matrix [row (dec col)])
        down (get-in matrix [(dec row) col])
        right (get-in matrix [row (inc col)])
        up (get-in matrix [(inc row) col])]
    (->> (list left down right up)
         (remove nil?))))