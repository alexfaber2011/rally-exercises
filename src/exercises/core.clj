(ns exercises.core
  (:gen-class))

(def test-matrix [[0 1 0 0 0]
                  [1 0 0 1 1]
                  [1 1 0 0 1]
                  [0 1 0 0 0]
                  [1 0 0 0 1]])

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
    (let [parsed-lines (->> input
                            clojure.string/split-lines
                            (mapv clojure.string/trim)
                            parse-lines)]
      [parsed-lines nil])
    (catch Exception e [nil e])))

(defn get-neighbors
  "returns a vector of neighbors (left, down, right, up), nil prunning where necessary"
  [matrix row col]
  (->> (for [row-adj [(dec row) row (inc row)]]
         (for [col-adj [(dec col) col (inc col)]]
           (when-not (and (= row-adj row) (= col-adj col))
             (get-in matrix [row-adj col-adj]))))
       flatten
       (remove nil?)
       vec))

(defn get-neighbor-count
  "returns an integer representing the number of 1's in the neighborhood"
  [neighbors]
  (reduce + neighbors))

(defn toggle-cell?
  [matrix row col]
  (let [cell (get-in matrix [row col])
        neighbor-count (-> (get-neighbors matrix row col)
                           get-neighbor-count)]
    (cond
      (and (= cell 1) (< neighbor-count 2)) true
      (and (= cell 1) (<= 2 neighbor-count 3)) false
      (and (= cell 1) (> neighbor-count 3)) true
      (and (= cell 0) (= neighbor-count 3)) true
      :else false)))

(defn find-cells-to-toggle
  [matrix]
  "takes a matrix (vector of vectors of integers) and spits out all the coordinates of the cells to toggle"
  (let [height (count matrix)
        width (count (get matrix 0))]
    (->> (for [row (range height)]
           (for [col (range width)]
             (when (toggle-cell? matrix row col)
               [row col])))
         flatten
         (remove nil?)
         (partition 2)
         (mapv vec))))

(defn exists-in?
  [seq elem]
  (some #(= elem %) seq))

(defn toggle-cell
  "turns a 1 into a 0 and an 0 into a 1"
  [cell]
  (if (= cell 0) 1 0))

(defn rebuild-cells
  "takes a list of coordinates to toggle, and returns a vector of vectors that results after an iteration
   of the game logic is applied."
  [matrix cell-cords-to-toggle]
  (let [height (count matrix)
        width (count (get matrix 0))]
    (->> (for [row (range height)]
           (for [col (range width)]
             (let [cell (get-in matrix [row col])]
               (if (exists-in? cell-cords-to-toggle [row col])
                 (toggle-cell cell)
                 cell))))
         (mapv vec))))

(defn evolve
  [input]
  (let [[parsed-input exception] (parse-input input)]
    (if parsed-input
      (->> parsed-input
           find-cells-to-toggle
           (rebuild-cells parsed-input))
      (str "Unable to parse gameboard. " (.getMessage exception)))))