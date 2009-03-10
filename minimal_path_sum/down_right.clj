(defn split-by-whitespace
  "Takes a string, splits it by any sequence of whitespace characters, and returns a lazy sequence: String -> (Strings)"
  [str]
  (.split str "\\s+"))

(defn split-by-comma
  "Takes a string, splits it by a single comma, and returns a lazy sequence: String -> (Strings)"
  [str]
  (.split str ","))

(defn integerize
  [strings]
  (map #(Integer. %) strings)) 

(defn split-file
  "Takes a filepath, opens the file, and retruns a 2-D vector of Strings by splitting first according to fn split-into-lines and then fn split-line: filename ->[[String]]"
  [file-name split-into-lines split-line]
  (let [file-contents (slurp file-name)]
    (apply vector (map #(apply vector %)
		       (map split-line (split-into-lines file-contents))))))


(defn two-d-vector-lookup [v]
	(fn [x y] ((v y) x)))



;;;; Sample usage

; (def cell (two-d-vector-lookup (split-file "/path/to/matrix.txt"
;		  split-by-whitespace
;		  split-by-comma)))

(def matrix-file "/Users/michael/Documents/clojure-study-dc/minimal_path_sum/matrix.txt")
(def test-matrix-file "/Users/michael/Documents/clojure-study-dc/minimal_path_sum/matrix-small.txt")

(def cell (two-d-vector-lookup (split-file matrix-file
					   split-by-whitespace
					   (comp integerize split-by-comma))))

(def dim 80)

;;;; Naive crawler, single-threaded
;;   At each cell:
;;   1. find value and add it to the path-sum thus far
;;   2. check bounds: if at 'right' bound and 'bottom' bound, report path and value
;;                    if not at right bound, recur with 'right' cell
;;                    if not at bottom bound, recur with 'lower' cell
;;   3. At each recursion, pass along next cell coords, path, and path-sum.
;;   4. When reporting path and path-sum, compare with global value, replace if path-sum is lower or nil

(defstruct search-state :coords :path :path-sum)

(def search-results (ref nil))

(defn report-path [search-state]
  (dosync (alter search-results #(if (or (nil? %) (< (:path-sum search-state) (:path-sum %))) search-state %))))

;;; naive: the O2^n rate of growth clobbers this
; user> (time (handle-cell-naive start)) ; 11 x 11
; "Elapsed time: 528.115 msecs"
; user> (time (handle-cell-naive start)) ; 12 x 12
; "Elapsed time: 2199.156 msecs"
; user> (time (handle-cell-naive start)) ; 13 x 13
; "Elapsed time: 7870.555 msecs"

(defn handle-cell-naive [{[x y] :coords path :path path-sum :path-sum :as strc}]
  (let [val (cell x y)
	new-path-sum (+ path-sum val)
	right? (< x (- dim 1))
	down? (< y (- dim 1))]
   (cond (and right? down?) (do
			       (handle-cell-naive {:coords [(inc x) y] :path (cons [x y] path) :path-sum new-path-sum})
			       (handle-cell-naive {:coords [x (inc y)] :path (cons [x y] path) :path-sum new-path-sum}))
	  right? (handle-cell-naive {:coords [(inc x) y] :path (cons [x y] path) :path-sum new-path-sum})
	  down? (handle-cell-naive {:coords [x (inc y)] :path (cons [x y] path) :path-sum new-path-sum})
	  :else (report-path {:path (cons [x y] path) :path-sum  new-path-sum}))))

; At least it works:
; With the test 5x5 matrix
; (handle-cell-naive start)
; {:path ([4 4] [3 4] [3 3] [3 2] [2 2] [2 1] [1 1] [0 1] [0 0]), :path-sum 2427}
; (reduce + '(131 201 96 342 746 422 121 37 331))
; 2427

;;; solution: track path-sum at each location so that a handle-cell recursion path will terminate if it reaches a cell with a higher path sum that the lowest reported. This will kill off recursions such that rate of growth will be On.
;;  So we'll have to keep track of the path-sum to each of the cells


(def scores
     (apply vector 
            (map (fn [_] 
                   (apply vector (map (fn [_] (ref nil)) 
                                      (range dim)))) 
                 (range dim))))
(def score (two-d-vector-lookup scores))

(defn update-cell-path-sum [[x y :as coords] path-sum]
  (let [score-ref (score x y)]
   (dosync (alter score-ref
		  (fn [v] (if (or (nil? v) (< path-sum v)) path-sum v))))))

(defn handle-cell-aware [{[x y] :coords path :path path-sum :path-sum :as strc}]
  (let [val             (cell x y)
	new-path-sum    (+ path-sum val)
	right?          (< x (- dim 1))
	down?           (< y (- dim 1))
	lowest-path-sum (update-cell-path-sum [x y] new-path-sum)]
    (if (not (> new-path-sum lowest-path-sum))
      (cond (and right? down?) (do
				 (handle-cell-aware {:coords [(inc x) y] :path (cons [x y] path) :path-sum new-path-sum})
				 (handle-cell-aware {:coords [x (inc y)] :path (cons [x y] path) :path-sum new-path-sum}))
	    right? (handle-cell-aware {:coords [(inc x) y] :path (cons [x y] path) :path-sum new-path-sum})
	    down? (handle-cell-aware {:coords [x (inc y)] :path (cons [x y] path) :path-sum new-path-sum})
	    :else (report-path {:path (cons [x y] path) :path-sum  new-path-sum})))))

;; With a 12x12 matrix:
; user> (time (handle-cell-naive
;	     start)
;	    )
; "Elapsed time: 4291.478 msecs"
; {:path ([11 11] [11 10] [11 9] [10 9] [9 9] [8 9] [8 8] [7 8] [6 8] [6 7] [5 7] [4 7] [3 7] [3 6] [3 5] [3 4] [3 3] [3 2] [2 2] [2 1] [1 1] [0 1] [0 0]), :path-sum 72284}
; user> (time (handle-cell-aware
; 	     start)
;	    )
; "Elapsed time: 23.015 msecs"
; nil
; user> @search-results
; {:path ([11 11] [11 10] [11 9] [10 9] [9 9] [8 9] [8 8] [7 8] [6 8] [6 7] [5 7] [4 7] [3 7] [3 6] [3 5] [3 4] [3 3] [3 2] [2 2] [2 1] [1 1] [0 1] [0 0]), :path-sum 72284}
;
;; With the 80x80 matrix:
; (time (handle-cell-aware start))
; "Elapsed time: 13102.734 msecs"
; (reverse (:path @search-results))
; ([0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [6 1] [7 1] [7 2] [8 2] [8 3] [9 3] [10 3] [11 3] [11 4] [12 4] [13 4] [14 4] [15 4] [16 4] [17 4] [17 5] [18 5] [19 5] [20 5] [21 5] [22 5] [23 5] [23 6] [24 6] [25 6] [26 6] [27 6] [28 6] [29 6] [29 7] [29 8] [30 8] [30 9] [31 9] [31 10] [31 11] [32 11] [33 11] [33 12] [33 13] [33 14] [33 15] [33 16] [33 17] [34 17] [34 18] [34 19] [34 20] [34 21] [34 22] [35 22] [35 23] [35 24] [36 24] [37 24] [37 25] [38 25] [38 26] [39 26] [40 26] [41 26] [41 27] [42 27] [43 27] [43 28] [43 29] [43 30] [43 31] [43 32] [44 32] [45 32] [46 32] [46 33] [46 34] [46 35] [46 36] [46 37] [46 38] [46 39] [47 39] [48 39] [48 40] [48 41] [48 42] [48 43] [48 44] [49 44] [49 45] [49 46] [49 47] [50 47] [50 48] [51 48] [51 49] [52 49] [52 50] [53 50] [54 50] [55 50] [55 51] [55 52] [56 52] [57 52] [58 52] [58 53] [59 53] [60 53] [60 54] [60 55] [60 56] [60 57] [60 58] [60 59] [60 60] [60 61] [60 62] [61 62] [62 62] [63 62] [64 62] [65 62] [66 62] [66 63] [66 64] [66 65] [66 66] [66 67] [67 67] [68 67] [69 67] [70 67] [71 67] [71 68] [72 68] [72 69] [73 69] [74 69] [75 69] [76 69] [76 70] [76 71] [77 71] [77 72] [78 72] [78 73] [78 74] [78 75] [78 76] [78 77] [78 78] [78 79] [79 79])
; user> (:path-sum @search-results)
; 427337

(def matrix (split-file matrix-file split-by-whitespace split-by-comma))

(def dims (count matrix))

(defstruct cell-report :coords :path-sum :path-history)

(def test-cp0 (struct cell-report [2 1] 5 '()))
(def test-cp1 (struct cell-report [1 1] 2 '()))
(def test-cp2 (struct cell-report [1 1] 4 '()))
(def test-cp3 (struct cell-report [1 2] 3 '()))

(def cps (vector test-cp0 test-cp1 test-cp2 test-cp3)) 

(defn lower-path-sum [cp1 cp2] 
  (if (< (:path-sum cp1) (:path-sum cp2)) cp1 cp2))

; user> (lower-path-sum test-cp2 test-cp1)
; {:coords [1 1], :path-sum 2, :path-history ()}

(defn same-coords? [cp1 cp2]
  (= (:coords cp1) (:coords cp2)))

(defn iter-reduce-reports
  [processed-reports reports-to-do]
  "iterative procedure that conjoins either the next report-to-do or the item among the next 2 reports-to-do that is returned by lower-path-sum"
  (cond (nil? reports-to-do) processed-reports
	(= (count reports-to-do) 1) (conj processed-reports (first reports-to-do))
	(same-coords? (first reports-to-do) (second reports-to-do))
	(recur (conj processed-reports (lower-path-sum (first reports-to-do) (second reports-to-do))) (nthrest reports-to-do 2)) 
	:else (recur (conj processed-reports (first reports-to-do)) (rest reports-to-do))))

(defn reduce-reports
  [reports-to-do]
  (iter-reduce-reports (vector) reports-to-do))
	  
 
; user> (same-coords? test-cp1 test-cp3)
; false


(def cell-val (comp #(Integer. %) (two-d-vector-lookup matrix)))

(def start-report (struct cell-report [0 0] (cell-val 0 0) (vector [0 0])))

(defn find-neighbor-generator [{ [x y] :coords sum :path-sum history :path-history :as report}]
  (fn [[dx dy]]
    (let [nx (+ x dx)
	  ny (+ y dy)]
      (if (and (< nx dims) (< ny dims))
	(struct cell-report [nx ny] (+ sum (cell-val nx ny)) (conj history [nx ny]))
	nil))))

(defn report-on-neighbors [{ [x y] :coords sum :path-sum history :path-history :as report}]
  (let [find-neighbor (find-neighbor-generator report)]
    (pmap find-neighbor (list [0 1] [1 0]))))

(defn next-reports [reports]
  (filter #(not (nil? %)) (apply concat (map report-on-neighbors reports))))

(defn completed? [reports]
  (and (= (count reports) 1)
       (= (inc ((:coords (reports 0)) 0)) dims)
       (= (inc ((:coords (reports 0)) 1)) dims)))

(defn advance [reports]
  (if (completed? reports)
    (reports 0)
    (let [next-reports-seq (next-reports reports)]
      (recur (reduce-reports next-reports-seq)))))
      
; user> (time (advance (vector start-report)))
; "Elapsed time: 200.811 msecs"
; {:coords [79 79], :path-sum 427337, :path-history [[0 0] [1 0] [2 0] [3 0] [4 0] [5 0] [6 0] [6 1] [7 1] [7 2] [8 2] [8 3] [9 3] [10 3] [11 3] [11 4] [12 4] [13 4] [14 4] [15 4] [16 4] [17 4] [17 5] [18 5] [19 5] [20 5] [21 5] [22 5] [23 5] [23 6] [24 6] [25 6] [26 6] [27 6] [28 6] [29 6] [29 7] [29 8] [30 8] [30 9] [31 9] [31 10] [31 11] [32 11] [33 11] [33 12] [33 13] [33 14] [33 15] [33 16] [33 17] [34 17] [34 18] [34 19] [34 20] [34 21] [34 22] [35 22] [35 23] [35 24] [36 24] [37 24] [37 25] [38 25] [38 26] [39 26] [40 26] [41 26] [41 27] [42 27] [43 27] [43 28] [43 29] [43 30] [43 31] [43 32] [44 32] [45 32] [46 32] [46 33] [46 34] [46 35] [46 36] [46 37] [46 38] [46 39] [47 39] [48 39] [48 40] [48 41] [48 42] [48 43] [48 44] [49 44] [49 45] [49 46] [49 47] [50 47] [50 48] [51 48] [51 49] [52 49] [52 50] [53 50] [54 50] [55 50] [55 51] [55 52] [56 52] [57 52] [58 52] [58 53] [59 53] [60 53] [60 54] [60 55] [60 56] [60 57] [60 58] [60 59] [60 60] [60 61] [60 62] [61 62] [62 62] [63 62] [64 62] [65 62] [66 62] [66 63] [66 64] [66 65] [66 66] [66 67] [67 67] [68 67] [69 67] [70 67] [71 67] [71 68] [72 68] [72 69] [73 69] [74 69] [75 69] [76 69] [76 70] [76 71] [77 71] [77 72] [78 72] [78 73] [78 74] [78 75] [78 76] [78 77] [78 78] [78 79] [79 79]]}

;; using pmap in next-reports and report-on-neighbor
; "Elapsed time: 1303.795 msecs"
; "Elapsed time: 507.872 msecs"
; "Elapsed time: 278.743 msecs"

;; using map
;  "Elapsed time: 390.865 msecs"
;  "Elapsed time: 575.219 msecs"
;  "Elapsed time: 203.156 msecs"
