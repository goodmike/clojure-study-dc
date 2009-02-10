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

(def cell (two-d-vector-lookup (split-file "/Users/michael/Documents/clojure-study-dc/minimal_path_sum/matrix.txt"
		  split-by-whitespace
		  (comp integerize split-by-comma))))

;;;; Naive crawler, single-threaded
;;   At each cell:
;;   1. find value and add it to the path-sum thus far
;;   2. check bounds: if at 'right' bound and 'bottom' bound, report path and value
;;                    if not at right bound, recur with 'right' cell
;;                    if not at bottom bound, recur with 'lower' cell
;;   3. At each recursion, pass along next cell coords, path, and path-sum.
;;   4. When reporting path and path-sum, compare with global value, replace if path-sum is lower or nil

(defstruct search-state :coords :path :path-sum)

(defn report-path [_]
  'foo)

(defn handle-cell [{[x y] :coords path :path path-sum :path-sum :as strc}]
  (let [val (cell x y)
	new-path-sum (+ path-sum val)
	right? (< x 10)
	down? (< y 10)]
    (cond (and right? down?) (do
			       (handle-cell {:coords [(inc x) y] :path (cons [x y] path) :path-sum new-path-sum})
			       (handle-cell {:coords [x (inc y)] :path (cons [x y] path) :path-sum new-path-sum}))
	  right? (handle-cell {:coords [(inc x) y] :path (cons [x y] path) :path-sum new-path-sum})
	  down? (handle-cell {:coords [x (inc y)] :path (cons [x y] path) :path-sum new-path-sum})
	  :else (report-path {:path path :path-sum  path-sum}))))