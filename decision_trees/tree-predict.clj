;; replace with actual fn from clojure.contrib.seq-utils
(defn separate
  "Returns a vector:
   [ (filter f s), (filter (complement f) s) ]"
  [f s]
  [(filter f s) (filter (complement f) s)])

;; for loading tabular data into 2D-vector representation
(def file-contents (slurp "/Users/michael/Documents/clojure-study-dc/decision_trees/example.txt"))

(def data (apply vector (map #(apply vector %)
			(map #(.split % "\\t") (#(.split % "\\n") file-contents)))))

;; tree node abstraction and helper constructor:
(defstruct decision-node :col :value :results :true-branch :false-branch)

(defn make-decision-node
  ([{col :col value :value results :results true-branch :true-branch false-branch :false-branch
     :or {col -1 value nil results nil true-branch nil false-branch nil}}]
     (struct decision-node col value results true-branch false-branch))
  ([]
     (make-decision-node {})))

;; Classification And Regression Trees algorithm
(defn divide-set
  "divides data rows into two sets based on the data in a specific column:
   [row][col], col, test-value => {:true-set :false-set}"
  [rows col value]
  (let [test-op
	(if (.isInstance java.lang.Number value)
	  >=
	  =)
	[true-set false-set] (separate #(test-op (% col) value) rows)]
    {:true-set true-set :false-set false-set}))

(defn unique-counts
  "count occurences of each result in a set of rows:
  [row][col], result-col => {[result occurences]}"
  [rows, result-col]
  (reduce (fn [accum row] (assoc accum (row result-col) (inc (accum result-col)))) {} rows))
    
