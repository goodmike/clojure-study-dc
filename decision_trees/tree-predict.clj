;; replace with actual fn from clojure.contrib.seq-utils
(defn separate
  "Returns a vector:
   [ (filter f s), (filter (complement f) s) ]"
  [f s]
  [(filter f s) (filter (complement f) s)])

;; for loading tabular data into 2D-vector representation
(def file-contents (slurp "/Users/michael/Documents/clojure-study-dc/decision_trees/longer-example.txt"))

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
	(if (.isInstance Number value)
	  >=
	  =)
	[true-set false-set] (map vec (separate #(test-op (% col) value) rows))]
    {:true-set true-set :false-set false-set}))

(defn unique-counts
  "count occurences of each result in a set of rows:
  [row][col], result-col => {[result occurences]}"
  [rows, result-col]
  (reduce (fn [accum row]
	    (let [result (row result-col)
		  rcount (or (accum result) 0)]
	      (assoc accum result (inc rcount))))
	  {} rows))

(defn last-col [row] (dec (count row)))

(defn entropy-of-set
  "Measures how different the values of the set are. No variation yields a 0.
  The more mixed-up the set's values, the higher the number.
  Takes a function for finding unique counts:
  [row][col], counts-fn => double"
  [count-fn rows]
  (if (empty? rows)
    0.0
    (let [log2     #(/ (Math/log %) (Math/log 2))
	  last-col (last-col (rows 0))
	  results  (count-fn rows last-col)]
      (reduce (fn [accum [key val]]
		(let [p (/ val (count rows))]
		  (- accum (* p (log2 p)))))
	      0 results))))

(def score-fn (partial entropy-of-set unique-counts)) ; pollute the global env for now

(defn analyze-column [{start-score :start-score :as bests} rows col-num]
  (let [best-branch-for-cell
	(fn [accum cell-val]
	  (let [{true-set :true-set false-set :false-set} (divide-set rows col-num cell-val)
		  p (/ (count true-set) (count rows))
		  gain (- start-score (* p (score-fn true-set)) (* (- 1 p) (score-fn false-set)))]
	    (if (and (> gain (:gain bests))
		     (not (nil? true-set))
		     (not (nil? false-set)))
	      (assoc bests
		:gain gain
		:criteria {:col col-num :value cell-val}
		:sets {:true-set true-set :false-set false-set})
	      bests)))]
	       
  (reduce best-branch-for-cell
	  bests
	  (reduce (fn [accum col] (conj accum col)) #{} (map (fn [row] (row col-num)) rows)))))

;; Recursive tree-building
;
;  user> (build-tree data)
; {:col 0, :value "google", :results nil, :true-branch {:col 2, :value "yes", :results nil, :true-branch ...
;
(defn build-tree
  [rows]
  (if (= (count rows) 0)
    (make-decision-node)
    (let [{gain :gain, {col :col value :value} :criteria, {true-set :true-set false-set :false-set} :sets }
	  (loop ; through columns to find best split for this set
	      [bests {:gain 0 :criteria nil :sets nil :start-score (score-fn rows)}
	       col          (dec (last-col (rows 0)))]
	    (if (>= col 0)
	      (recur (analyze-column bests rows col) (dec col))
	      bests))]
      (if (> gain 0)
	(make-decision-node {:col col :value value
			     :true-branch (build-tree true-set) :false-branch (build-tree false-set)})
	(make-decision-node {:results (unique-counts rows (last-col (rows 0)))})))))

; Conventional crawling the tree to classify a new observation:
(defn classify [observation tree]
  (if (not (nil? (:results tree)))
    (:results tree)
    (let [test-op (if (.isInstance Number (:value tree))
		    >=
		    =)]
      (recur observation (if (test-op (observation (:col tree)) (:value tree))
			      (:true-branch  tree)
			      (:false-branch tree))))))


;; for timing (and comparing) this and future versions of build-tree
(defmacro times [n body]
	"Time the execution of body n times"
	`(time (dotimes [x# ~n] ~body)))


; You come at build-tree with a set of rows and a sample data set (also a row in structure)

(defn lazy-build-tree
  [rows]
  (lazy-seq (list
    (if (= (count rows) 0)
      (make-decision-node)
    
      (let [{gain :gain, {col :col value :value} :criteria, {true-set :true-set false-set :false-set} :sets }
	    (loop ; through columns to find best split for this set
		[bests {:gain 0 :criteria nil :sets nil :start-score (score-fn rows)}
		 col          (dec (last-col (rows 0)))]
	      (if (>= col 0)
		(recur (analyze-column bests rows col) (dec col))
		bests))]
	(println "in lazy-build-tree")
	(if (> gain 0)
	  (make-decision-node {:col col :value value
			       :true-branch (lazy-build-tree true-set) :false-branch (lazy-build-tree false-set)})
	  (make-decision-node {:results (unique-counts rows (last-col (rows 0)))})))))))

(defn classify-lazy [observation tree-seq]
  (let [tree (first tree-seq)]
    (if (not (nil? (:results tree)))
      (:results tree)
      (let [test-op (if (.isInstance Number (:value tree))
		      >=
		      =)]
	(recur observation (if (test-op (observation (:col tree)) (:value tree))
			     (:true-branch  tree)
			     (:false-branch tree)))))))