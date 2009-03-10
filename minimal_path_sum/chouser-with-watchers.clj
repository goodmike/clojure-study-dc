(defn parse-grid
  "Take a string that contains lines of comma-separated numbers, and
return a vector of vectors of Integers"
  [s]
  (vec (map (fn [line] (vec (map #(Integer. %) (.split line ","))))
            (.split s "\\s+"))))
 
(defn neighbor-seqs
  "Returns a vector of vectors of lazy seqs representing a square
matrix with size rows. Each lazy seq contains [y x] coordinates of
this cell's four neighbors to the north, south, east, and west."
  [size]
  (let [delta [[-1 0] [1 0] [0 -1] [0 1]]]
    (vec (for [y (range size)]
           (vec (for [x (range size)]
                  (filter (fn [nyx] (every? #(< -1 % size) nyx))
                          (map #(map + [y x] %) delta))))))))
 
(defn min-sum
  "Calculates a new best-known cost sum to reach the given cell, based
on its own cost plus the smallest cost sum of its neighbors.
get-sum must be a function that takes a single [y x] coordinate
vector and returns the current cost sum of that cell, or nil if its
not known."
  [oldsum yx costs neighbors get-sum]
  (let [neighbor-sums (filter identity (map get-sum (get-in neighbors yx)))
        newsum (+ (get-in costs yx)
                  (if neighbor-sums (apply min neighbor-sums) 0))]
    (if (or (nil? oldsum) (< newsum oldsum))
      newsum
      oldsum)))
 
(defn use-queue
  "Updates the cost sum of the top-left cells, adds its neighbors to a
work queue, and then repeats the process for all cells in the queue.
When the work queue is empty, the correct minimal cost is in the
bottom-right cell. The cost sum for that cell is returned."
  [costs size neighbors]
  (loop [sums (vec (replicate size (vec (replicate size nil))))
         work-queue (conj clojure.lang.PersistentQueue/EMPTY [0 0])]
    (if (empty? work-queue)
      (peek (peek sums))
      (let [yx (peek work-queue)
            oldsum (get-in sums yx)
            newsum (min-sum oldsum yx costs neighbors #(get-in sums %))]
        (if (== oldsum newsum)
          (recur sums (pop work-queue))
          (recur (assoc-in sums yx newsum)
                 (apply conj (pop work-queue) (get-in neighbors yx))))))))
 
(defn use-agents
  "Sets up a grid of agents. Sets up a watcher for each agent that
sends update actions to its neighboring agents. A count of all
scheduled cell agents is kept so that when all agent updates are
complete, the 'done' semaphore is released.This allows the calling
thread to collect and return the bottom-right agent's value."
  [costs size neighbors]
  (let [sums (vec (for [_ costs] (vec (for [_ costs] (agent nil)))))
        get-sum #(deref (get-in sums %))
        done (java.util.concurrent.Semaphore. 0)
        remaining (java.util.concurrent.atomic.AtomicInteger. 1)]
    (doseq [y (range size) x (range size)]
      (add-watch (get-in sums [y x]) :send (agent nil)
                   (fn [_ _]
                     (let [nbs (get-in neighbors [y x])]
 
                       ; Before asking neighbors to update, increment
                       ; 'remaining' for all of them, but 'dec' once
                       ; for ourselves since we're now running.
                       (.addAndGet remaining (dec (count nbs)))
 
                       ; Now send 'min-sum' to all neighbors
                       (doseq [nyx nbs]
                         (send (get-in sums nyx)
                               (fn [oldsum]
                                 (let [newsum (min-sum oldsum nyx costs
                                                       neighbors get-sum)]
                                   ; If the sum is different we must
                                   ; not decrement 'remaining' yet, or
                                   ; it may touch zero even though
                                   ; watchers will still do more work.
                                   (when (== oldsum newsum)
                                     (when (zero? (.decrementAndGet remaining))
                                       (.release done)))
                                   newsum))))))))
    (send (ffirst sums) min-sum [0 0] costs neighbors get-sum)
    (.acquire done)
    @(peek (peek sums))))
 
(defn main
  "Solves the puzzle using either the use-queue or use-agent func above.
gridstr is the input grid given as described in parse-grid."
  [func gridstr]
  (let [costs (parse-grid gridstr)
        size (count costs)
        neighbors (neighbor-seqs size)]
    (func costs size neighbors)))