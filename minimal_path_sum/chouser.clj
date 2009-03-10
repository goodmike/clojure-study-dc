(defn parse-grid [s]
  (vec (map (fn [line] (vec (map #(Integer. %) (.split line ","))))
            (.split s "\\s+"))))
 
(defn main [gridstr]
  (let [costs (parse-grid gridstr)
        size (count costs)
        min-sums (vec (for [_ costs] (vec (for [_ costs] (agent nil)))))
        done (java.util.concurrent.Semaphore. 0)
        remaining (java.util.concurrent.atomic.AtomicInteger. 1)
        calc (fn calc [oldsum yx]
               (let [nbs (filter (fn [nyx] (every? #(< -1 % size) nyx))
                                 (map #(map + yx %)[[1 0] [0 1]]))
                     newsum (+ (get-in costs yx)
                               (or (apply min
                                          (or (filter identity
                                                      (map #(deref (get-in min-sums %)) nbs))
                                              [nil]))
                                   0))]
                 (send (agent nil) (fn [_]
                   (when (zero? (.decrementAndGet remaining))
                     (.release done))))
                 (if (or (nil? oldsum) (< newsum oldsum))
                   (do
                     (.addAndGet remaining (count nbs))
                     (doseq [nyx nbs]
                       (send (get-in min-sums nyx) calc nyx))
                     newsum)
                   oldsum)))]
    (send (ffirst min-sums) calc [0 0])
    (.acquire done)
    @(peek (peek min-sums))))

;(prn (time (main (slurp "matrix.txt"))))