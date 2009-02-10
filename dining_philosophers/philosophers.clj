(ns dining_philosophers)

;;;;; The dining philosophers problem:
;;;;; An implementation of the Misra-Chandy solution in Clojure
;;;;; Michael Harrison, January 2009.
;;;;; mh@michaelharrison.ws
;;;;; michaelharrison.ws/weblog
;;;;;
;;;;; About the problem and the Misra-Chandy solution:
;;;;; http://en.wikipedia.org/wiki/Dining_philosopher%27s_problem
;;;;; Influenced by Rick Hickey's Ant simulation, described here:
;;;;; http://clojure.blip.tv/file/812787/
;;;
;;; 1. For every pair of philosophers contending for a resource, create
;;; a fork and give it to the philosopher with the lower ID. Each fork
;;; can either be dirty or clean. Initially, all forks are dirty.
;;; 2. When a philosopher wants to use a set of resources (i.e. eat),
;;; he must obtain the forks from his contending neighbors. For all
;;; such forks he does not have, he sends a request message.
;;; 3. When a philosopher with a fork receives a request message, he
;;; keeps the fork if it is clean, but gives it up when it is dirty.
;;; If he sends the fork over, he cleans the fork before doing so.
;;; 4. After a philosopher is done eating, all his forks become dirty.
;;; If another philosopher had previously requested one of the forks,
;;; he cleans the fork and sends it.
;;;
;;; My iterpretation of the rules:
;;; * when a philosopher starts thinking, his forks become dirty.
;;; * a philosopher may start thinking on any turn, whether he's eaten or not.
;;; * requests do not persist or have state per se: if a dirty fork is
;;;   proximal to a needy philosopher, it is handed over. Otherwise,
;;;   nothing happens.



;;; philosophers ;;;

(defstruct philosopher :name :id :food :thoughts :forks)

(defn new-philosopher [name id]
     (struct philosopher name id 0 1 '()))

(def philosophers
     (apply vector
	    (map
	     #(ref %)
	     [(new-philosopher "Confucius" 0)
	      (new-philosopher "Descartes" 1)
	      (new-philosopher "James" 2)
	      (new-philosopher "Plato" 3)
	      (new-philosopher "Russell" 4)])))

(def num-philosophers (count philosophers))
(def range-philosophers (range num-philosophers))

(defn bound
  "returns n wrapped into range 0-b: looping or wheel"
  [b n]
  (let [n (rem n b)]
    (if (neg? n)
      (+ n b)
      n)))

(defn wrand 
  "given a vector of slice sizes, returns the index of a slice given a
  random spin of a roulette wheel with compartments proportional to
  slices."
  [slices]
  (let [total (reduce + slices)
        r (rand total)]
    (loop [i 0 sum 0]
      (if (< r (+ (slices i) sum))
        i
        (recur (inc i) (+ (slices i) sum))))))

(defn prev-place-id [id]
  (bound num-philosophers (dec id)))

(defn next-place-id [id]
  (bound num-philosophers (inc id)))

(defstruct fork :cleanliness)

(defn clean-fork []
  (struct fork "clean"))

(defn dirty-fork []
  (struct fork "dirty"))

(defn soil [forks]
  (if (empty? forks)
    '()
    (cons (dirty-fork) (soil (rest forks)))))

(defn add-fork [to-place fork]
  "Must be called within transaction"
    (if (< (count (:forks to-place)) 2)
      (assoc to-place :forks (conj (:forks to-place) fork))))


;; Create a fork "between" each philosopher and his next next neighbor,
;; and give the fork to the philosopher with the lowest ID (the entry
;; with the lowest index)
(loop [i 0]
  (if (not (= i num-philosophers))
    (do
      (let [id (min i (next-place-id i))
	    philosopher (philosophers id)]
	
	(dosync (alter philosopher
		       add-fork
		       (struct fork "dirty"))))
      (recur (inc i)))))



;;; Eating:

(defn can-eat? [philosopher]
  (= 2 (count (:forks @philosopher))))

(defn feed [philosopher]
  (alter philosopher
	 (fn [ph-val]
	   (assoc ph-val
	     :food
	     (inc (:food ph-val))))))

(defn find-forks
  "Examines forks and returns a structure {:dirty <first dirty fork found, if found> :others <a list containing the remaining fork if any>"
  ([to-search searched]
     (if (empty? to-search)
       {:others searched}
       (let [fork (first to-search)
	     remaining-forks  (rest to-search)]
	       (if (= (:cleanliness fork) "dirty")
		 {:dirty fork
		  :others (if (empty? remaining-forks)
			    searched
			    (concat remaining-forks searched))
		  }
		 (recur remaining-forks (cons fork searched))))))
  ([v] (find-forks v '())))


(defn sort-forks-for [philosopher]
  (find-forks (:forks @philosopher)))

(defn choose-neighbor [id]
  (philosophers (([prev-place-id next-place-id](int (rand 2))) id)))

(defn request-fork [philosopher neighbor]
  (let [forks (sort-forks-for neighbor)]
    (if (:dirty forks)
      (do
	(alter neighbor assoc :forks (if (nil? (:others forks))
					  '()
					  (:others forks)))
	(alter philosopher add-fork (clean-fork))))))

(defn choose-activity-for [philosopher]
  (let [food (:food @philosopher)
	thoughts (:thoughts @philosopher)]
    (["think" "eat"](wrand [food thoughts]))))



(defn act [idx]
  (let [philosopher (philosophers idx)]
    (dosync
     (if (= (choose-activity-for philosopher) "think") ; no need to eat
       (alter philosopher assoc
	      :thoughts (inc (:thoughts @philosopher))
	      :forks (soil (:forks @philosopher)))
       (if (can-eat? philosopher)                      ; try to eat
	 (feed philosopher)
	 (request-fork philosopher (choose-neighbor idx)))))
    idx))

(def running (ref true))
(defn running? [] @running)

(defn agent-act [idx]
    (let [philosopher (philosophers idx)]
      (. Thread (sleep 100))
      (dosync
       (when (running?)
	 (send-off *agent* #'agent-act))
       (if (= (choose-activity-for philosopher) "think")
	 (alter philosopher assoc
		:thoughts (inc (:thoughts @philosopher))
		:forks (soil (:forks @philosopher)))
	 (if (can-eat? philosopher)
	   (feed philosopher)
	   (request-fork philosopher (choose-neighbor idx)))))
      idx))

; Non-concurrent way to cycle through philosophers (by seat) and have
; each act in turn.
(defn cycle-seats [num-cycles]
  (for [i (range num-cycles) idx range-philosophers]
    (act idx)))

(def agents (apply vector (for [i range-philosophers]
			    (agent i))))



;;; Report

(defn report-philosopher [philosopher]
  (let [p philosopher forks (apply vector (:forks p))]
  (format  "Name: %12s   Food eaten: %4d   Forks: %s"
	   (:name p)
	   (:food p)
	   (str (for [i (range (count forks))]
		     (:cleanliness (forks i)))))))



;;; drivers: use (start) to start, (stop) to stop, and (report) to report before, during, and after the agents run.
(defn report []
  (let [snapshots (dosync (apply vector (map deref philosophers)))]
    (for [i (range (count snapshots))]
      (do
	(newline)
	(println (report-philosopher (snapshots i)))
	""))))

(defn start []
  (dosync (ref-set running true))
  (str "starting: " (map #(send-off % agent-act) agents)))

(defn stop []
  (dosync (ref-set running false))
  (println "stopping")
  (println (report)))
  