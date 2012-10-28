(ns paip.chap6
  (:require (clojure [walk :as walk]
                     [pprint :as pprint]
                     [string :as str])))

(defn starts-with?
  [l x]
  (and (list? l)
       (= (first l) x)))

(declare variable? segment-pattern? single-pattern? 
         segment-matcher single-matcher 
         segment-match-fn single-match-fn)

(defn variable?
  "Is x a variable (a symbol beginning with '?')?"
  [x]
  (and (symbol? x) (= (get (str x) 0) \?)))


(defn match-variable
  "Does VAR match input? Uses (or updates) and returns bindings"
  [var input bindings]
  ;(println "Checking var " var " in " bindings " = " input)
  (if-let [binding (bindings var)]
    (if (= binding input) bindings)
    (into bindings {var input})))

(defn pat-match
  "Match pattern against input in the context of the bindings.
  Uses a map for bindings, on success returns the bindings,
  on failure returns nil.  Empty bindings is a valid match, no
  substitutions necessary"
  ([pattern input] (pat-match pattern input '{}))
  ([pattern input bindings]
    ;(println "PAT-MATCH " pattern " = " input)
    (cond 
      (nil? bindings) nil
      (variable? pattern)
        (match-variable pattern input bindings)
      (= pattern input) bindings
      (segment-pattern? pattern)  
        (do (println "Matching pattern " (second (first pattern)) 
                     " : " input)
          (segment-matcher pattern input bindings))
      (single-pattern? pattern)
        (single-matcher pattern input bindings)
      (and (seq? pattern) (seq? input))
        (pat-match (rest pattern) (rest input)
                   (pat-match (first pattern) (first input)
                              bindings))
      :else nil)))

(defn match-partitions
  "Returns lazy sequence of possible partitions of list:
   (before match) (var and after)"
  ([var input]
    (match-partitions var input []))
  ([var input before]
  (cons (list before input) 
    (if-not (empty? input)
      (lazy-seq (match-partitions var (rest input) 
                                      (conj before (first input))))))))


(defn segment-pattern?
  "Is this a segment matching pattern: ((?* var) . pat)"
  [pattern]
  (and (seq? pattern) (seq? (first pattern))
       (segment-match-fn (first (first pattern)))))

(defn segment-matcher
  "Call the right function for this kind of segment pattern"
  [pattern input bindings]
  (apply (segment-match-fn (first (first pattern)))
         (list pattern input bindings)))

(defn segment-match
  "Match the segment pattern ((?* var) . pat) against input."
  [pattern input bindings & {:keys [plus-match]}]
  (let [var (second (first pattern))
        pat (rest pattern)]
    (println "SEGMENT-MATCH " var " in " input " rest of pattern=" pat)
    (if (empty? pat)
      (match-variable var input bindings)
      (some (fn [[bind rest-input]]
              (println "TESTING: " var "=" bind " and " 
                       pat " matches " rest-input)
              (pat-match pat rest-input
                         (match-variable var bind bindings)))
            (if plus-match
              (rest (match-partitions (first pat) input))
              (match-partitions (first pat) input))))))

(defn segment-match+
  "Match one or more elements of input."
  [pattern input bindings]
  (segment-match pattern input bindings :plus-match 1))

(defn segment-match?
  "Match zero or one element of input."
  [pattern input bindings]
  (let [var (second (first pattern))
        pat (rest pattern)]
    (or (pat-match (cons var pat) input bindings)
        (pat-match pat input bindings))))

(defn match-if
  "Test an arbitrary expression involving variables.
  The pattern looks like ((?if code) . rest)"
  [pattern input bindings]
  (and (eval (walk/prewalk-replace bindings (second (first pattern))))
       (pat-match (rest pattern) input bindings)))

(def segment-match-fn
  {'?*  segment-match
   '?+  segment-match+
   '??  segment-match?
   '?if match-if})

(defn single-pattern?
  "Is this a single matching pattern?
  E.g. (:is x predicate) (:and . patterns) (:or . patterns)"
  [pattern]
  (and (seq? pattern)
       (single-match-fn (first pattern))))

(defn single-matcher
  "Call the right function for this kind of single pattern"
  [pattern input bindings]
  (apply (single-match-fn (first pattern))
         (rest pattern) input bindings))

(defn match-is
  "Succeed and bind var if the input satisfies pred.
  where var-and-pred is the list (var pred)"
  [var-and-pred input bindings]
  (let [var (first var-and-pred)
        pred (second var-and-pred)
        bindings (pat-match var input bindings)]
    (if (and bindings (apply pred input))
      bindings)))

(defn match-and
  "Succeed if all the patterns match the input"
  [patterns input bindings]
  (loop [patterns patterns
         bindings bindings]
    (cond (nil? bindings) nil
          (empty? patterns) bindings
          :else (recur (rest patterns)
                       (pat-match (first patterns) input bindings)))))


(defn match-or
  "Succeed if any of the patterns match the input"
  [patterns input bindings]
  (loop [patterns patterns]
    (if-not (empty? patterns)
      (if-let [new-bindings (pat-match (first patterns) input bindings)]
        new-bindings
        (recur (rest patterns))))))

(defn match-not
  "Succeed if none of the patterns match the input
  This will never bind any variables."
  [patterns input bindings]
  (if-not (match-or patterns input bindings)
    bindings))

(def single-match-fn
  {:is  match-is
   :or  match-or
   :and match-and
   :not match-not})

; Tree Search
(defn tree-search 
  "Find a state that satisfies goal-p.  Start with stats,
  and search according to successors and combiner."
  [states goal-p successors combiner]
  (loop [states states]
    (println "Search: " states)
    (cond (empty? states) nil
          (goal-p (first states)) (first states)
          :else (let [next-states (successors (first states))]
                  ;(println "Next states:" next-states)
                  (recur (combiner next-states (rest states)))))))

(defn depth-first-search
  "Search new states first until goal is reached."
  [start goal-p successors]
  (tree-search (list start) goal-p successors concat))

(defn binary-tree [x] (list (* 2 x) (inc (* 2 x))))

(defn prepend [x y] (concat y x))

(defn breadth-first-search
  "Search old states first until goal is reached"
  [start goal-p successors]
  (tree-search (list start) goal-p successors prepend))

(defn finite-binary-tree [n]
  (fn [x] (remove #(> % n) (binary-tree x))))

;paip.chap6=> (breadth-first-search 1 (partial = 12) (finite-binary-tree 15))
;Search:  (1)
;Search:  (2 3)
;Search:  (3 4 5)
;Search:  (4 5 6 7)
;Search:  (5 6 7 8 9)
;Search:  (6 7 8 9 10 11)
;Search:  (7 8 9 10 11 12 13)
;Search:  (8 9 10 11 12 13 14 15)
;Search:  (9 10 11 12 13 14 15)
;Search:  (10 11 12 13 14 15)
;Search:  (11 12 13 14 15)
;Search:  (12 13 14 15)
;12
;paip.chap6=> (depth-first-search 1 (partial = 12) (finite-binary-tree 15))
;Search:  (1)
;Search:  (2 3)
;Search:  (4 5 3)
;Search:  (8 9 5 3)
;Search:  (9 5 3)
;Search:  (5 3)
;Search:  (10 11 3)
;Search:  (11 3)
;Search:  (3)
;Search:  (6 7)
;Search:  (12 13 7)
;12

(defn diff 
  "Return the function that finds the difference from num"
  [num]
  (fn [x] (java.lang.Math/abs (- x num))))

(defn sorter 
  "Return a combiner function that sorts according to cost-fn"
  [cost-fn]
  (fn [new old]
    (sort-by cost-fn (concat new old))))

(defn best-first-search
  "Search lowest cost states first until goal is reached."
  [start goal-p successors cost-fn]
  (tree-search (list start) goal-p successors (sorter cost-fn)))

;paip.chap6=> (best-first-search 1 (partial = 12) binary-tree (diff 12))
;Search:  (1)
;Search:  (3 2)
;Search:  (7 6 2)
;Search:  (14 15 6 2)
;Search:  (15 6 2 28 29)
;Search:  (6 2 28 29 30 31)
;Search:  (12 13 2 28 29 30 31)
;12
 
(defn price-is-right
  "Return a function that measures the difference from price,
  but gives a big penalty for going over price."
  [price]
  (fn [x] (if (> x price)
            Long/MAX_VALUE
            (- price x))))

;paip.chap6=> (best-first-search 1 (partial = 12) binary-tree (price-is-right 12))
;Search:  (1)
;Search:  (3 2)
;Search:  (7 6 2)
;Search:  (6 2 14 15)
;Search:  (12 2 13 14 15)
;12

(defn beam-search
  "Search highest scoring states first until goal is reached,
  but never consider more than beam-width states at a time."
  [start goal-p successors cost-fn beam-width]
  (tree-search (list start) goal-p successors
               (fn [old new]
                 (take beam-width ((sorter cost-fn) old new)))))

(defrecord City [name long lat])

(def ^:dynamic *cities*
  (map (partial apply ->City) 
       '[(Atlanta        84.23  33.45)
         (Boston         71.05  42.21)
         (Chicago        87.37  41.50)
         (Denver        105.00  39.45)
         (Eugene        123.05  44.03)
         (Flagstaff     111.41  35.13)
         (Grand-Jct     108.37  39.05)
         (Houston       105.00  34.00)
         (Indianapolis   86.10  39.46)
         (Jacksonville   81.40  30.22)
         (Kansas-City    94.35  39.06)
         (Los-Angeles   118.15  34.03)
         (Memphis        90.03  35.09)
         (New-York       73.58  40.47)
         (Oklahoma-City  97.28  35.26)
         (Pittsburgh     79.57  40.27)
         (Quebec         71.11  46.49)
         (Reno          119.49  39.30)
         (San-Francisco 122.26  37.47)
         (Tampa          82.27  27.57)
         (Victoria      123.21  48.25)
         (Wilmington     77.57  34.14)]))

(def earth-diameter 12765.0)

(defn xyz-coords 
  "Returns the [x,y,z] coordinates of a point on a sphere.
  The center is (0 0 0) and the north pole is (0 0 1)"
  [city]
  (let [psi (Math/toRadians (:lat city))
        phi (Math/toRadians (:long city))]
    [(* (Math/cos psi) (Math/cos phi))
     (* (Math/cos psi) (Math/sin phi))
     (Math/sin psi)]))

(defn distance 
  "The Euclidian distance between two points.
  The points are coordinates in n-dimensional space."
  [point1 point2]
  (Math/sqrt (reduce + (map (fn [a b] (* (- a b) (- a b)))
                            point1 point2))))

(defn air-distance
  "The great circle distance between two cities"
  [city1 city2]
  (let [d (distance (xyz-coords city1) (xyz-coords city2))]
    (* earth-diameter (Math/asin (/ d 2)))))

(defn neighbor-fn
  "Returns function that find all cities within 1000 kilometers"
  [cities]
  (fn [city] 
    (filter (fn [c] (and (not (= c city))
                         (< (air-distance c city) 1000.0)))
            cities)))

(defn find-city
  [cities city]
  (some #(if (= city (:name %)) %) cities))

(defn trip
  "Search for a way from the start to dest"
  [start dest]
  (let [start-city (find-city *cities* start)
        dest-city (find-city *cities* dest)]
    (println "From" start-city "to" dest-city)
    (beam-search start-city (partial = dest-city)
                 (neighbor-fn *cities*)
                 (partial air-distance dest-city)
                 1)))

;paip.chap6=> (trip 'San-Francisco 'Boston)
;From #paip.chap6.City{:name San-Francisco, :long 122.26, :lat 37.47} to #paip.chap6.City{:name Boston, :long 71.05, :lat 42.21}
;Search:  (#paip.chap6.City{:name San-Francisco, :long 122.26, :lat 37.47})
;Search:  (#paip.chap6.City{:name Reno, :long 119.49, :lat 39.3})
;Search:  (#paip.chap6.City{:name Grand-Jct, :long 108.37, :lat 39.05})
;Search:  (#paip.chap6.City{:name Denver, :long 105.0, :lat 39.45})
;Search:  (#paip.chap6.City{:name Kansas-City, :long 94.35, :lat 39.06})
;Search:  (#paip.chap6.City{:name Indianapolis, :long 86.1, :lat 39.46})
;Search:  (#paip.chap6.City{:name Pittsburgh, :long 79.57, :lat 40.27})
;Search:  (#paip.chap6.City{:name Boston, :long 71.05, :lat 42.21})
;#paip.chap6.City{:name Boston, :long 71.05, :lat 42.21})

;paip.chap6=> (trip 'Boston 'San-Francisco)
;From #paip.chap6.City{:name Boston, :long 71.05, :lat 42.21} to #paip.chap6.City{:name San-Francisco, :long 122.26, :lat 37.47}
;Search:  (#paip.chap6.City{:name Boston, :long 71.05, :lat 42.21})
;Search:  (#paip.chap6.City{:name Pittsburgh, :long 79.57, :lat 40.27})
;Search:  (#paip.chap6.City{:name Chicago, :long 87.37, :lat 41.5})
;Search:  (#paip.chap6.City{:name Kansas-City, :long 94.35, :lat 39.06})
;Search:  (#paip.chap6.City{:name Denver, :long 105.0, :lat 39.45})
;Search:  (#paip.chap6.City{:name Flagstaff, :long 111.41, :lat 35.13})
;Search:  (#paip.chap6.City{:name Reno, :long 119.49, :lat 39.3})
;Search:  (#paip.chap6.City{:name San-Francisco, :long 122.26, :lat 37.47})
;#paip.chap6.City{:name San-Francisco, :long 122.26, :lat 37.47})

(defrecord Path [state previous cost-so-far total-cost])

(defn make-path [state & {:keys [previous cost-so-far total-cost]
                        :or {cost-so-far 0 total-cost 0}}]
  (Path. state previous cost-so-far total-cost))

(defn map-path 
  "Call fn on each state in the path, collecting results"
  [fn path]
  (if-not (nil? path)
    (cons (fn (:state path))
          (lazy-seq (map-path fn (:previous path))))))

(defmethod print-method Path
  [path w]
  (binding [*out* w]
    (print "Path:" (:total-cost path) "km:"
             (reverse (map-path :name path)))))

(defn path-saver
  [successors cost-fn cost-left-fn]
  (fn [old-path]
    (let [old-state (:state old-path)]
      (map (fn [new-state]
             (let [cost (+ (:cost-so-far old-path)
                               (cost-fn old-state new-state))]
               ;(println "New State:" new-state " cost:" cost)
               (make-path new-state
                          :previous old-path
                          :cost-so-far cost
                          :total-cost (+ cost
                                         (cost-left-fn new-state)))))
           (successors old-state)))))

(defn trip
  "Search for a way from the start to dest"
  [start dest & {:keys [beam-width] :or {beam-width 1}}]
  (let [start-city (find-city *cities* start)
        dest-city (find-city *cities* dest)]
    (println "From" start-city "to" dest-city)
    (beam-search (make-path start-city)
                 #(= dest-city (:state %))
                 (path-saver (neighbor-fn *cities*)
                             air-distance
                             (partial air-distance dest-city))
                 :total-cost
                 beam-width)))
      
; Graph Search
(declare new-states adjoin)

(defn graph-search 
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner.
  Don't try the same state twice."
  ([states goal-p successors combiner]
    (graph-search states goal-p successors combiner =))
  ([states goal-p successors combiner state=]
    (loop [states states old-states []]
      (println "Search: " states)
      (cond (empty? states) nil
            (goal-p (first states)) (first states)
            :else (let [next-states (new-states states successors
                                                state= old-states)]
                    ;(println "Next states:" next-states)
                    (recur (combiner next-states (rest states))
                           (adjoin (first states) old-states 
                                   :test state=)))))))

(defn new-states 
  "Generate successor states that have not been seen before."
  [states successors state= old-states]
  (remove #(or (some (partial state= %) states)
               (some (partial state= %) old-states))
          (successors (first states))))

(defn adjoin 
  "Add x to lst if x is not already in lst, according to test"
  [x lst & {:keys [test] :or {test =}}]
  (if (some (partial test x) lst)
    lst
    (conj lst x)))

(defn next2 [x] (list (+ x 1) (+ x 2)))

;paip.chap6=> (tree-search '(1) (partial = 6) next2 prepend)
;Search:  (1)
;Search:  (2 3)
;Search:  (3 3 4)
;Search:  (3 4 4 5)
;Search:  (4 4 5 4 5)
;Search:  (4 5 4 5 5 6)
;Search:  (5 4 5 5 6 5 6)
;Search:  (4 5 5 6 5 6 6 7)
;Search:  (5 5 6 5 6 6 7 5 6)
;Search:  (5 6 5 6 6 7 5 6 6 7)
;Search:  (6 5 6 6 7 5 6 6 7 6 7)
;6
;paip.chap6=> (graph-search '(1) (partial = 6) next2 prepend)
;Search:  (1)
;Search:  (2 3)
;Search:  (3 4)
;Search:  (4 5)
;Search:  (5 6)
;Search:  (6 7)
;6


(defn insert-with
  "Returns a lazy sequence equivalent to 
  (concat (take-while pred coll) [value] (drop-while pred coll))."
  [pred coll value]
  (lazy-seq
    (if-let [s (seq coll)]
      (if (pred (first s) value)
        (cons (first s) (insert-with pred (next s) value))
        (cons value s))
      (cons value nil))))

(deftype priority-queue [pred coll cnt]
  clojure.lang.IPersistentList
  (count [q] cnt)
  (cons [q x] (priority-queue. pred (insert-with pred coll x) (inc cnt)))
  (empty [q] (priority-queue. pred nil 0))
  (peek [q] (first coll))
  (pop [q]
    (if (empty? coll)
      (throw (IllegalStateException. "Can't pop empty queue"))
      (priority-queue. pred (next coll) (dec cnt))))
  (seq [q] (seq coll)))

(defmethod print-method priority-queue [q w]
  (.write w (print-str (or (.coll q) '()))))
    
(defn pq
  "Returns a priority queue where elements are ordered by pred
  such that (pred (first coll) (second coll)) is always true." 
  [pred] 
  (priority-queue. pred nil 0))

(defn better-path? 
  [path1 path2]
  (< (:total-cost path1) (:total-cost path2)))


;For each succesor state, want to check to see two things:
; if it is already on the set of paths to be searched...
  ; ignore for now
; if it is in the set of old paths already searched
  ; if its score is lower, remove from old, keep going
  ; else don't use it

(defmethod print-method Path
  [path w]
  (binding [*out* w]
    (print "Path cost:" (:total-cost path) ":"
             (reverse (map-path identity path)))))

(defn new-better-paths
  "Generate successor paths to states that have not been seen before,
   or if they have, have better score now"
  [paths old-paths pred]
  (loop [paths paths old-paths old-paths new-paths []]
    (if (empty? paths) 
      [new-paths old-paths]
      (let [path (first paths)]
        (if-let [old-path (old-paths (:state path))]
          (if (pred path old-path)
            (recur (rest paths)
                   (dissoc old-paths (:state path))
                   (conj new-paths path))
            (recur (rest paths) old-paths new-paths))
          (recur (rest paths)
                 old-paths
                 (conj new-paths path)))))))
  
(defn a*-search 
  "Find a path whose state satisfies goal-p.  Start with paths,
  and expand successors, exploring least cost first.
  When there are duplicate states, keep the one with the lower
  cost and discard the other."
  ([paths goal-p successors cost-fn cost-left-fn 
    & {:keys [state=] :or {state= =}}]
    (let [path-successors (path-saver successors cost-fn cost-left-fn)]
      (loop [paths (into (pq better-path?) (map make-path paths))
             old-paths {}]
        (println "Search: " paths)
        (cond (empty? paths) nil
              (goal-p (:state (first paths))) (first paths)
              :else (let [path (first paths)
                          state (:state path)
                          old-paths (assoc old-paths state path)
                          [new-paths old-paths]
                            (new-better-paths 
                              (path-successors path) 
                              old-paths better-path?)]
                      (recur (into (pop paths) new-paths)
                             old-paths)))))))

;paip.chap6=> (a*-search [1] (partial = 6) next2 (fn [x y] 1) (diff 6))
;Search:  (Path cost: 0 : (1))
;Search:  (Path cost: 4 : (1 3) Path cost: 5 : (1 2))
;Search:  (Path cost: 3 : (1 3 5) Path cost: 4 : (1 3 4) Path cost: 5 : (1 2))
;Search:  (Path cost: 3 : (1 3 5 6) Path cost: 4 : (1 3 5 7) Path cost: 4 : (1 3 4) Path cost: 5 : (1 2))
;Path cost: 3 : (1 3 5 6))
  

