(ns paip.chap4
  (:require (clojure [set :as set]
                     [pprint :as pprint])))

(defrecord op [action preconds add-list del-list])

(def ^:dynamic *ops* (atom ()))

(defn find-all
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence."
  [item sequence & {:keys [test] :or {test #'=}}]
  (filter #(test item %) sequence))

(declare apply-op action? appropriate? achieve achieve-all print-indent)

(defn GPS 
  "General Problem Solver: achieve all goals using *ops*."
  [state goals ops]
  (remove #(not (action? %)) (achieve-all (conj state '(start))
                                       goals ops nil)))

(defn achieve-all
  "Try to achieve each goal, then make sure they still hold."
  [state goals ops goal-stack]
  (let [final-state (reduce #(achieve %1 %2 ops goal-stack) state goals)]
      (if (set/subset? (set goals) final-state)
        final-state)))

(defn achieve
  "A goal is achieved if it already holds,
   or if there is an appropriate op for it that is applicable."
  [state goal ops goal-stack]
  ;(print-indent (count goal-stack) "Goal: " goal)
  (cond 
    (nil? state)  nil
    (contains? state goal) state
    (some #{goal} goal-stack) nil
    :else 
      (some #(apply-op state goal % ops goal-stack)
          (find-all goal ops :test #'appropriate?))))

(defn appropriate?
  "An op is appropraite to a goal if it is in its add list."
  [goal op]
  (-> op :add-list (contains? goal)))

(defn apply-op
  "Print a message and update *state* if op is applicable."
  [state goal op ops goal-stack]
  ;(print-indent (count goal-stack) "Consider: " (:action op))
  (if-let [new-state (achieve-all state (:preconds op) ops
                                  (cons goal goal-stack))]
    (do
      ;(print-indent (count goal-stack) "Action: " (:action op))
      (-> new-state
        (set/difference (:del-list op))
        (set/union (:add-list op))))))


(defn make-op [ & {:keys [action preconds add-list del-list]}]
  (op. action preconds add-list del-list))

;(defn make-op [action preconds add-list del-list]
;    (struct op action preconds add-list del-list))

(def ^:dynamic *school-ops*
  (list
  (make-op :action 'drive-son-to-school
      :preconds '(son-at-home car-works)
      :add-list #{'son-at-school}
      :del-list #{'son-at-home})
  (make-op :action 'shop-installs-battery
      :preconds '(car-needs-battery shop-knows-problem shop-has-money)
      :add-list #{'car-works})
  (make-op :action 'tell-shop-problem
      :preconds '(in-communication-with-shop)
      :add-list #{'shop-knows-problem})
  (make-op :action 'telephone-shop
      :preconds '(know-phone-number)
      :add-list #{'in-communication-with-shop})
  (make-op :action 'look-up-number
      :preconds '(have-phone-book)
      :add-list #{'know-phone-number})
  (make-op :action 'give-shop-money
      :preconds '(have-money)
      :add-list #{'shop-has-money}
      :del-list #{'have-money})))

(defn starts-with 
  [lst x]
  (and (list? lst) (= (first lst) x)))

(defn executing?
  "Is x of the form: (executing ...) ?"
  [x]
  (starts-with x 'executing))

(defn convert-op
  "Make op conform to the (EXECUTING op) convention"
  [op]
  (if-not (some executing? (:add-list op))
    (assoc op :add-list (conj (:add-list op) 
                              (list 'executing (:action op))))
    op))

(defn make-op 
  "Make a new operator that obeys the (EXECUTING op) convention"
  [& {:keys [action preconds add-list del-list]}]
  (convert-op (op. action preconds add-list del-list)))

(def ^:dynamic *school-ops* (map convert-op *school-ops*))

(defn action?
  "Is x something that is (start) or (executing ...)"
  [x]
  (or (= x '(start)) (executing? x)))

(defn print-indent
  "Print indented info"
  [indent & rest ]
  (print (apply str (repeat indent " ")))
  (apply println rest))

(def ^:dynamic *banana-ops*
  (list
  (make-op :action 'climb-on-chair
      :preconds '(chair-at-middle-room at-middle-room on-floor)
      :add-list '#{at-bananas on-chair}
      :del-list '#{at-middle-room on-floor})
  (make-op :action 'push-chair-from-door-to-middle-room
      :preconds '(chair-at-door at-door)
      :add-list '#{chair-at-middle-room at-middle-room}
      :del-list '#{chair-at-door at-door})
  (make-op :action 'walk-from-door-to-middle-room
      :preconds '(at-door on-floor)
      :add-list '#{at-middle-room}
      :del-list '#{at-door})
  (make-op :action 'grasp-bananas
      :preconds '(at-bananas empty-handed)
      :add-list '#{has-bananas}
      :del-list '#{empty-handed})
  (make-op :action 'drop-ball
      :preconds '(has-ball)
      :add-list '#{empty-handed}
      :del-list '#{has-ball})
  (make-op :action 'eat-banans
      :preconds '(has-bananas)
      :add-list '#{empty-handed not-hungry}
      :del-list '#{has-bananas hungry})))

(defn make-maze-op
  "Make an operator to  move between two places"
  [here there]
  (make-op :action (list 'move here there)
           :preconds (list ['at here])
           :add-list #{['at there]}
           :del-list #{['at here]}))

(defn make-maze-ops
  "Make maze ops in both directions"
  [pair]
  (list (make-maze-op (first pair) (second pair))
        (make-maze-op (second pair) (first pair))))

(def ^:dynamic *maze-ops* 
  (mapcat make-maze-ops 
      '((1 2) (2 3) (3 4) (4 9) (9 14) (9 8) (8 7) (7 12) (12 13)
        (12 11) (11 6) (11 16) (16 17) (17 22) (21 22) (22 23)
        (23 18) (23 24) (24 19) (19 20) (20 15) (15 10) (10 5) (20 25))))

(defn moves
 "Extract from and to of all (executing (move from to)) actions"
  [action]
  (if-let [[_ [_ from to]] action]  
    (list from to)))

(defn find-path 
  "Search maze for path from start to end"
  [start end *maze-ops*]
  (if-let [result (GPS #{['at start]} [['at end]] *maze-ops*)]
    (let [path (apply hash-map (mapcat moves result))]
      (take-while identity (iterate path start)))))

(defn but-x 
  [x lst]
  (remove #(= x %) lst))

(defn permutations
  "Find all permutations of given list"
  [lst]
  (if (empty? lst) '([])
    (mapcat #(map (partial cons %) (permutations (but-x % lst))) lst)))

;Maze generation and drawing from "Clojure Programming", chapter 3, pg 146-9
(defn maze-wall-gen
  "Returns a random maze carved out of walls;  walls is a set of
  2-item sets #{a b} where a and b are locations.
  The returned maze is a set of the remaining walls."
  [walls]
  (let [paths (reduce (fn [index [a b]]
                        (merge-with into index {a [b] b [a]}))
                    {} (map seq walls))
        start-loc (rand-nth (keys paths))]
    (loop [walls walls
           unvisited (disj (set (keys paths)) start-loc)]
      (if-let [loc (when-let [s (seq unvisited)] (rand-nth s))]
        (let [walk (iterate (comp rand-nth paths) loc)
              steps (zipmap (take-while unvisited walk) (next walk))]
          (recur (reduce disj walls (map set steps))
                 (reduce disj unvisited (keys steps))))
        walls))))

(defn grid
  [w h]
    (set (concat
      (for [i (range (dec w)) j (range h)] #{[i j] [(inc i) j]})
      (for [i (range w) j (range (dec h))] #{[i j] [i (inc j)]}))))

(defn dbl [x] (* 2 x))
(defn dblOdd [x] (dec (dbl x)))

(defn draw
  [w h maze solution]
    (doto (javax.swing.JFrame. "Maze")
      (.setContentPane
        (doto (proxy [javax.swing.JPanel] []
          (paintComponent [^java.awt.Graphics g]
            (let [g (doto ^java.awt.Graphics2D (.create g)
                      (.scale 5 5)
                      (.translate 3 3)
                      (.setStroke (java.awt.BasicStroke. 0.4)))]
              (.drawRect g -2 -2 (dbl w) (dbl h))
              (doseq [[[xa ya] [xb yb]] (map sort maze)]
                (let [[xc yc] (if (= xa xb)
                                [(dec xa) ya]
                                [xa (dec ya)])]
                  (.drawLine g (dbl xa) (dbl ya) (dbl xc) (dbl yc))))
              (.setColor g java.awt.Color/RED)
              (doseq [[[xa ya] [xb yb]] (map sort solution)]
                (.drawLine g (dblOdd xa) (dblOdd ya) 
                             (dblOdd xb) (dblOdd yb))))))
          (.setPreferredSize (java.awt.Dimension.
                               (* 10 (inc w)) (* 10 (inc h))))))
      .pack
      (.setVisible true)))

(defn form-maze-ops
  "Given the walls of a maze (specified as set of sets #{a b} where a and b are
   locations), form description of maze in terms of possible moves"
  [w h walls]
  (let [paths (remove walls (grid w h))]
    (mapcat make-maze-ops paths)))

(defn solve-maze
  [w h start end]
  (let [walls (maze-wall-gen (grid w h))
        maze-ops (form-maze-ops w h walls)
        solution (rest (map moves (GPS #{['at start]} [['at end]] maze-ops)))]
    ;(println solution)
    (draw w h walls solution)))

