(ns paip.chap5
  (:require (clojure [walk :as walk]
                     [pprint :as pprint]
                     [string :as str])))

(defn starts-with?
  [l x]
  (and (list? l)
       (= (first l) x)))

(defn symbol-string
  "Converts a string into list of symbols, splitting words by whitespace"
  [s]
  (map symbol (str/split s #"\s+")))

(defn variable?
  "Is x a variable (a symbol beginning with '?')?"
  [x]
  (and (symbol? x) (= (get (str x) 0) \?)))

(defn segment-pattern?
  "Is this a segment matching pattern: ((?* var) . pat)"
  [pattern]
  (and (list? pattern)
       (starts-with? (first pattern) '?*)))

(defn match-variable
  "Does VAR match input? Uses (or updates) and returns bindings"
  [var input bindings]
  (if-let [binding (bindings var)]
    (and (= binding input) bindings)
    (into bindings {var input})))

(declare segment-match)

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
        (do ;(println "Matching pattern " (second (first pattern)) 
            ;         " : " input)
          (segment-match pattern input bindings))
      (and (seq? pattern) (seq? input))
        (pat-match (rest pattern) (rest input)
                   (pat-match (first pattern) (first input)
                              bindings))
      :else nil)))

(defn match-partitions
  "Returns lazy sequence of possible matches for var in pattern:
   (before match) (var and after)"
  ([var input]
    (match-partitions var input []))
  ([var input before]
    (loop [input input before before]
      (if-let [head (first input)]
        (if (= var head)
          (cons (list before input) 
              (lazy-seq (match-partitions var (rest input) (conj before head))))
          (recur (rest input) (conj before head)))))))

(defn segment-match
  "Match the segment pattern ((?* var) . pat) against input."
  [pattern input bindings]
  (let [var (second (first pattern))
        pat (rest pattern)]
    ;(println "SEGMENT-MATCH " var " in " input " rest of pattern=" pat)
    (if (empty? pat)
      (match-variable var input bindings)
      (some (fn [[bind rest-input]]
              ;(println "TESTING: " var "=" bind " and " 
              ;         pat " matches " rest-input)
              (pat-match pat rest-input
                         (match-variable var bind bindings)))
            (match-partitions (first pat) input)))))

(defn rule-pattern [rule] (first rule))
(defn rule-response [rule] (rest rule))

(declare eliza use-eliza-rules)

(def ^:dynamic *eliza-rules*
  '((((?* ?x) hello (?* ?y))
        (How do you do.  Please state your problem.))
    (((?* ?x) I want (?* ?y))
        (What would it mean if you got ?y)
        (Why do you want ?y) (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
        (Do you really think its likely that ?y) (Do you wish that ?y)
        (What do you think about ?y) (Really-- if ?y))
    (((?* ?x) no (?* ?y))
        (Why not?) (You are being a bit negative)
        (Are you saying "NO" just to be negative?))
    (((?* ?x) I was (?* ?y))
        (Were you really?) (Perhaps I already knew you were ?y)
        (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))
        (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
        (What other feelings do you have?))))

(defn quit?
  "Check if input is a list representing exit command (quit, exit, bye)"
  [input]
  (#{'exit 'stop 'bye 'quit} (first input)))

(defn switch-viewpoint
  "Change I to you and vice versa, and so on."
  [word-map]
  (walk/prewalk-replace 
    '{I you, you I, me you, am are}
     word-map))

(defn eliza
  "Respond to user input using pattern matching rules."
  []
  (loop []
    (print "ELIZA>") (flush)
    (if-let [response (use-eliza-rules (symbol-string (read-line)))]
      (do 
        (println (str/join " " (flatten response)))
        (recur)))))

(defn use-eliza-rules
  "Find some rule with which to transform the input.
   Returns nil if input is 'quit'"
  [input]
  (if-not (quit? input)
    (some (fn [rule]
            ;(println "Match: " input " => " (first rule))
            ;(println (list 'pat-match (first rule) input) " => "
            ;          (pat-match (first rule) input))
            (if-let [result (pat-match (rule-pattern rule) input)]
              (replace result (rand-nth (rule-response rule)))))
          *eliza-rules*)))
