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
    (if-not (empty? input)
      (cons (list before input) 
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

