(ns paip.re
  (:require (clojure [walk :as walk]
                     [pprint :as pprint]
                     [string :as str])))

; more NFA style pattern matching, keeping track of state explicitly instead
; of relying on recursion.

; For now assume pattern is flat, except for segment patterns (?*, ?+, ??)

(declare next-state)

(defn variable?
  "Is x a variable (a symbol beginning with '?')?"
  [x]
  (and (symbol? x) (= (get (str x) 0) \?)))

(defn prefix-match
  "Given a variable value and an input sequence, find if variable matches
   start of input and return unmatched part, or nil if does not match"
  [x input]
;  (println "Prefix match" x "~" input)
  (if (symbol? x)
    (if (= x (first input)) (rest input))
    (loop [x x input input]
      (if (empty? x) input
        (if (= (first x) (first input))
          (recur (rest x) (rest input)))))))

(defn match-variable
  "Does VAR match input? Uses (or updates) and returns bindings"
  [var {:keys [input bindings] :as current}]
;  (println "Checking var " var " in " bindings " = " input)
  (if-let [binding (bindings var)]
    (if-let [rest-input (prefix-match binding input)] 
      (next-state current :input rest-input))
    (next-state current :bindings (into bindings {var (first input)}))))

(defn next-state
  "Processes current state giving the next, with default behaviour of 
   popping front off of pattern and input"
  [{:keys [pattern input bindings] :as current} & 
   {new-pattern :pattern new-input :input new-bindings :bindings}]
  (into current
    {:pattern (or new-pattern (rest pattern))
     :input (or new-input (rest input))
     :bindings (or new-bindings bindings)}))

(declare continue-matching stop-matching)

(defn pat-match
  [pattern input]
  (loop [current {:pattern pattern
                  :input input
                  :bindings {}}
         stack ()]
    (if (empty? current) false
      (let [pattern (:pattern current)
            input (:input current)]
        (println "Current pattern =" pattern "and input" input)
        (cond
          ; Reached the end of the pattern, so see if we have also reached the
          ; end of the input, thus we have a match so return the succesful
          ; bindings, else pop off the stack and keep looking
          (empty? pattern) 
            (if (empty? input) (:bindings current)
              (recur (first stack) (rest stack)))
          ; First symbol of pattern is a variable, so check if it has been
          ; bound and matches the input, or bind it to the first symbol of the
          ; input.  Note depth first regarding stack
          (variable? (first pattern))
            (recur (match-variable (first pattern) current)
                   stack)
          ; If pattern is currently a sequence operator, add two next states
          ; to stack corresponding to potential branches
          (seq? (first pattern))
            ; For now default to ?*, then generalize
            ; Which means either we keep matching variable to input,
            ; or stop matching variable and move on with input
            ; Again, greedily gobble whole input, so start by checking that
            (if (empty? input)
              (recur (stop-matching current) stack)
              (recur (continue-matching current)
                     (cons (stop-matching current) stack)))
          ; Otherwise should be symbol to symbol matching
          (= (first pattern) (first input))
            (recur (next-state current) stack)
          :default
            (recur (first stack) (rest stack)))))))

(defn continue-matching
  "Return state that corresponds to adding next symbol from input to currently
   matched variable"
  [{:keys [pattern input] :as current}]
  (into current {:matching (cons (first input)
                                 (:matching current))
                 :input (rest input)}))

(defn stop-matching
  "Given state that corresponds to a current matching (so is tracking a
  currently matched sequence), return new state with that as binding"
  [{:keys [pattern input bindings matching] :as current}]
  (let [variable (second (first pattern))]
    (next-state (dissoc current :matching)
      :bindings (into bindings {variable (reverse matching)})
      :input input)))
