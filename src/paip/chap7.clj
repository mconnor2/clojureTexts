(ns paip.chap7
  (:require (clojure [walk :as walk]
                     [pprint :as pprint]
                     [string :as str]))
  (:use (paip [chap5 :only (rule-based-translator) :as chap5]
              [chap6 :only (pat-match) :as chap6])))


(defn mkrule ([pattern response]
  {:pattern pattern :response response})
 ([rule] 
  {:pattern (first rule) :response (second rule)}))

(defn mkexp [lhs op rhs] (list op lhs rhs))
(defn exp? [x] (list? x))
(defn exp-args [x] (rest x))
(defn exp-op [x] (first x))
(defn exp-lhs [x] (second x))
(defn exp-rhs [x] (last x))

(def ^:dynamic pat-match-abbrevs (atom {}))

(defn pat-match-abbrev 
  "Define symbol as a macro standing for a pat-match pattern."
  [symb exp]
  (swap! pat-match-abbrevs assoc symb exp))

(defn expand-pat-match-abbrev
  "Expand out all pattern matching abbreviations in pat."
  [pat]
  (cond 
    (or (nil? pat) (= () pat)) pat
    (symbol? pat) (if-let [abbrev (@pat-match-abbrevs pat)]
                   abbrev pat)
    (seq? pat) (cons (expand-pat-match-abbrev (first pat))
                     (expand-pat-match-abbrev (rest pat)))
    :else pat))

(pat-match-abbrev '?x* '(?* ?x))
(pat-match-abbrev '?y* '(?* ?y))

(def ^:dynamic *student-rules* (map #(-> % expand-pat-match-abbrev mkrule)
  '(((?x* \.)                 ?x)
    ((?x* \. ?y*)         (?x ?y))
    ((if ?x* \, then ?y*) (?x ?y))
    ((if ?x* then ?y*)    (?x ?y))
    ((if ?x* \, ?y*)      (?x ?y))
    ((?x* \, and ?y*)     (?x ?y))
    ((find ?x* and ?y*)   ((= to-find-1 ?x) (= to-find-2 ?y)))
    ((find ?x*)           (= to-find ?x))
    ((?x* equals ?y*)     (= ?x ?y))
    ((?x* same as ?y*)    (= ?x ?y))
    ((?x* = ?y*)          (= ?x ?y))
    ((?x* is equal to ?y*) (= ?x ?y))
    ((?x* is ?y*)          (= ?x ?y))
    ((?x* - ?y*)          (- ?x ?y))
    ((?x* minus ?y*)      (- ?x ?y))
    ((difference between ?x* and ?y*) (- ?y ?x))
    ((difference ?x* and ?y*) (- ?y ?x))
    ((?x* + ?y*)          (+ ?x ?y))
    ((?x* plus ?y*)       (+ ?x ?y))
    ((sum ?x* and ?y*)    (+ ?x ?y))
    ((product ?x* and ?y*)  (* ?x ?y))
    ((?x* * ?y*)          (* ?x ?y))
    ((?x* times ?y*)      (* ?x ?y))
    ((?x* / ?y*)          (/ ?x ?y))
    ((?x* per ?y*)        (/ ?x ?y))
    ((?x* divided by ?y*) (/ ?x ?y))
    ((half ?x*)           (/ ?x 2))
    ((on half ?x*)        (/ ?x 2))
    ((twice ?x*)          (* ?x 2))
    ((square ?x*)         (* ?x ?x))
    ((?x* % less than ?y*) (* ?y (/ (- 100 ?x) 100)))
    ((?x* % more than ?y*) (* ?y (/ (+ 100 ?x) 100)))
    ((?x* % ?y*) (* ?y (/ ?x 100))))))

(declare translate-to-expression translate-pair create-list-of-equations
         solve-equations solve noise-word-p make-variable)

(defn student
  "Solve certain Algebra Word Problems."
  [words]
  (->> words (remove noise-word-p)
             translate-to-expression
             create-list-of-equations
             solve-equations))

(defn translate-to-expression
  "Translate an English phrase into an equation or expression."
  [words]
  (or (rule-based-translator words *student-rules*
                             :rule-if :pattern
                             :rule-then :response
                             :matcher pat-match
                             :action (fn [bindings response]
                                       (println "Response: " response)
                                       (replace (translate-pair bindings)
                                                response)))
      (make-variable words)))

(defn map-map-vals
  "Returns map with function f applied to all vals in given map"
  [f m]
  (apply merge (map (fn [[k v]] {k (f v)}) m)))

(defn translate-pair 
  "Translate the value part of binding into an equation or expression"
  [bindings]
  (println "Bindings: " bindings)
  (map-map-vals translate-to-expression bindings))

(defn create-list-of-equations
  "Seperate out equations embedded in nested parens."
  [exp]
  (cond (nil? exp) nil
        (symbol? (first exp)) (list exp)
        :default (conj (create-list-of-equations (first exp))
                         (create-list-of-equations (rest exp)))))

(defn make-variable
  "Create a variable name based on the given list of words"
  ;; The list of words will already have noise words removed
  [words]
  (first words))

(defn noise-word-p
  "Is this a low-content word that can be safely ignored?"
  [word]
  ('#{a an the this number of $} word))

(declare print-equations solve one-unknown solve-arithmetic isolate)

(defn solve-equations
  "Print the equations and their solution"
  [equations]
  (print-equations "The equations to be solved are:" equations)
  (print-equations "The solutions is:" (solve equations)))

(defn solve
  "Solve a system of equations by constraint propogation."
  ; Try to solve or one equation, and substitute its value into
  ; the others.  If that doesn't work, return what is known.
  ([equations] (solve equations '()))
  ([equations known]
   (println "Equations =" equations "known =" known)
    (or (some (fn [equation]
                (if-let [x (one-unknown equation)]
                  (let [answer (solve-arithmetic (isolate equation x))]
                    (solve (walk/prewalk-replace 
                              {(exp-lhs answer) (exp-rhs answer)}
                              (remove #(= % equation) equations))
                           (cons answer known)))))
              equations)
        known)))

(defn isolate
  "Isolate the lone x in e on the left-hand side of e."
  [e x]
  (println "Isolating" x "from" e)
  (cond (= (exp-lhs e) x) 
          e
        (in-exp x (exp-rhs e))
          (isolate (mkexp (exp-rhs e) '= (exp-lhs e)) x)
        (in-exp x (exp-lhs (exp-lhs e)))
          (isolate (mkexp (exp-lhs (exp-lhs e)) '=
                          (mkexp (exp-rhs e)
                                 (inverse-op (exp-op (exp-lhs e)))
                                 (exp-rhs (exp-lhs e)))) x)
        (commutative? (exp-op (exp-lhs e)))
          (isolate (mkexp (exp-rhs (exp-lhs e)) '=
                          (mkexp (exp-rhs e)
                                 (inverse-op (exp-op (exp-lhs e)))
                                 (exp-lhs (exp-lhs e)))) x)
        :default ; Case V: A/f(x) = B -> f(x) = A/B
          (isolate (mkexp (exp-rhs (exp-lhs e)) '=
                          (mkexp (exp-lhs (exp-lhs e))
                                 (exp-op (exp-lhs e))
                                 (exp-rhs e))) x)))

(def ^:dynamic inverse-op 
  '{+ -
    - +
    * /
    / *
    = =})

(defn unknown? [exp] (symbol? exp))
(defn in-exp
  [x exp]
  (or (= x exp)
      (and (exp? exp)
           (or (in-exp x (exp-lhs exp)) (in-exp x (exp-rhs exp))))))

(defn no-unknown? 
  [exp]
  (cond (unknown? exp) false
        (not (exp? exp)) true
        (no-unknown? (exp-lhs exp)) (no-unknown? (exp-rhs exp))
        :default false))

(defn one-unknown
  "Returns the single unknown in exp, if there is exactly on"
  [exp]
  (cond (unknown? exp) exp
        (not (exp? exp)) false
        (no-unknown? (exp-lhs exp)) (one-unknown (exp-rhs exp))
        (no-unknown? (exp-rhs exp)) (one-unknown (exp-lhs exp))
        :default false))

(def commutative? '#{+ * =})

(defn solve-arithmetic 
  "Do the arithmetic for the right hand side"
  [equation]
  (mkexp (exp-lhs equation) '= (eval (exp-rhs equation))))

(defn binary-exp?
  [exp]
  (and (exp? exp) (= (count (exp-args exp)) 2)))

(defn prefix->infix
  [exp]
  (if (not (list? exp)) exp
      (map prefix->infix
           (if (binary-exp? exp)
             (list (exp-lhs exp) (exp-op exp) (exp-rhs exp))
             exp))))

(defn print-equations [header equations]
  (println header)
  (doall (map #(-> % prefix->infix println) equations)))
