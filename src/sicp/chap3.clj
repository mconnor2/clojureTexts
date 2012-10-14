(def ^:dynamic balance 100)

(defn withdraw [amount]
    (if (>= balance amount)
	(do (set! balance (- balance amount))
	    balance)
	"Insufficient funds"))

(defn make-withdraw [start-balance]
    (let [balance (ref start-balance)]
	(fn [amount]
	    (if (>= @balance amount)
		(dosync (alter balance - amount)
			@balance)
		"Insufficient funds"))))

;No idea how to do the above with set! and Vars... docs don't make any real sense to me, and also don't apply to clojure 1.3

;3.1
(defn make-accumulator [initial]
    (let [sum (ref initial)]
	(fn [i]
	    (dosync (alter sum + i)))))

;user=> (def A (make-accumulator 5))
;#'user/A
;user=> (A 10)
;15
;user=> (A 10)
;25

;3.2
(defn make-monitored [f]
    (let [count (ref 0)]
	(fn [x]
	    (cond (= x 'how-many-calls)
		      @count
		  (= x 'reset-count)
		      (dosync (ref-set count 0))
		  true
		      (dosync (alter count + 1)
			      (f x))))))

;3.3
(defn make-account [start-balance password]
    (let [balance (ref start-balance)
	  withdraw (fn [amount]
	    (if (>= @balance amount)
		(dosync (alter balance - amount))
		"Insufficient funds"))
	  deposit  (fn [amount]
	    (dosync (alter balance + amount)))]
	(fn [pass m]
	    (if (= pass password)
		(cond (= m 'withdraw) withdraw
		      (= m 'deposit)  deposit
		      true  (println "Unknown request -- MAKE-ACCOUNT" m))
		(fn [x] (println "Incorrect password"))))))


;3.4
(defn make-account [start-balance password]
    (let [balance (ref start-balance)
	  incorrect-pass (ref 0)
	  withdraw (fn [amount]
	    (if (>= @balance amount)
		(dosync (alter balance - amount))
		"Insufficient funds"))
	  deposit  (fn [amount]
	    (dosync (alter balance + amount)))]
	(fn [pass m]
	    (if (= pass password)
		(dosync 
		    (ref-set incorrect-pass 0)
		    (cond (= m 'withdraw) withdraw
			  (= m 'deposit)  deposit
			  true  (println "Unknown request -- MAKE-ACCOUNT" m)))
		(fn [x] 
		    (if (>= @incorrect-pass 7)
			(println "Call-the-cops")
			(dosync
			    (alter incorrect-pass + 1)
			    (println "Incorrect password"))))))))

;3.5
(defn random-in-range [low high]
    (let [range (- high low)]
	(+ low (* (Math/random) range))))

(defn monte-carlo [trials experiment]
    (loop [trials-remaining trials
	   trials-passed 0.0]
	(cond (= trials-remaining 0)
	         (/ trials-passed trials)
	      (experiment)
		 (recur (- trials-remaining 1) (+ trials-passed 1))
	      true
		 (recur (- trials-remaining 1) trials-passed))))

(defn estimate-integral [P x1 x2 y1 y2 trials]
    (let [integral-test (fn []
	    (let [x (random-in-range x1 x2)
		  y (random-in-range y1 y2)]
		(P x y)))]
	(* (monte-carlo trials integral-test)
	   (- x2 x1) (- y2 y1))))

(defn estimate-pi [trials]
    (let [unit-circle (fn [x y] (<= (+ (* x x) (* y y)) 1.0))]
	(estimate-integral unit-circle -1 1 -1 1 trials)))

;ser=> (estimate-pi 1000)                            
;3.168
;user=> (estimate-pi 1000)
;3.128
;user=> (estimate-pi 1000)
;3.06
;user=> (estimate-pi 1000)
;3.1
;user=> (estimate-pi 1000)
;3.14
;user=> (estimate-pi 10000)
;3.148
;user=> (estimate-pi 10000)
;3.1656
;user=> (estimate-pi 100000)
;3.13532
;user=> (estimate-pi 1000000)
;3.1424
;user=> (estimate-pi 1000000)
;3.1424
;user=> (estimate-pi 1000000)
;3.141704
;
;Not too shabby

;3.6
(def rand
    (let [x (ref random-init)]
	(fn [command]
	    (cond (= command 'generate)
		    (do-sync (alter x rand-update))
		  (= command 'reset)
		    (fn [seed]
			(do-sync (ref-set x seed)))
		  true
		    (println "Invalid rand command " command)))))


;3.7
(defn make-account [start-balance password]
    (let [balance (ref start-balance)
	  withdraw (fn [amount]
	    (if (>= @balance amount)
		(dosync (alter balance - amount))
		"Insufficient funds"))
	  deposit  (fn [amount]
	    (dosync (alter balance + amount)))]
	(fn [pass m]
	    (if (= pass password)
		(cond (= m 'withdraw) withdraw
		      (= m 'deposit)  deposit
		      (= m 'balance)  balance
		      true  (println "Unknown request -- MAKE-ACCOUNT" m))
		(fn [x] (println "Incorrect password"))))))

(defn make-joint [account other-password password]
    (let [balance (account other-password 'balance)
	  withdraw (fn [amount]
	    (if (>= @balance amount)
		(dosync (alter balance - amount))
		"Insufficient funds"))
	  deposit  (fn [amount]
	    (dosync (alter balance + amount)))]
	(fn [pass m]
	    (if (= pass password)
		(cond (= m 'withdraw) withdraw
		      (= m 'deposit)  deposit
		      (= m 'balance)  balance
		      true  (println "Unknown request -- MAKE-ACCOUNT" m))
		(fn [x] (println "Incorrect password"))))))

;user=> (def paul-acc (make-account 100 'secret))
;#'user/paul-acc
;user=> (def peter-acc (make-joint paul-acc 'secret 'secret2))
;#'user/peter-acc
;user=> ((peter-acc 'secret2 'withdraw) 10)
;90
;user=> ((paul-acc 'secret 'withdraw) 10)  
;80
;
;Ugly, I bet I could clean that up with either dispatch on make-account...

;3.8
(def f (let [x (ref 0)]
	(fn [s] (let [i @x]
		    (dosync (ref-set x s))
		    i))))

;user=> (f 0)
;0
;user=> (f 1)
;0
;user=> (f 0)
;1
;user=> (f 1)
;0
;user=> (+ (f 0) (f 1))
;1
;user=> (f 0)          
;1
;user=> (+ (f 0) (f 1))
;0

;3.9
;Just defining parameters for new environments created for each call to 
;factorial
;
;Factorial recursive:
; E1: n = 6,  E2: n = 5, E3: n = 4, E4: n = 3, E5: n = 2, E6: n = 1
;
;Factorial Iterative:
; E1: n = 6
; E2: product = 1, counter = 1, max-count = 6
; E3: product = 1, counter = 2, max-count = 6
; E4: product = 2, counter = 3, max-count = 6
; E5: product = 6, counter = 4, max-count = 6
; E6: product = 24, counter = 5, max-count = 6
; E7: product = 120, counter = 6, max-count = 6
; E8: product = 720, counter = 7, max-count = 6

;3.10
;The only change should be one more environment with initial-amount set, otherwise an environment with balance set is created that points back to that.
;

;3.11
;acc points to procedure that was created in an environment that also included balance, withdraw, deposit and dispatch.  When depositing it just calls this procedure, setting amount appropriately.
;
;Only thing shared is top level enviornment, so the fact that make-accont exists

;3.12
; (cdr x) -> (b)
; (append! x y)
; (cdr x) -> (b c d)

;3.13
; z -> a -> b -> c +
;      ^-----------+
; (last-pair z) will recurse forever

;3.14
; I believe this reverses a list in place, right?
; v -> a -> b -> c -> d
; mystery: () <- a <- b <- c <- d  : z
; now v: a -> (), z is reverse of v

;3.15
; z1  [ ][ ]
;      V  V
;     [ ][ ] -> [b]
;     wow
;
; z2 -> [ ][ ] -> [a]->[b]
; 	 V
; 	[wow][b]

;3.16
; 3: [][] -> [][] -> [][/]
;
; 4: [ ][ ] -> [ ][ ]
;     V         ^
;    [ ][ ] ----+
;
; 7: [ ][ ]
;     V  V
;    [ ][ ]
;     V  V
;    [ ][ ]
;
;       V------+
; inf: [ ][ ] -+
;

;3.17
; I will assume I have some set representation, not-in and add
(defn count-pairs [x]
    (let [count-set (fn [x set]
	(if (not (and (pair? x) (not-in x set))
	    0
	    (+ (count-set (car x) (add x set))
	       (count-set (cdr x) (add x set))
	       1))))]
	(count-set x [])))

;3.18
;3.19
; Doing the constant memory loop counting method only
(defn loop-detect [l]
    (let [move_check (fn [s f]
			(cond (not (pair? (cdr f)))	false
			      (= (cdr f) s)		true
			      (not (pair? (cdr (cdr f)))) false
			      (= (cdr (cdr f)) s)	true
			      true	(move_check (cdr s) (cdr (cdr f)))))]
	(move_check l l)))


;3.20
; E1: x -> [ x = 1, y = 2]
; E2: z -> [ x = E1:x, y = E1:x]
; E3: etc etc
;

(def front-ref :front)
(def rear-ref :rear)
(defn front-ptr [queue] @(front-ref queue))
(defn rear-ptr  [queue] @(rear-ref queue))
(defn set-front-ptr! [queue item]
    (dosync (ref-set (front-ref queue) item)))
(defn set-rear-ptr! [queue item]
    (dosync (ref-set (rear-ref queue) item)))

(defn empty-queue? [queue] (= (front-ptr queue) '()))

(defn make-queue [] {:front (ref '()) :rear (ref '())})

(defn front-queue [queue]
    (if (empty-queue? queue)
	(println "FRONT called with an empty queue" queue)
	(first (front-ptr queue))))

;3.21
; Its printing the front pointer (which points to front of entire queue), and then back pointer, which just sees last element
; When we remove the last, just moves front past the rear, rear still pointing to nothing.

(defn print-queue [queue]
    (println (front-ptr queue)))

;3.22
; A little half and half, since I assume we have set-cdr and set-car, which don't
; really exist in non listy clojure
(defn make-queue []
    (let [front-ptr (ref '())
	  rear-ptr  (ref '())
	  empty-queue?
	    (fn [] (= '() @front-ptr))
	  front-queue
	    (fn []
		(if (empty-queue?)
		    (println "FRONT called with an empty queue"))
		    (car @front-ptr))
	  insert-queue
	    (fn [item]
		(let [new-pair (cons item '())]
		    (cond (empty-queue?)
			    (do 
				(set-front-ptr! new-pair)
				(set-back-ptr! new-pair))
			  true
			    (do
				(set-cdr! @rear-ptr new-pair)
				(set-rear-ptr! new-pair)))))
	  delete-queue
	    (fn []
		(if (empty-queue?)	
		    (println "DELETE called with an empty queue")
		    (set-front-ptr! (cdr @front-ptr))))]
	  (fn [m]
	    (cond (= m 'front-queue) front-queue
		  (= m 'insert-queue) insert-queue
		  (= m 'delete-queue) delete-queue))))

;Of course the clojure way to do this would be to use internal persistent structures, such as vector lets say, but we'll let that slide.


