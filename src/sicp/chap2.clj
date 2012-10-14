;No real notion of pair in clojure, at least not in the car/cdr sense that I
; can see, so lets start with rational numbers as maps (Persistent hash map
; apparently)

(defn gcd [a b]
    (if (zero? b)
	a
	(gcd b (mod a b))))

(defn make-rat [n d]
    (let [c (gcd n d)]
    {:n (/ n c) :d (/ d c)}))

(defn numer [r] (:n r))
(defn denom [r] (:d r))

(defn add-rat [x y]
    (make-rat (+ (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))

(defn sub-rat [x y]
    (make-rat (- (* (numer x) (denom y))
		 (* (numer y) (denom x)))
	      (* (denom x) (denom y))))

(defn mul-rat [x y]
    (make-rat (* (numer x) (numer y))
	      (* (denom x) (denom y))))

(defn div-rat [x y]
    (make-rat (* (numer x) (denom y))
	      (* (denom x) (numer y))))

(defn equal-rat? [x y]
    (= (* (numer x) (denom y))
       (* (numer y) (denom x))))

(defn print-rat [x] 
    (printf "%d / %d\n" (numer x) (denom x)))

;user=> (def one-half (make-rat 1 2))
;#'user/one-half
;user=> one-half
;{:n 1, :d 2}
;user=> (print-rat one-half)
;1 / 2
;nil
;user=> (def one-third (make-rat 1 3))
;#'user/one-third
;user=> (print-rat (add-rat one-half one-third))
;5 / 6
;nil
;user=> (print-rat (mul-rat one-half one-third))
;1 / 6
;nil
;user=> (print-rat (add-rat one-third one-third)) 

;2.1
(defn make-rat [n d]
    (let [p (neg? (* n d)) 
	  c (gcd n d)]
    {:n ((if p - +) (Math/abs (/ n c))) 
     :d (Math/abs (/ d c))}))

;user=> (make-rat 4 6)                          
;{:n 2, :d 3}
;user=> (make-rat -4 6)
;{:n -2, :d 3}
;user=> (make-rat 4 -6)
;{:n -2, :d 3}

;2.2
(defn make-point [x y]
    {:x x :y y})

(defn x-point [p]
    (:x p))

(defn y-point [p]
    (:y p))

(defn make-line [p1 p2]
    {:start p1 :end p2})

(defn start-segment [s]
    (:start s))

(defn end-segment [s]
    (:end s))

(defn midpoint-segment [s]
    (let [start (start-segment s)
	  end   (end-segment s)]
	(make-point (/ (+ (x-point start)
			  (x-point end))
		       2.0)
		    (/ (+ (y-point start)
			  (y-point end))
		       2.0))))

(defn print-point [p]
    (printf "(%f, %f)\n" (x-point p) (y-point p)))

;user=> (def p1 (make-point 2 3))
;#'user/p1
;user=> (def p2 (make-point 5 8))
;#'user/p2
;user=> (def l (make-line p1 p2))
;#'user/l
;user=> (print-point (midpoint-segment l))
;(3.500000, 5.500000)

;2.3
(defn length [p1 p2]
    (Math/sqrt (+ (square (- (x-point p2) (x-point p1)))
		  (square (- (y-point p2) (y-point p1))))))

;Define a rectangle as four points.  No constraints on points should actually
; define a rectangle.  For side-length and top-length to work, must be in
; clockwise order (or counter clockwise actually, just needs to be in an order)
(defn make-rect [p1 p2 p3 p4]
    {:p1 p1 :p2 p2 :p3 p3 :p4 p4})

(defn side-length [r]
    (length (:p2 r) (:p1 r)))

(defn top-length [r]
    (length (:p3 r) (:p2 r)))

(defn perimeter [r]
    (+ (* 2 (side-length r)) (* 2 (top-length r))))

(defn area [r]
    (* (side-length r) (top-length r)))

;Now for second definition, just need make sure side-length and top-length
; work.  Lets assume axis alligned, for sake of brevity, so now rectangle is
; one corner plus top and side length
(defn make-rect [p1 p2 p3 p4]
    {:p p1 
     :side (length p1 p2)
     :top  (length p2 p3)})

(defn side-length [r]
    (:side r))

(defn top-length [r]
    (:top r))

;2.4
(defn cons [x y]
   (fn [m] (m x y)))

(defn car [z]
    (z (fn [p q] p)))

;(car (cons x y))
;((cons x y) (fn [p q] p))
;(fn [m] (m x y) (fn [p q] p))
;((fn [p q] p) x y)
;x

(defn cdr [z]
    (z (fn [p q] q)))

;2.5
;n= 2^a 3^b
;a = (divn n 2), b = (divn n 3)
;or a = 0s to the right (in base 2)
(defn cons [a b]
    (* (pow 2 a) (pow 3 b)))

(defn divn [n p]
    (letfn [(divn-iter [m t]  ;m*2^t = n
		(if (zero? (mod m p))
		    (divn-iter (/ m 2) (inc t))
		    t))]
	(divn-iter n 0)))

(defn car [n]
    (divn n 2))

(defn cdr [n]
    (divn n 3))

;2.6
(defn zero [] 
    (fn [f] (fn [x] x)))

(defn add-1 [n]
    (fn [f] (fn [x] (f (((n) f) x)))))

;(add-1 (zero))
;(add-1 (fn [f] (fn [x] x)))
;(fn [f] (fn [x] (f (((fn [f] (fn [x] x)) f) x))))
;(fn [f] (fn [x] (f ((fn [x] x) x))))
;(fn [f] (fn [x] (f x)))

(defn one []
    (fn [f] (fn [x] (f x))))

(defn two []
    (fn [f] (fn [x] (f (f x)))))

(defn add [a b]
    (fn [f] (fn [x] (((a) f) (((b) f) x)))))

;user=> (((two) inc) 0)                             
;2
;user=> (((one) inc) 0)
;1
;user=> (((zero) inc) 0)
;0
;user=> (((add two two) inc) 0)                     
;4
;user=> (((add two one) inc) 0)
;3
;user=> (((add two zero) inc) 0)
;2

;2.7
; Of course, with cons, car and cdr, upper-bound = cdr, lower-bound = car
(defn make-interval [a b]
    {:low a :high b})

(defn upper-bound [n]
    (:high n))

(defn lower-bound [n]
    (:low n))

(defn print-interval [i]
    (printf "[%f,%f]\n" (lower-bound i) (upper-bound i)))

;2.8
; Smallest difference between upper_x and lower_y,
; largest between upper_y and lower_x
(defn sub-interval [x y]
    (make-interval (- (lower-bound y) (upper-bound x))
		   (- (upper-bound y) (lower-bound x))))

(defn add-interval [x y]
    (make-interval (+ (lower-bound x) (lower-bound y))
		   (+ (upper-bound x) (upper-bound y))))

;2.9
; width of addition = ((xb+yb) - (xa+ya))/2 = (xb-xa)/2 + (yb-ya)/2
; width of subtraction = ((yb-xa) - (ya-xb))/2 = (yb-ya)/2 + (xb-xa)/2

;2.10
(defn mul-interval [x y]
    (let [p1 (* (lower-bound x) (lower-bound y))
	  p2 (* (lower-bound x) (upper-bound y))
	  p3 (* (upper-bound x) (lower-bound y))
	  p4 (* (upper-bound x) (upper-bound y))]
	(make-interval (min p1 p2 p3 p4)
		       (max p1 p2 p3 p4))))

(defn spans-zero? [x]
    (and (>= 0 (upper-bound x)) (<= 0 (lower-bound x))))

(defn div-interval [x y]
    (if (or (spans-zero? x) (spans-zero? y))
	(printf "Error, interval spans zero")
	(mul-interval x
		      (make-interval (/ 1.0 (upper-bound y))
				     (/ 1.0 (lower-bound y))))))

;2.11
; ++, ++ -> aa bb
; ++, -+ -> ba bb
; -+, ++ -> ab bb
; -+, -+ -> ab<?ba aa>?bb
; --, -- -> bb aa
; -+, -- -> ba aa
; --, -+ -> ab aa
; --, ++ -> ab ba
; ++, -- -> ba ab
;Don't really need to code this up...

;2.12
(defn make-center-width [c w]
    (make-interval (- c w) (+ c w)))
(defn make-center-percent [c p]
    (let [w (* p c)]
	(make-center-width c w)))
(defn center [i]
    (/ (+ (lower-bound i) (upper-bound i)) 2.0))
(defn width [i]
    (/ (- (upper-bound i) (lower-bound i)) 2.0))
(defn percent [i]
    (/ (width i) (center i)))

;2.13
;  x+-a  * y+-b = (x-a)*(y-b) - (x+a)*(y+b)
;  		= xy-ay-bx+ab - (xy+ay+bx+ab)
;  assume a,b<<x,y ~>  xy +- (ay+bx)

;2.14
(defn par1 [r1 r2]
    (div-interval (mul-interval r1 r2)
		  (add-interval r1 r2)))
(defn par2 [r1 r2]
    (let [one (make-interval 1 1)]
	(div-interval one
		      (add-interval (div-interval one r1)
				    (div-interval one r2)))))

;user=> (def A (make-center-percent 10 0.01))
;#'user/A
;user=> (def B (make-center-percent 5 0.01))
;#'user/B
;user=> (print-interval (par1 A B))              
;[3.234653,3.434680]
;nil
;user=> (print-interval (par2 A B))
;[3.300000,3.366667]
;nil

;Doing multiplication of a and b increases the width over just addition and 
; division with a zero width constant.

;2.15 
;par2 does seem to be better since it doesn't unnecessarily expand intervals
;through computing with two uncertain numbers, except for one operation.  Its
;really the number of operations that combine uncertain numbers that determines
;increase or decrease (me thinks).

;2.16
;Doesn't seem possible, but maybe I will think about it later... XXX
;

;2.17
(defn null? [l]
    (= l '()))
(defn last-pair [items]
    (letfn [(last-pair-help [items last]
		(if (null? items)
		    (list last)
		    (last-pair-help (rest items) (first items))))]
	(last-pair-help (rest items) (first items))))

;user=> (last-pair (list 23 72 149 34))                   
;(34)
;Hmmm, how does clojure test empty list?  Not really a list oriented language
; as it were.  Man, so first and rest deal with seqs, and given a seq everything
; works above, but still can't use nil?  How the hell do you get nil... rest of
; '() ?

;2.18
(defn reverse [lst]
    (letfn [(reverse-iter [lst tsl]
		(if (null? lst)
		    tsl
		    (reverse-iter (rest lst) (cons (first lst) tsl))))]
	(reverse-iter lst '())))

;Use clojure's loop/recur to actually do tail call, and no need to declare
; helper iterative function... actually slightly cleaner code, although
; a hack underneath.
(defn reverse [lst]
    (loop [l lst
	   tsl '()]
	(if (null? l)
	    tsl
	    (recur (rest l) (cons (first l) tsl)))))

;2.19
(def us-coins (list 50 25 10 5 1))
(def uk-coins (list 100 50 20 10 5 2 1 0.5))

(def no-more? null?)
(def first-denomination first)
(def except-first-denomination rest)

(defn cc [amount coin-values]
    (cond (zero? amount) 1
	  (or (< amount 0) (no-more? coin-values)) 0
	  true (+ (cc amount (except-first-denomination coin-values))
		  (cc (- amount (first-denomination coin-values))
		      coin-values))))

;user=> (cc 100 us-coins)
;292
;user=> (cc 100 uk-coins)
;104561

;user=> (cc 100 (reverse us-coins))
;292
; Seems fine...

;2.20
(defn same-parity [g & w]
    (letfn [(parity-rest [m w]
		(if (null? w) '()
		    (let [a (first w)]
			(if (= (mod a 2) m) 
			    (cons a (parity-rest m (rest w)))
			    (parity-rest m (rest w))))))]
	(if w (cons g (parity-rest (mod g 2) w)) 
	      (list g))))

;user=> (same-parity 1)                          
;(1)
;user=> (same-parity 1 2)
;(1)
;user=> (same-parity 1 2 4)
;(1)
;user=> (same-parity 1 2 4 5)
;(1 5)
;user=> (same-parity 1 2 4 5 7)
;(1 5 7)
;user=> (same-parity 2 2 4 5 7)
;(2 2 4)


;2.21
(defn square [x] (* x x))
(defn square-list [items]
    (if (null? items)
	'()
	(cons (square (first items))
	      (square-list (rest items)))))

(defn square-list2 [items]
    (map square items))

;user=> (square-list '(1 2 3 4))
;(1 4 9 16)
;user=> (square-list2 '(1 2 3 4))
;(1 4 9 16)

;2.22
;The iterative process prepends the new arguments at the front of the list,
; so the first item will be the last in the list, etc.  reverse!
;Cons creates a new list with the item at the front, followed by the rest
; attaching as a linked list, so cons with list first would put the entire list first as the first entry in list, followed by rest. ((((1) 2) 3 ) 4)

;2.23
(defn for-each [f items]
    (loop [l items]
	(if (null? l)
	    nil
	    (do
		(f (first l))
		(recur (rest l))))))

;user=> (for-each (fn [x] (printf "%d\n" x)) (list 57 321 88))
;57
;321
;88
;nil

;2.24
;(1 (2 (3 4)))
; 1
;  \ 2
;   \ \ 3  4
;    \ \ \/
;     \ \/
;      \/
; awkwardly drawn

;2.25
(def car first)
(def cdr rest)
(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(car (car '((7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;2.26
;(1 2 3 4 5 6)
;((1 2 3) 4 5 6)
;((1 2 3) (4 5 6))

;2.27
(defn deep-reverse [items]
    (loop [items items
	   tsl '()]
	(if (null? items)
	    tsl
	    (let [x (first items)]
		(recur (rest items) 
		       (cons (if (list? x)
				 (deep-reverse x)
				 x)
			     tsl))))))

;user=> (reverse x)                                                              ((3 4) (1 2))
;user=> (deep-reverse x)                            
;((4 3) (2 1))

;2.28
(defn fringe [t]
    (cond (null? t) '()
	  (list? t) (concat (fringe (first t)) (fringe (rest t)))
	  true	    (list t)))

;user=> (fringe x)
;(1 2 3 4)
;user=> (fringe (list x x))
;(1 2 3 4 1 2 3 4)

;That was the easy recursive method using concat... can I do better
(defn fringe [t]
    (letfn [(fringe-iter [t lst]
		(cond (null? t) lst
		      (list? t) (fringe-iter (first t)
					     (fringe-iter (rest t)
							  lst))
		      true 	(cons t lst)))]
	(fringe-iter t '())))

;2.29
(defn make-mobile [left right]
    (list left right))
(defn make-branch [length structure]
    (list length structure))

(defn left-branch  [m] (first m))
(defn right-branch [m] (first (rest m)))

(defn branch-length    [b] (first b))
(defn branch-structure [b] (first (rest b)))

(defn branch-weight [b]
    (let [bs (branch-structure b)]
	(if (number? bs)
	    bs
	    (total-weight bs))))

(defn total-weight [m]
    (+ (branch-weight (left-branch m))
       (branch-weight (right-branch m))))

(defn balanced? [m]
    (or (number? m)
	(let [lb (left-branch m)
	      rb (right-branch m)]
	    (and (= (* (branch-length lb) (branch-weight lb))
		    (* (branch-length rb) (branch-weight rb)))
		 (balanced? (branch-structure rb))
		 (balanced? (branch-structure lb))))))

;d) Need to change right-branch, branch-structure, and then check for list? in branch-weight and balanced?  I guess ideally those predicates should be to test if structure is a mobile or a weight, so number? would work better.  With that change, just right-branch and branch-structure (in scheme of course, (cons 3 4) doesn't work in clojure)

;2.30
(defn square [x] (* x x))

(defn square-tree [t]
    (cond (null? t) '()
	  (number? t) (square t)
	  true (cons (square-tree (first t))
		     (square-tree (rest t)))))

;user=> (square-tree '(1 (2 (3 4) 5) (6 7)))
;(1 (4 (9 16) 25) (36 49))

(defn square-tree [t]
    (map (fn [sub-tree]
	    (if (number? sub-tree)
		(square sub-tree)
		(square-tree sub-tree)))
	 t))

;2.31
(defn tree-map [f t]
    (map (fn [sub-tree]
	    (if (list? sub-tree)
		(tree-map f sub-tree)
		(f sub-tree)))
	 t))

;user=> (tree-map square '(1 (2 (3 4) 5) (6 7))) 
;(1 (4 (9 16) 25) (36 49))

;2.32
(defn subsets [s]
    (if (null? s)
	(list '())
	(let [r (subsets (rest s))
	      f (first s)]
	    (concat r (map #(cons f %1) r)))))

;user=> (subsets '(1 2 3))                                   
;(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

;2.33
(defn map [p s]
    (reduce #(concat %1 (list (p %2))) '() s))

;XXX Why can I not do this... is this a difference of cons semantics from
;    scheme to clojure thats messing me up?  distressing
; ahhh, accumulate is not reduce, but its a fold left I guess, right?
; (define (accumulate op initial sequence)
;   (if (null? sequence)
;       initial
;       (op (car sequence)
;           (accumulate op initial (cdr sequence)))))
(defn append2 [s1 s2]
    (reduce #(cons %2 %1) s2 (reverse s1)))

(defn length [s]
    (reduce (fn [x y] (inc x)) 0 s))

;2.34
(defn horner-eval [x coeff]
    (reduce (fn [termseq c] (+ c (* x termseq)))
	    0
	    (reverse coeff)))

;user=> (horner-eval 2 [1 3 0 5 0 1])
;79
; 1 + 3*2 + 5*8 + 32 = 79

;2.35
(defn count-leaves [t]
    (reduce + 0 
	      (if (number? t) (list 1)
		  (map count-leaves t))))

;2.36
(defn null? [s] (= s '()))
(defn accumulate-n [op init seqs]
    (if (null? (first seqs))
	'()
	(cons (reduce op init (map first seqs))
	      (accumulate-n op init (map rest seqs)))))

;user=> (accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
;(22 26 30)

;2.37
(defn dot-product [v w]
    (reduce + 0 (map * v w)))

(defn matrix-*-vector [m v]
    (map #(dot-product %1 v) m))

(defn transpose [mat]
    (accumulate-n conj '() (reverse mat)))

(defn matrix-*-matrix [m n]
    (let [cols (transpose n)]
	(map #(matrix-*-vector cols %1) m)))

;2.38
;Good, just to doublecheck that reduce is fold-left, accumulate fold-right
(defn fold-left [op init s]
    (loop [result init
	   cdr    s]
	(if (null? cdr)
	    result
	    (recur (op result (first cdr))
		   (rest cdr)))))

(defn fold-right [op init s]
    (if (null? s)
	init
	(op (first s)
	    (fold-right op init (rest s)))))

;user=> (fold-left / 1 [1 2 3])
;1/6
;user=> (reduce / 1 [1 2 3])
;1/6
;fold-right =   (/ 1 (/ 2 (/ 3 1))) = 3/2

;user=> (fold-left list '() '(1 2 3))
;(((() 1) 2) 3)
;fold-right = (1 (2 (3 ())))

; (op x y) = (op y x), associative or whatever

;2.39
(defn reverse [sequence]
    (fold-right (fn [x y] (conj y x))
		[] sequence))

(defn reverse [sequence]
    (fold-left (fn [x y] (cons y x))
		'() sequence))

;user=> (fold-right concat [] (map (fn [i] (map (fn [j] (list i j)) (range 1 i)))
;((2 1) (3 1) (3 2) (4 1) (4 2) (4 3) (5 1) (5 2) (5 3) (5 4) (6 1) (6 2) (6 3) (6 4) (6 5) (7 1) (7 2) (7 3) (7 4) (7 5) (7 6) (8 1) (8 2) (8 3) (8 4) (8 5) (8 6) (8 7) (9 1) (9 2) (9 3) (9 4) (9 5) (9 6) (9 7) (9 8))
;

;And looks like clojure mapcat is flatmap

;2.40
(defn unique-pairs [n]
    (mapcat (fn [i] 
		(map (fn [j] (list i j))
		     (range 1 i)))
	    (range 1 n)))

(defn make-pair-sum [pair]
    (list (first pair) (second pair) (+ (first pair) (second pair))))

(defn prime-sum? [pair]
    (prime? (+ (first pair) (second pair))))

(defn prime-sum-pairs [n]
    (map make-pair-sum
	(filter prime-sum? (unique-pairs n))))

;user=> (prime-sum-pairs 10)
;((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11) (7 4 11) (7 6 13) (8 3 11) (8 5 13) (9 2 11) (9 4 13) (9 8 17))

;2.41
(defn unique-triples [n]
    (mapcat (fn [i]
		(mapcat (fn [j]
			    (map (fn [k] (list k j i))
				 (range 1 j)))
		     (range 1 i)))
	    (range 1 n)))

(defn unique-pairs-sum [n s]
    (filter (fn [trip] (= s (reduce + 0 trip)))
	    (unique-triples n)))

;user=> (unique-pairs-sum 10 11)
;((2 4 5) (2 3 6) (1 4 6) (1 3 7) (1 2 8))

;2.42
(defn safe? [k positions]
    (let [new-p  (first positions)
	  others (rest positions)
	  diagonal (fn [p n]
		      (or (= (+ p n) new-p)
			  (= (- p n) new-p)))]
	(and (null? (filter (fn [p] (= p new-p)) others)) ;Not in same row
	     (not-any? true? (map diagonal others (range 1 k))))))

(defn adjoin-position [new-row k rest-of-queens]
    (cons new-row rest-of-queens))
(def empty-board '())
(defn queens [board-size]
    (letfn [(queen-cols [k]
	(if (zero? k)
	    (list empty-board)
	    (filter #(safe? k %)
		(mapcat
		    (fn [rest-of-queens]
			(map #(adjoin-position % k rest-of-queens)
			     (range board-size)))
		    (queen-cols (- k 1))))))]
    (queen-cols board-size)))

;user=> (queens 4)
;((2 0 3 1) (1 3 0 2))
;user=> (queens 8)
;((3 1 6 2 5 7 4 0) (4 1 3 6 2 7 5 0) (2 4 1 7 5 3 6 0) (2 5 3 1 7 4 6 0) (4 6 0 2 7 5 3 1) (3 5 7 2 0 6 4 1) (2 5 7 0 3 6 4 1) (4 2 7 3 6 0 5 1) (4 6 3 0 2 7 5 1) (3 0 4 7 5 2 6 1) (2 5 3 0 7 4 6 1) (3 6 4 2 0 5 7 1) (5 3 1 7 4 6 0 2) (5 3 6 0 7 1 4 2) (0 6 3 5 7 1 4 2) (5 7 1 3 0 6 4 2) (5 1 6 0 3 7 4 2) (3 6 0 7 4 1 5 2) (4 7 3 0 6 1 5 2) (3 7 0 4 6 1 5 2) (1 6 4 7 0 3 5 2) (0 6 4 7 1 3 5 2) (1 4 6 3 0 7 5 2) (3 1 6 4 0 7 5 2) (4 6 0 3 1 7 5 2) (5 3 0 4 7 1 6 2) (4 0 3 5 7 1 6 2) (4 1 5 0 6 3 7 2) (5 2 6 1 7 4 0 3) (1 6 2 5 7 4 0 3) (6 2 0 5 7 4 1 3) (4 0 7 5 2 6 1 3) (0 4 7 5 2 6 1 3) (2 5 7 0 4 6 1 3) (5 2 0 6 4 7 1 3) (6 4 2 0 5 7 1 3) (6 2 7 1 4 0 5 3) (4 2 0 6 1 7 5 3) (1 4 6 0 2 7 5 3) (2 5 1 4 7 0 6 3) (5 0 4 1 7 2 6 3) (7 2 0 5 1 4 6 3) (1 7 5 0 2 4 6 3) (4 6 1 5 2 0 7 3) (2 5 1 6 4 0 7 3) (5 1 6 0 2 4 7 3) (2 6 1 7 5 3 0 4) (5 2 6 1 3 7 0 4) (3 1 6 2 5 7 0 4) (6 0 2 7 5 3 1 4) (0 5 7 2 6 3 1 4) (2 7 3 6 0 5 1 4) (5 2 6 3 0 7 1 4) (6 3 1 7 5 0 2 4) (3 5 7 1 6 0 2 4) (1 5 0 6 3 7 2 4) (1 3 5 7 2 0 6 4) (2 5 7 1 3 0 6 4) (5 2 0 7 3 1 6 4) (7 3 0 2 5 1 6 4) (3 7 0 2 5 1 6 4) (1 5 7 2 0 3 6 4) (6 1 5 2 0 3 7 4) (2 5 1 6 0 3 7 4) (3 6 2 7 1 4 0 5) (3 7 4 2 0 6 1 5) (2 4 7 3 0 6 1 5) (3 1 7 4 6 0 2 5) (4 6 1 3 7 0 2 5) (6 3 1 4 7 0 2 5) (7 1 3 0 6 4 2 5) (6 1 3 0 7 4 2 5) (4 0 7 3 1 6 2 5) (3 0 4 7 1 6 2 5) (4 1 7 0 3 6 2 5) (2 6 1 7 4 0 3 5) (2 0 6 4 7 1 3 5) (7 1 4 2 0 6 3 5) (2 4 1 7 0 6 3 5) (2 4 6 0 3 1 7 5) (4 1 3 5 7 2 0 6) (5 2 4 7 0 3 1 6) (4 7 3 0 2 5 1 6) (3 1 4 7 5 0 2 6) (3 5 0 4 1 7 2 6) (5 2 0 7 4 1 3 6) (4 2 0 5 7 1 3 6) (3 1 7 5 0 2 4 6) (5 2 4 6 0 3 1 7) (5 3 6 0 2 4 1 7) (3 6 4 1 5 0 2 7) (4 6 1 5 2 0 3 7))
;user=> (filter #(= (first %) 2) (queens 8))
;((2 4 1 7 5 3 6 0) (2 5 3 1 7 4 6 0) (2 5 7 0 3 6 4 1) (2 5 3 0 7 4 6 1) (2 5 7 0 4 6 1 3) (2 5 1 4 7 0 6 3) (2 5 1 6 4 0 7 3) (2 6 1 7 5 3 0 4) (2 7 3 6 0 5 1 4) (2 5 7 1 3 0 6 4) (2 5 1 6 0 3 7 4) (2 4 7 3 0 6 1 5) (2 6 1 7 4 0 3 5) (2 0 6 4 7 1 3 5) (2 4 1 7 0 6 3 5) (2 4 6 0 3 1 7 5))
;And (2 6 1 7 4 0 3 5) is whats displayed in the book.  Apparently it works.

;2.43
; Ahh, it calls queen-cols board-size times when it should only call it once,
; so doing 8^8 more work since calls recurisvely (my guess at least)

;2.44
(defn up-split [painter n]
    (if (= n 0)
	painter
	(let [smaller (up-split painter (- n 1))]
	    (below painter (beside smaller smaller)))))

;2.45
(defn split [t1 t2]
    (fn [painter n]
	(if (= n 0)
	    painter
	    (let [smaller ((split t1 t2) painter (- n 1))]
		(t1 painter (t2 smaller smaller))))))
	       

(def right-split (split beside below))
(def up-split (split below beside))

;2.46
(defn make-vect [x y]
    {:x x :y y})
(def xcor-vect :x)
(def ycor-vect :y)

(defn add-vect [v1 v2]
    (let [x1 (xcor-vect v1)
	  y1 (ycor-vect v1)
	  x2 (xcor-vect v2)
	  y2 (ycor-vect v2)]
	(make-vect (+ x1 x2) (+ y1 y2))))

(defn add-vect [v1 v2]
    (let [{x1 :x y1 :y} v1
	  {x2 :x y2 :y} v2]
	(make-vect (+ x1 x2) (+ y1 y2))))

(defn add-vect [{x1 :x y1 :y} {x2 :x y2 :y}]
    (make-vect (+ x1 x2) (+ y1 y2)))

(defn sub-vect [v1 v2]
    (let [x1 (xcor-vect v1)
	  y1 (ycor-vect v1)
	  x2 (xcor-vect v2)
	  y2 (ycor-vect v2)]
	(make-vect (- x1 x2) (- y1 y2))))

(defn scale-vect [s v]
    (make-vect (* s (xcor-vect v))
	       (* s (ycor-vect v))))

;2.47
(def origin-frame first)
(def edge1-frame second)
(def edge2-frame third)
(defn third [l]
    (first (rest (rest l))))

;with (origin (edge1 edge2))
(def origin-frame first)
(def edge1-frame second)
(def edge2-frame nnext)

;2.48
(defn make-segment [start end]
    (list start end))
(def start-segment first)
(def end-segment second)

;2.49
(def outline
    (let [v0 (make-vect 0 0)
	  v1 (make-vect 0 1)
	  v2 (make-vect 1 1)
	  v3 (make-vect 1 0)]
	(segments->painter (list (make-segment v0 v1)
				 (make-segment v1 v2)
			         (make-segment v2 v3)
			         (make-segment v3 v0)))))


(def Xframe 
    (let [v0 (make-vect 0 0)
	  v1 (make-vect 0 1)
	  v2 (make-vect 1 1)
	  v3 (make-vect 1 0)]
	(segments->painter (list (make-segment v0 v2)
				 (make-segment v1 v3)))))

;Rest are variations of the above.

;2.50
(defn flip-horiz [painter]
    (transform-painter painter
		       (make-vect 0.0 1.0)
		       (make-vect 1.0 1.0)
		       (make-vect 0.0 0.0)))
(defn rotate180 [painter]
    (transform-painter painter
		       (make-vect 1.0 1.0)
		       (make-vect 0.0 1.0)
		       (make-vect 1.0 0.0)))
(defn rotate270 [painter]
    (transform-painter painter
		       (make-vect 0.0 1.0)
		       (make-vect 0.0 0.0)
		       (make-vect 1.0 1.0)))

;2.51
(defn below [painter1 painter2]
    (let [split-point (make-vect 0.0 0.5)
	  paint-down  (transform-painter painter1
					 (make-vect 0.0 0.0)
					 (make-vect 1.0 0.0)
					 split-point)
	  paint-up    (transform-painter painter2
					 split-point
					 (make-vect 1.0 0.5)
					 (make-vect 0.0 1.0))]
	(fn [frame]
	    (paint-left frame)
	    (paint-right frame))))

(defn below [painter1 painter2]
    (rotate90 (beside (rotate270 painter1) (rotate270 painter2))))

;2.52... don't really need to do this one.

;2.53
;(a b c)
;((george))
;((y1 y2))
;(y1 y2)
;false
;nil
;(red shoes blue socks)

;2.54
(defn equal? [a b]
    (cond (and (list? a) (list? b)) (and (equal? (first a) (first b))
				         (equal? (rest a) (rest b)))
	  (and (symbol? a) (symbol? b)) (= a b)
	  true false))

;2.55
;'abracadabra -> (quote abracadabra)
;car of that is quote, clearly

(def variable? symbol?)
(defn same-variable? [v1 v2]
    (and (variable? v1) (variable? v2)
	 (= v1 v2)))

(defn make-sum [a1 a2]
    (list '+ a1 a2))

(defn sum? [x]
    (and (list? x) (= (first x) '+)))
(def addend second)
(defn augend [s]
    (first (nnext s)))

(defn make-product [m1 m2]
    (list '* m1 m2))

(defn product? [x]
    (and (list? x) (= (first x) '*)))
(def multiplier second)
(defn multiplicand [p]
    (first (nnext p)))

(defn deriv [exp var]
    (cond (number? exp)   0
	  (variable? exp) (if (same-variable? exp var) 1 0)
	  (sum? exp)      (make-sum (deriv (addend exp) var)
				    (deriv (augend exp) var))
	  (product? exp)  (make-sum
			    (make-product (multiplier exp)
					  (deriv (multiplicand exp) var))
			    (make-product (deriv (multiplier exp) var)
					  (multiplicand exp)))
	  true            (println "unknown expression type -- DERIV" exp)))

(defn =number? [exp num]
    (and (number? exp) (== exp num)))

(defn make-sum [a1 a2]
    (cond (=number? a1 0) a2
	  (=number? a2 0) a1
	  (and (number? a1) (number? a2)) (+ a1 a2)
	  true (list '+ a1 a2)))

(defn make-product [m1 m2]
    (cond (or (=number? m1 0) (=number? m2 0)) 0
	  (=number? m1 1) m2
	  (=number? m2 1) m1
	  (and (number? m1) (number? m2)) (* m1 m2)
	  true (list '* m1 m2)))
	    
;2.56
(defn make-exponentiation [base exp]
    (cond (=number? exp 0) 1
	  (=number? exp 1) base
	  true (list '** base exp)))

(defn exponentiation? [x]
    (and (list? x) (= (first x) '**)))
(def base second)
(defn exponent [x]
    (first (nnext x)))

(defn deriv [exp var]
    (cond (number? exp)   0
	  (variable? exp) (if (same-variable? exp var) 1 0)
	  (sum? exp)      (make-sum (deriv (addend exp) var)
				    (deriv (augend exp) var))
	  (product? exp)  (make-sum
			    (make-product (multiplier exp)
					  (deriv (multiplicand exp) var))
			    (make-product (deriv (multiplier exp) var)
					  (multiplicand exp)))
	  (exponentiation? exp)
	    (make-product (exponent exp)
			  (make-product (make-exponentiation (base exp)
						(make-sum (exponent exp) -1))
					(deriv (base exp) var)))
	  true            (println "unknown expression type -- DERIV" exp)))

;user=> (deriv '(** x 2) 'x)
;(* 2 x)
;user=> (deriv '(** x 3) 'x)
;(* 3 (** x 2))

;2.57
(defn sum? [x]
    (and (seq? x) (= (first x) '+)))
(defn augend [s]
    (if-let [aug (nnext s)]
	(cons '+ aug)
	0))

(defn product? [x]
    (and (seq? x) (= (first x) '*)))
(defn multiplicand [p]
    (if-let [cand (nnext p)]
	(cons '* cand)
	1))

;user=> (deriv '(* x y 3 x) 'x)             
;(+ (* x (* y 3)) (* y 3 x))
;user=> (deriv '(* x y (+ x 3)) 'x)
;(+ (* x y) (* y (+ x 3)))

;2.58
(defn make-sum [a1 a2]
    (cond (=number? a1 0) a2
	  (=number? a2 0) a1
	  (and (number? a1) (number? a2)) (+ a1 a2)
	  true (list a1 '+ a2)))
(defn make-product [m1 m2]
    (cond (or (=number? m1 0) (=number? m2 0)) 0
	  (=number? m1 1) m2
	  (=number? m2 1) m1
	  (and (number? m1) (number? m2)) (* m1 m2)
	  true (list m1 '* m2)))
	   

; Contains a * in the infix
(defn sum? [x]
    (and (seq? x) (.contains x '+)))
;  product (+) takes precedence over * in splitting defining operator
(defn product? [x]
    (and (seq? x) (not (sum? x))
		  (.contains x '*)))

(defn addend [x]
    (let [ad (take-while (partial not= '+) x)]
	(if (product? ad) ad
			  (first ad))))
(defn augend [x]
    (let [aug (rest (drop-while (partial not= '+) x))]
	(if (or (product? aug) (sum? aug))
	    aug
	    (first aug))))

(defn multiplier [p]
    (first (take-while (partial not= '*) p)))
(defn multiplicand [p]
    (first (rest (drop-while (partial not= '*) p))))

;user=> (deriv '(x * 3 + x * x) 'x)                     
;(3 + (x + x))
;user=> (deriv '(x + 3 * (x + y + 2)) 'x)
;4

;2.59
(defn null? [x] (= x '()))

(defn element-of-set? [x set]
    (cond (null? set) false
	  (= x (first set)) true
	  true (recur x (rest set))))

(defn adjoin-set [x set]
    (if (element-of-set? x set)
	set
	(cons x set)))

(defn intersection-set [set1 set2]
    (loop [set1 set1
	   inter '()]
	(cond (or (null? set1) (null? set2))
		inter
	      (element-of-set? (first set1) set2)
		(recur (rest set1)
		       (cons (first set1) inter))
	      true 
		(recur (rest set1) inter))))

(defn union-set [set1 set2]
    (cond (null? set1) 	set2	
	  (element-of-set? (first set1) set2) 
			(recur (rest set1) set2)
	  true  	(recur (rest set1)
			       (cons (first set1) set2))))

;2.60
;element-of-set? unchanged
(def adjoin-set cons)
(def union-set append)
;intersection-set unchanged, but slow
;adjoin and union are fast, so anytime you need to do these multiple times
;with fewer checking of membership, its faster


;Ordered list set
(defn element-of-set? [x set]
    (cond (null? set) false
	  (= x (first set)) true
	  (< x (first set)) false
	  true (recur x (rest set))))
;Need to reverse... ah well
(defn intersection-set [set1 set2]
    (loop [set1 set1
	   set2 set2
	   inter '()]
	(if (or (null? set1) (null? set2))
	    inter
	    (let [x1 (first set1)
		  x2 (first set2)]
		(cond (= x1 x2)
			(recur (rest set1) (rest set2)
			       (cons x1 inter))
		      (< x1 x2) 
			(recur (rest set1) set2 inter)
		      true
			(recur set1 (rest set2) inter))))))

;2.61
(defn adjoin-set [x set]
    (cond (null? set) false
	  (= x (first set)) set
	  (< x (first set)) (cons x set)
	  true (cons (first set) (adjoin-set x (rest set)))))

;2.62
(defn union-set [set1 set2]
    (loop [set1 set1
	   set2 set2
	   inter '()]
	(if (or (null? set1) (null? set2))
	    inter
	    (let [x1 (first set1)
		  x2 (first set2)]
		(cond (= x1 x2)
			(recur (rest set1) (rest set2)
			       (cons x1 inter))
		      (< x1 x2) 
			(recur (rest set1) set2 (cons x1 inter))
		      true
			(recur set1 (rest set2) (cons x2 inter)))))))

;Tree set
(def entry first)
(def left-branch second)
(defn right-branch [tree]
    (first (rest (rest tree))))
(defn make-tree [entry left right]
    (list entry left right))

(defn element-of-set? [x set]
    (cond (null? set) false
	  (= x (entry set)) true
	  (< x (entry set)) (recur x (left-branch set))
	  (> x (entry set)) (recur x (right-branch set))))

(defn adjoin-set? [x set]
    (cond (null? set) (make-tree x '() '())
	  (= x (entry set)) set
	  (< x (entry set))
	    (make-tree (entry set)
		       (adjoin-set x (left-branch set))
		       (right-branch set))
	  (> x (entry set))
	    (make-tree (entry set)
		       (left-branch set)
		       (adjoin-set x (right-branch set)))))

;2.63
;infix -> so (1,3,5,7,9,11) for all of the trees in 2.16
; I feel like they do produce the same..., but one is tail recursive and grows linearly, while the other is what, n^2 due to append at least.

;2.64
;Partial tree works by dividing first n elements into 3 sets, (n-1)/2 left tree, 1 element root, and (n-1) / 2 right tree, and recurses on the left and right tree, thus forming a balanced tree.  Should be O(N) growth, since each step doing 2^i up to log(n) steps, which sums together to like N, or can think of it as deposit one element in each branch, and there are N elements, so fixed amount of work per element, O(N).

;2.65
;Pretty sure the answer is just make into ordered list, take union of ordered 
; lists, and convert back to tree... right?

(defn tree-intersection-set [tree1 tree2]
    (list->tree
	(intersection-set (tree->list-2 tree1)
			  (tree->list-2 tree2))))

(defn tree-union-set [tree1 tree2]
    (list->tree
	(union-set (tree->list-2 tree1)
	 	   (tree->list-2 tree2))))


;2.66
(defn lookup [given-key set-of-records]
    (cond (null? set-of-records) false
	  (= given-key (key (entry set-of-records)))
	    (entry set-of-records)
	  (< given-key (key (entry set-of-records)))
	    (recur given-key (left-branch set-of-records))
	  true 
	    (recur given-key (right-branch set-of-records))))


(defn make-leaf [symbol weight]
    (list 'leaf symbol weight))
(defn leaf? [object] (= (first object) 'leaf))
(defn symbol-leaf [x] (second x))
(defn weight-leaf [x] (first (rest (rest x))))

(defn left-branch [tree] (first tree))
(defn right-branch [tree] (second tree))
(defn symbols [tree]
    (if (leaf? tree)
	(list (symbol-leaf tree))
	(first (rest (rest tree)))))
(defn weight [tree]
    (if (leaf? tree)
	(weight-leaf tree)
	(first (rest (rest (rest tree))))))
(defn make-code-tree [left right]
    (list left
	  right
	  (concat (symbols left) (symbols right))
	  (+ (weight left) (weight right))))

(defn choose-branch [bit branch]
    (cond (== bit 0) (left-branch branch)
	  (== bit 1) (right-branch branch)
	  true       (println "bad bit -- CHOOSE-BRANCH" bit)))
(defn decode [bits tree]
    (letfn [(decode-1 [bits current-branch]
		(if (= '() bits)
		    '()
		    (let [next-branch 
			    (choose-branch (first bits) current-branch)]
			(if (leaf? next-branch)
			    (cons (symbol-leaf next-branch)
				  (decode-1 (rest bits) tree))
			    (decode-1 (rest bits) next-branch)))))]
	(decode-1 bits tree)))

;2.67
(def sample-tree
    (make-code-tree (make-leaf 'A 4)
		    (make-code-tree
			(make-leaf 'B 2)
			(make-code-tree (make-leaf 'D 1)
					(make-leaf 'C 1)))))
(def sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;user=> (decode sample-message sample-tree)              
;(A D A B B C A)

(defn adjoin-set [x set]
    (cond (= '() set) (list x)
	  (< (weight x) (weight (first set))) (cons x set)
	  true  (cons (first set)
		      (adjoin-set x (rest set)))))

(defn make-leaf-set [pairs]
    (if (= '() pairs)
	'()
	(let [pair (first pairs)]
	    (adjoin-set (make-leaf (first pair)   ;symbol
				   (second pair)) ;frequency
			(make-leaf-set (rest pairs))))))

;2.68
(defn encode-symbol [x tree]
    (if (leaf? tree)
	(if-not (= (symbol-leaf tree) x)
		(println "Improper leaf in tree" tree x)
		'())
	(let [left (left-branch tree)
	      right (right-branch tree)]
	    (if (.contains (symbols left) x)
		(cons 0 (encode-symbol x left))
		(cons 1 (encode-symbol x right))))))

(defn encode [message tree]
    (if (= '() message)
	'()
	(concat (encode-symbol (first message) tree)
		(encode (rest message) tree))))

;user=> (encode '(A D A B B C A) sample-tree)
;(0 1 1 0 0 1 0 1 0 1 1 1 0)

;2.69
(defn succesive-merge [leaves]
    (let [left (first leaves)]
	(if-let [right (second leaves)]
	    (succesive-merge (adjoin-set (make-code-tree left right)
					 (rest (rest leaves))))
	    left)))

(defn generate-huffman-tree [pairs]
    (succesive-merge (make-leaf-set pairs)))

;user=> (succesive-merge (make-leaf-set '((A 4) (B 2) (C 1) (D 1))))
;((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
;user=> (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))          
;((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
;user=> sample-tree
;((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)

;2.70
(def lyrics-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))
(def lyrics-tree (generate-huffman-tree lyrics-pairs))
;user=> lyrics-tree
;((leaf NA 16) ((leaf YIP 9) (((leaf A 2) ((leaf WAH 1) (leaf BOOM 1) (WAH BOOM) 2) (A WAH BOOM) 4) ((leaf SHA 3) ((leaf JOB 2) (leaf GET 2) (JOB GET) 4) (SHA JOB GET) 7) (A WAH BOOM SHA JOB GET) 11) (YIP A WAH BOOM SHA JOB GET) 20) (NA YIP A WAH BOOM SHA JOB GET) 36)

(encode '(GET A JOB) lyrics-tree)
;(1 1 1 1 1 1 1 0 0 1 1 1 1 0)

(encode '(SHA NA NA NA NA NA NA NA NA) lyrics-tree)
;(1 1 1 0 0 0 0 0 0 0 0 0)

(encode '(WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP) lyrics-tree)
;(1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0)

(encode '(SHA BOOM) lyrics-tree)
;(1 1 1 0 1 1 0 1 1)

;2.71
;n = 6
;a 32
;- b 16
;  - c 8
;    - d 4
;      - e 2
;        f 1
; most frequent gets 1 bit, least frequent get n-1 bits

;2.72
;O(N) for most frequent, O(N^2) for least

;2.73
;a) Each operator impliments its derivative function, which numbers can't
;b) 
(defn sum-deriv [opperands var]
    (make-sum (deriv (addend opperands) var)
	      (deriv (augend opperands) var)))

(defn product-deriv [exp var]
    (make-sum (make-product (multiplier exp)
	   		    (deriv (multiplicand exp) var))
	      (make-product (deriv (multiplier exp) var)
			    (multiplicand exp))))

;c) yeah, would work
;d) Have one table of derivatives, and need to add operator types to them.  or wahtever, have to put them differently.

;2.75
(defn make-from-mag-ang [r a]
    (letfn [(dispatch [op]
		(cond (= op 'real-part) (* r (cos a))
		      (= op 'imag-part) (* r (sin a))
		      (= op 'magnitude) r
		      (= op 'angle) a
		      true (println "Unknown op -- MAKE-FROM-MAG-ANG" op)))]
	dispatch))

;2.76
; explicit dispatch, need to add dispatching to every operator for new types,
;                    for new operations, define once, but explict for each type
; data-directed, Need to put proper combinations in the 'table', so each operator for a new type, and each type for a new operator, but all stored in a single place
; message-passing, just a single place for each type, but when new operators are added need to modify each type to handle that message.

;2.77
;(magnitude '(complex rectangular 3.4))
;(magnitude '(rectangular 3.4))
;(magnitude-rectangular whatever)

;2.79
;2.80
;Just do it lets say.  I mean, define =zero? or equ? for each.  Unless I define zero? then can use generic a-b = 0
(defn =zero? [x] (apply-generic '=zero? x))

(put '=zero? 'scheme-number
    (fn [x] (attach-tag 'scheme-number (= x 0))))
(put '=zero? 'rational
    (fn [x] (attach-tag 'rational (= (numer x) 0))))
(put '=zero? 'complex
    (fn [x] (attach-tag 'complex (and (= (real-part x) 0) 
				      (= (imag-part x) 0)))))

(defn equ? [x y] (=zero? (sub x y)))


(defn apply-generic [op & args]
    (let [type-tags (map type-tag args)]
	(if-let [proc (get op type-tags)]
	    (apply proc (map contents args))
	    (if (= (length args) 2)
		(let [type1 (first type-tags)
		      type2 (second type-tags)
		      a1 (first args)
		      a2 (second args)
		      t1->t2 (get-coercion type1 type2)
		      t2->t1 (get-coercion type2 type1)]
		    (cond t1->t2 (recur op (t1->t2 a1) a2)
			  t2->t1 (recur op a1 (t2->t1 a2))
			  true   (println "No method for these types"
					 (list op type-tags))))
		(println "No method for these types"
		         (list op type-tags))))))

;2.81
;a. Infinite loop when it keeps try exp complex complex -> exp  complex complex
;b. Well, I think it works properly, as long as there is no coercion between 
;   same type.  That only happens if op isn't defined for those types.
;c. Well, check if types are equal before proceeding to coerce.  Granted, could
;   put it earlier before get-coercion...

(defn apply-generic [op & args]
    (let [type-tags (map type-tag args)]
	(if-let [proc (get op type-tags)]
	    (apply proc (map contents args))
	    (if (= (length args) 2)
		(let [type1 (first type-tags)
		      type2 (second type-tags)
		      a1 (first args)
		      a2 (second args)
		      t1->t2 (get-coercion type1 type2)
		      t2->t1 (get-coercion type2 type1)]
		    (cond (= type1 type2)
				 (println "No method for these types"
					 (list op type-tags))
			  t1->t2 (recur op (t1->t2 a1) a2)
			  t2->t1 (recur op a1 (t2->t1 a2))
			  true   (println "No method for these types"
					 (list op type-tags))))
		(println "No method for these types"
		         (list op type-tags))))))

;2.82
;May be fun to try to trampoline this, but lets ignore that for now
(defn apply-generic [op & args]
    (let [type-tags (map type-tag args)]
	(if-let [proc (get op type-tags)]
	    (apply proc (map contents args))
	    (try-types type-tags type-tags op args))))

(defn mapzip [fs args]
    (lazy-seq
	(let [sf (seq fs)
	      sa (seq args)]
	    (when (and sf sa)
		(cons ((first sf) (first sa))
		      (mapzip (rest sf) (rest sa)))))))

(defn try-types [type-stack type-tags op args]
    (let [target-type (first type-stack)
	  next-stack  (rest type-stack)
	  type-coercs (map #(get-coercion % target-type) type-tags)]
	(cond (every? ifn? type-coercs)
		    (apply apply-generic 
			   (cons op (mapzip type-coercs type-tags)))
	      (nil? next-stack) (println "No method for these types"
					(list op type-tags))
	      true  (recur next-stack type-tags op args))))

;2.83
; XXX
;2.86
; May have to revisit these later, for some odd reason, possibly because not able to run them?, but these problems are not sticking with me. 
;

(defn install-polynomial-package []
  ;; internal procedures
  ;; representation of poly
  (defn make-poly [variable term-list]
    (cons variable term-list))
  (defn variable [p] (first p))
  (defn term-list [p] (rest p))

  ;; Section 2.3.2
  (defn (variable? x) (symbol? x))
  (defn (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
  ;; representation of terms and term lists
  (defn adjoin-term [term term-list]
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (def the-empty-termlist '())
  (defn first-term [term-list] (first term-list))
  (defn rest-terms [term-list] (rest term-list))
  (defn empty-termlist? [term-list] (null? term-list))
  (defn make-term [order coeff] (list order coeff))
  (defn order [term] (first term))
  (defn coeff [term] (second term))

  ;; add-poly and procedures used by it
  (defn add-poly [p1 p2]
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (term-list p1)
			      (term-list p2)))
	(println "Polys not in same var -- ADD-POLY"
		 (list p1 p2))))

  (defn add-terms [L1 L2]
    (cond (empty-termlist? L1) L2
	  (empty-termlist? L2) L1
	  (true
	    (let [t1 (first-term L1)
		  t2 (first-term L2)]
		(cond (> (order t1) (order t2))
			(adjoin-term t1 (add-terms (rest-terms L1) L2))
		      (< (order t1) (order t2))
		        (adjoin-term t2 (add-terms L1 (rest-terms L2)))
		      true
			(adjoin-term
			    (make-term (order t1)
				       (add (coeff t1) (coeff t2)))
			    (add-terms (rest-terms L1) (rest-terms L2))))))))

  ;; mul-poly and procedures used by it
  (defn mul-poly [p1 p2]
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(println "Polys not in same var -- MUL-POLY"
		 (list p1 p2))))

  (defn mul-terms [L1 L2]
    (if (empty-termlist? L1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))
  (defn mul-term-by-all-terms [t1 L]
    (if (empty-termlist? L)
	(the-empty-termlist)
	(let [t2 (first-term L)]
	    (adjoin-term
		(make-term (+ (order t1) (order t2))
			   (mul (coeff t1) (coeff t2)))
		(mul-term-by-all-terms t1 (rest-terms L))))))

  ;; interface to rest of the system
  (defn tag [p] (attach-tag 'polynomial p))
  
  (put 'add '(polynomial polynomial) 
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial) 
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms)))))

;2.87
;Assumes that term-list will be empty for zero polynomials
(put '=zero? 'polynomial
    (fn [x] (attach-tag 'polynomial (empty-termlist? (term-list x)))))

;2.88
;Say there was a negation...
(put 'sub '(polynomial polynomial)
    (fn [x] (attach-tag 'polynomial (add-poly p1 (neg p2)))))

(put 'neg 'polynomial
    (fn [x] (attach-tag 'polynomial (neg-poly x))))

(defn neg-poly [p1]
    (make-poly (variable p1)
	       (neg-terms (term-list p1))))
(defn neg-terms [L1]
    (map negate-term L1))
(defn negate-term [t]
    (list (neg (coeff t)) (order t)))

;2.89
; Can I actually do this and keep add-terms and mul-terms consistent...
;
