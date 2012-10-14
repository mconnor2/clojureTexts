(defn square ([x] (* x x)))

(defn sum-of-squares 
    ([x y]  (+ (square x) (square y))))

(defn abs [x]
    (if (< x 0) (- x) x))

;1.1
; Evaluate:
;10
;>10
;(+ 5 3 4)
;> 12
;(- 9 1)
;> 8
;(/ 6 2)
;> 3
;(+ (* 2 4) (- 4 6))
;> 6
;(define a 3)
;'a = 3
;(define b (+ a 1))
;'b = 4
;(+ a b (* a b))
;> 19
;(= a b)
;> false
;(if (and (> b a) (< b (* a b)))
;    b
;    a)
;> 4
;(cond ((= a 4) 6)
;      ((= b 4) (+ 6 7 a))
;       (else 25))
;> 16
;(+ 2 (if (> b a) b a))
;> 6
;(* (cond ((> a b) a)
;         ((< a b) b)
;         (else -1))
;   (+ a 1))
;> 16

;1.2
; (5 + 4 + (2 - 3 - (6 + 4/5))) / (3 * (6 - 2) * (2 - 7))
; (/ (+ 5 4 (- 2 3 (+ 6 (/ 4 5)))) (* 3 (- 6 2) (- 2 7)))

;1.3
(defn sum-large-squares [a b c]
    (cond (and (< a b) (< a c)) (sum-of-squares b c)
	  (< b c) (sum-of-squares a c)
	  true (sum-of-squares a b)))

;1.4
; a + |b|: if b < 0, use a - b, else a + b

;1.5
;(define (p) (p))
;
;(defn test [x y]
;   (if (= x 0) 0 y))
;
;(test 0 (p))
;
; with applicative order, (p) is evaluated to (p), ad-infinitum, just loops
; with normal order, once fully expand to if, reduction doesn't evaluate y

(defn average [x y]
    (/ (+ x y) 2))

(defn improve [guess x]
    (average guess (/ x guess)))

(defn good-enough? [guess x]
    (< (abs (- (square guess) x)) 0.001))

(defn sqrt-iter [guess x]
    (if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x)
		   x)))

(defn sqrt [x]
    (sqrt-iter 1.0 x))

;1.6
(defn new-if [pred then-cl else-cl]
    (cond pred then-cl
	  true else-cl))

(defn sqrt-iter [guess x]
    (new-if (good-enough? guess x)
	guess
	(sqrt-iter (improve guess x)
		   x)))
;Stack overflow becuase special form allows short circuiting of applicative 
; evaluation order.

;1.7
; if numbers too small, then 0.001 is considerable portion of the sqrt
; (square (sqrt 0.0003))
; 0.001182225094736533

; And if numbers are too large, limited precision of floating point means will
; never get within 0.001

(defn close-enough? [a b]
    (< (/ (abs (- a b)) a) 0.001))

(defn sqrt-iter [guess x]
    (let [new-guess (improve guess x)]
	(if (close-enough? guess new-guess)
	    new-guess
	    (sqrt-iter new-guess x))))

;1.8
(defn improve-cube [guess x]
    (/ (+ (/ x (square guess)) guess guess) 3))

(defn cubert-iter [guess x]
    (let [new-guess (improve-cube guess x)]
	(if (close-enough? guess new-guess)
	    new-guess
	    (cubert-iter new-guess x))))

(defn cubert [x]
    (cubert-iter 1.0 x))

;1.9
; (+ 4 5)
; (inc (+ 3 5))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc (inc 5)))))
; ...
; (inc 8)
; 9
; recursive
;
; or
;
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
; iterative

;1.10
(defn A [x y]
    (cond (= y 0) 0
	  (= x 0) (* 2 y)
	  (= y 1) 2
	  true (A (- x 1)
		  (A x (- y 1)))))
; (A 1 10)
; (A 0 (A 1 9))
; (A 0 (A 0 (A 1 8)))
; ...
; (A 0 (A 0 ..x10 (A 0 1)))
; (A 0 x10 2)
; ...
; 2^10
; (A 1 x) = 2^x

; (A 2 4)
; (A 1 (A 2 3))
;      (A 1 (A 2 2))
;      	    (A 1 (A 2 1))
;      	          2
; 2^2^2^2 = 65536
; (A 2 n) = 2^(n)^2 (2 ^ 2 n times)
;
; (A 3 3)
; (A 2 (A 3 2)
;      (A 2 (A 3 1)))
;      	    2
;      (A 2 2) = 2^2 = 4 
;  2 ** 4 = (A 2 4)
; 2 ** 2 ** n-1
; 

;1.11
(defn f [n]
    (if (< n 3) 
	n
	(+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))

(defn f-iter [n a b c]
    (let [s (+ a (* 2 b) (* 3 c))]
	 (if (= n 3)
	     s
	     (f-iter (dec n) s a b))))
(defn f2 [n]	
    (if (< n 3)
	n (f-iter n 2 1 0)))

;1.12
; 1
; 1 1
; 1 2 1
; 1 3 3 1
; 1 4 6 4 1
;
; pt(r,c) = 1 if (c == 1 or c == r)
; 	    pt(r-1,c-1) + pt(r-1,c)

(defn pascal [r c]
    (if (or (= c 1) (= c r))
	1
	(+ (pascal (dec r) (dec c)) (pascal (dec r) c))))

;1.13
; fib(0) = 0
; fib(1) = 1
; fib(n) = fib(n-1) + fib(n-2)
;
; a = (1+sqrt(5))/2, b = (1 - sqrt(5))/2
; fib(0) = (a^0 - b^0) / sqrt(5) = 0
; fib(1) = (1+sqrt(5))/2 - (1 - sqrt(5))/2 = sqrt(5)/sqrt(5) = 1
; fib(n) = fib(n-1) + fib(n-2)
; 	 = ((((1+sqrt(5))^(n-1) - (1-sqrt(5))^(n-1)) / 2^(n-1))  +
; 	    (((1+sqrt(5))^(n-2) - (1-sqrt(5))^(n-2)) / 2^(n-2))) / sqrt(5)
; 	 = (a^(n-1) - b^(n-1)) + (a^(n-2) - b^(n-2))  / sqrt(5)
; 	 = ((a+1)*a^(n-2) - (b+1)*b^(n-2))
; 	 a*a = (1 + sqrt(5))/2^2 = (1 + 2sqrt(5) + 5) / 4 = 3/2 + sqrt(5)/2
; 	     = a+1
; 	 b*b = (1 - sqrt(5))/2^2 = (1 - 2sqrt(5) + 5) /4 = 3/2 - sqrt(5)/2
; 	     = b + 1

;1.14
;  ... skip steps when coins >= 4
;(cc 11 3)
;    + (cc 11 2)
;    	   + (cc 11 1)
;    	   	(cc 10 1)
;    	   	   (cc 9 1)
;    	   	     ... 
;    	   	       1
;    	     (cc 6  2)
;    	         + (cc 6 1)
;    	               (cc 5 1)
;    	                 ...
;    	                   1
;    	           (cc 1 2)
;    	               (cc 1 1)
;    	                   (cc 0 1)
;    	                       1
;    	         2
;    	   3
;      (cc 1 3)
;          (cc 1 1)
;              (cc 0 1)
;                  1
;      4
; 4 ways.   dp, nnp, n6p, 11p
; exponential growth (not calculating exact amount)

;1.15
; p called while angle > 0.1,  12.15/3^n < 0.1, n > log_3 121.5, n > 4.37 ~ 5
; space is linear, steps is logarithmic.  sine(121.5) ~ n = 7

;1.16

(defn square [x] (* x x))

(defn iter-expt [b n a]
    (cond (= n 0) a
	  (= (mod n 2) 0) 
	    (let [b2 (square b)]
		(iter-expt b2 (- (/ n 2) 1) (* a b2)))
	  true (iter-expt b (- n 1) (* a b))))

;1.17
(defn double [x] (+ x x))
(defn halve [x] (/ x 2))

(defn even? [x] (= (mod x 2) 0))

(defn mult [a b]
    (cond (= b 1) a
	  (even? b) (double (mult a (halve b)))
	  true      (+ a (mult a (- b 1)))))

;1.18
(defn iter-mult [a b c]  ; c + a*b = a'*b'
    (cond (= b 0) c
	  (even? b) (iter-mult (double a) (- (halve b) 1) (+ c (double a)))
	  true      (iter-mult a (- b 1) (+ c a))))

;1.19
; T_pq (a,b) = (bq + aq + ap, bp + aq)
; T_pq (T_pq(a,b)) = ((bp+aq)q + (bq+aq+ap)q + (bq+aq+ap)p, 
;		    -> bpq + aqq + bqq + aqq + apq + bqp + aqp + app
;		       b(2pq+qq) + a(2qq + 2qp + pp)
;		       b(2pq+qq) + a(2pq + qq) + a(pp+qq)
; 		      (bp+aq)p + (bq+aq+ap)q
; 		    -> bpp +  bqq + apq + aqq + apq
; 		       b(pp+qq) + a(2pq+qq)
; p' = pp + qq, q' = 2pq + qq

(defn fib-iter [a b p q c]
    (cond (zero? c) b
	  (even? c) (fib-iter a b 
			      (+ (* p p) (* q q))	; p'
			      (+ (* q q) (* 2 p q))	; q'
			      (/ c 2))
	  true      (fib-iter (+ (* b q) (* a q) (* a p))
			      (+ (* b q) (* a q))
			      p q (dec c))))
(defn fib [n]
    (fib-iter 1 0 0 1 n))

(defn gcd [a b]
    (if (zero? b)
	a
	(gcd b (mod a b))))

;1.20
;(gcd 206 40)
;(gcd 40 (rem 206 40))
;    (if (zero (rem 206 40))   rem 206 40 = 6  (1 rem)
;(gcd (rem 206 40) (rem 40 (rem 206 40)))
;    (if (zero (rem 40 (rem 206 40))  4 (2 rem)
;(gcd (rem 40 (rem 206 40) (rem (rem 206 40) (rem 40 (rem 206 40)))))
;    (if (zero (rem (rem 206 40) (rem 40 (rem 206 40))))) 2 (4 rem)
;(gcd (rem (rem 206 40) (rem 40 (rem 206 40))) (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))))
;    (if (zero (rem (rem 40 (rem 206 40)) (rem (rem 206 40) (rem 40 (rem 206 40)))))) 0 (7 rem)
;    (rem (rem 206 40) (rem 40 (rem 206 40))) 2 (4 rem)
;
; total 1 + 2 + 4 + 7 + 4 = 18 rem
; applicative takes 4

(defn divides? [a b]
    (= (mod b a) 0))
(defn find-divisor [n test]
    (cond (> (square test) n) n
	  (divides? test n) test
	  true (find-divisor n (inc test))))
(defn smallest-divisor [n]
    (find-divisor n 2))

(defn prime? [n]
    (= n (smallest-divisor n)))

(defn expmod [base exp m]
    (cond (zero? exp) 1
	  (even? exp) (mod (square (expmod base (/ exp 2) m))
			   m)
	  true	      (mod (* base (expmod base (- exp 1) m))
			   m)))

(defn random [n]
    (+ 1 (rand-int (- n 1))))

(defn fermat-test [n]
    (letfn [(try-it [a]
		(= (expmod a n n) a))]
	(try-it (random n))))

(defn fast-prime? [n times]
    (cond (zero? times) true
	  (fermat-test n) (fast-prime? n (- times 1))
	  true false))

;1.21
; (smallest-divisor 199) = 199
; (smallest-divisor 1999) = 1999
; (smallest-divisor 19999) = 7

;1.22
(defn runtime [] (System/nanoTime))
(defn timed-prime-test [n]
    (letfn [(report-prime [elapsedNS]
		(printf " *** %f ms\n" (/ elapsedNS 1000000.0))
		true)
	    (start-prime-test [n start]
		(if (prime? n)
		    (report-prime (- (runtime) start))
		    (do (printf "\n") false)))]
	(printf "%d " n)
	(start-prime-test n (runtime))))

(defn find-primes [start count]
    (loop [n start c count]
	(if (zero? c)
	    n
	    (recur (+ 2 n) 
		   (if (timed-prime-test n) (dec c) c)))))

;user=> (find-primes 1001 3)
;1001
;1003
;1005
;1007
;1009  *** 0 ms
;1011
;1013  *** 0 ms
;1015
;1017
;1019  *** 1 ms
;1021

;user=> (find-primes 10001 3)
;10001 
;10003 
;10005 
;10007  *** 1 ms
;10009  *** 1 ms
;10011 
;10013 
;10015 
;10017 
;10019 
;10021 
;10023 
;10025 
;10027 
;10029 
;10031 
;10033 
;10035 
;10037  *** 1 ms
;10039

;user=> (find-primes 100001 3)
;100001 
;100003  *** 2 ms
;100005 
;100007 
;100009 
;100011 
;100013 
;100015 
;100017 
;100019  *** 2 ms
;100021 
;100023 
;100025 
;100027 
;100029 
;100031 
;100033 
;100035 
;100037 
;100039 
;100041 
;100043  *** 2 ms
;100045

;user=> (find-primes 1000001 3)
;1000001 
;1000003  *** 4 ms
;1000005 
;1000007 
;1000009 
;1000011 
;1000013 
;1000015 
;1000017 
;1000019 
;1000021 
;1000023 
;1000025 
;1000027 
;1000029 
;1000031 
;1000033  *** 4 ms
;1000035 
;1000037  *** 4 ms
;1000039

;From 100,000 to 1,000,000 there is about a 2 times increase, which is 
; probably close to the expected sqrt(10) ~ 3.16

;1.23
(defn next-t [t]
    (if (= t 2) 3 (+ 2 t)))
(defn find-divisor [n test]
    (cond (> (square test) n) n
	  (divides? test n) test
	  true (find-divisor n (next-t test))))

;user=> (find-primes 100000001 3)
;100000001 
;100000003 
;100000005 
;100000007  *** 0 ms
;100000009 
;100000011 
;100000013 
;100000015 
;100000017 
;100000019 
;100000021 
;100000023 
;100000025 
;100000027 
;100000029 
;100000031 
;100000033 
;100000035 
;100000037  *** 1 ms
;100000039  *** 1 ms
;100000041

;Something going on here, it keeps going faster and faster the more I run.  Is
;there some jit magic going on?

;1.24
(defn timed-prime-test [n]
    (letfn [(report-prime [elapsedNS]
		(printf " *** %f ms\n" (/ elapsedNS 1000000.0))
		true)
	    (start-prime-test [n start]
		(if (fast-prime? n 10)
		    (report-prime (- (runtime) start))
		    (do (printf "\n") false)))]
	(printf "%d " n)
	(start-prime-test n (runtime))))

;With slow prime?
;user=> (find-primes 700000001 3)
;700000001  *** 2.018584 ms
;700000003
;700000005
;700000007
;700000009
;700000011
;700000013
;700000015
;700000017
;700000019
;700000021
;700000023
;700000025
;700000027
;700000029
;700000031  *** 1.986619 ms
;700000033
;700000035
;700000037
;700000039
;700000041
;700000043
;700000045
;700000047
;700000049
;700000051
;700000053
;700000055
;700000057
;700000059
;700000061
;700000063
;700000065
;700000067
;700000069  *** 1.997141 ms
;700000071

;With fast-prime? 10
;70000001 user=> (find-primes 7000001 3)
;ArithmeticException integer overflow  clojure.lang.Numbers.throwIntOverflow (Numbers.java:1583)
;
;Well that is a real shame.
;

;1.25 and 1.26 hardly require my input
; 1.25 deals with taking exponent of very large numbers, then remainder, 
; clearly not a good idea
;
;1.26 computes expmod twice, where each divides the space in half, so still 
;doing linear work, instead of halving repeatedly.

;1.27
(defn carmichael [n]
    (letfn [(try-it [a]
		(= (expmod a n n) a))
	    (test-n [a]
		(cond (zero? a) true
		      (try-it a) (test-n (dec a))
		      true (do (printf "failed %d\n" a) false)))]
	(test-n (dec n))))

;user=> (carmichael 23)
;true
;user=> (carmichael 561)
;true
;user=> (carmichael 562)
;failed 561
;false
;user=> (carmichael 1105)
;true
;user=> (carmichael 1107)
;failed 1105
;false
;user=> (carmichael 1101)
;failed 1099
;false
;user=> (carmichael 1729)
;true
;user=> (carmichael 2465)
;true
;user=> (carmichael 2821)
;true
;user=> (carmichael 6601)
;true
;user=> (carmichael 2323)
;failed 2321
;false

;1.28
;Miller-Rabin
; n is prime, a < n, a^(n-1) mod n = 1 (of course, a^n mod n = a)
; nontrivial square root of 1 modulo n, a != 1 or n-1, a^2 mod n = 1

(defn expmod [base exp m]
    (cond (zero? exp) 1
	  (even? exp) 
	    (let [ halfmod   (expmod base (/ exp 2) m)
		   squaremod (mod (square halfmod) m)]
		(if (and (not (or (= halfmod (dec m)) (= halfmod 1)))
			 (= 1 squaremod))
		    0 squaremod))
	  true	      (mod (* base (expmod base (- exp 1) m))
			   m)))

(defn miller-rabin-test [n]
    (letfn [(try-it [a]
		(= (expmod a (dec n) n) 1))]
	(try-it (random n))))

(defn fast-prime? [n times]
    (cond (zero? times) true
	  (miller-rabin-test n) (fast-prime? n (- times 1))
	  true false))

;user=> (fast-prime? 23 2)                                   
;true
;user=> (fast-prime? 23 4)
;true
;user=> (fast-prime? 22 4)
;false
;user=> (fast-prime? 6601 4)
;false
;user=> (fast-prime? 6601 23)
;false
;user=> (fast-prime? 1107 23)
;false
;user=> (fast-prime? 91 23)  
;false
;user=> (fast-prime? 97 23)
;true

;1.29
(defn sum [term a next b]
    (if (> a b)
	0
	(+ (term a)
	   (sum term (next a) next b))))

;Simpsons rule
; integral f a to b ~ h/3 * [y_0 + 4y_1 + 2y_2 + 4y_3 + ... + 4 y_n-1 + y_n]
; h = (b-a)/n, y_k = f(a + k*h)
(defn integral [f a b n]
    (let [h (/ (- b a) n)
	  simpson (fn [k]
		    (* (cond (or (= k 0) (= k n)) 1.0
			     (even? k) 	      	  2.0
			     true		  4.0)
		       (f (+ a (* k h)))))]
	 (* (/ h 3)
	    (sum simpson 0 inc n))))

;user=> (defn cube [x] (* x x x))
;#'user/cube
;user=> (integral cube 0 1 100)
;0.2499999999999999
;user=> (integral cube 0 1 1000)
;0.2500000000000002
;user=> (integral cube 0 1 10)  
;0.24999999999999997

;1.30
(defn sum [term a next b]
    (letfn [(iter [a result]
		(if (> a b)
		    result
		    (iter (next a) (+ (term a) result))))]
	(iter a 0)))

;1.31
(defn ident [x] x)

(defn product [term a next b]
    (letfn [(iter [a result]
		(if (> a b)
		    result
		    (iter (next a) (* (term a) result))))]
	(iter a 1)))

(defn factorial [x]
    (product ident 1 inc x))

(defn pi-approx [n]
    (letfn [(pi-term [k]
		(let [a (+ 1 (int (/ k 2)))
		      b (int (/ (inc k) 2))]
		      (/ (* 2.0 a)
			 (inc (* 2.0 b)))))]
	(* 4 (product pi-term 1 inc n))))

;user=> (pi-approx 100)                      
;3.1570301764551654
;user=> (pi-approx 1000)
;3.1431607055322552
;user=> (pi-approx 1000) 
;3.1431607055322552
;user=> (pi-approx 2000)
;3.142377365093862

;1.32
(defn accumulate [combiner null-value term a next b]
    (letfn [(iter [a result]
		(if (> a b)
		    result
		    (iter (next a) (combiner (term a) result))))]
	(iter a null-value)))

(defn product [term a next b]
    (accumulate * 1 term a next b))

(defn sum [term a next b]
    (accumulate + 0 term a next b))

(defn accumulate [combiner null-value term a next b]
    (if (> a b)
	null-value
	(combiner (term a) 
		  (accumulate combiner null-value term (next a) next b))))

;1.33
(defn filter-accumulate [pred? combiner null-value term a next b]
    (letfn [(iter [a result]
		(if (> a b)
		    result
		    (iter (next a) 
			  (if (pred? a) (combiner (term a) result)
					result))))]
	(iter a null-value)))

(defn square [x] (* x x))

(defn prime? [n] (fast-prime? n 100))

(defn sum-square-prime [a b]
    (filter-accumulate prime? + 0 square a inc b))

(defn product-relative-prime [n]
    (letfn [(relative-prime? [a]
		(= (gcd a n) 1))]
	(filter-accumulate relative-prime? * 1 ident 1 inc n)))

;1.34
;Should be an error since 2 doesn't evaluate to a lambda expression.  Evaluates to (2 2)
;user=> (defn f [g] (g 2))
;#'user/f
;user=> (f square)
;4
;user=> (f cube)
;8
;user=> (f 2)
;ClassCastException java.lang.Long cannot be cast to clojure.lang.IFn  user/f (NO_SOURCE_FILE:184)

;And we see that lambdas in clojure (fn) are the class clojure.lang.IFn

(defn close-enough? [a b]
    (< (Math/abs (- a b)) 0.001))
(defn positive? [x] (> x 0))
(defn negative? [x] (< x 0))
(defn average [a b] (/ (+ a b) 2.0))
(defn search [f neg-point pos-point]
    (let [midpoint (average neg-point pos-point)]
	(if (close-enough? neg-point pos-point)
	    midpoint
	    (let [test-value (f midpoint)]
		(cond (positive? test-value)
			(search f neg-point midpoint)
		      (negative? test-value)
		        (search f midpoint pos-point)
		      true midpoint)))))

(defn half-interval [f a b]
    (let [a-value (f a)
	  b-value (f b)]
	(cond (and (negative? a-value) (positive? b-value))
		(search f a b)
	      (and (negative? b-value) (positive? a-value))
		(search f b a)
	      true (printf "Values are not opposite sign: %f %f\n" a b))))

;user=> (half-interval (fn [x] (Math/sin x)) 2.0 4.0)
;3.14111328125

(def tolerance 0.00001)
(defn fixed-point [f first-guess]
    (letfn [(close? [v1 v2]
		(< (Math/abs (- v1 v2)) tolerance))
	    (try-point [guess]
		(let [next (f guess)]
		    (printf "%f -> %f\n" guess next)
		    (if (close? guess next)
			next
			(try-point next))))]
	(try-point first-guess)))

;1.35
;user=> (fixed-point (fn [x] (+ 1 (/ 1 x))) 1.0)
;1.6180327868852458

;1.36
;user=> (fixed-point (fn [x] (/ (Math/log 1000) (Math/log x))) 1.1)
;1.100000 -> 72.476574
;72.476574 -> 1.612732
;1.612732 -> 14.453501
;14.453501 -> 2.586267
;2.586267 -> 7.269672
;7.269672 -> 3.482238
;3.482238 -> 5.536501
;5.536501 -> 4.036406
;4.036406 -> 4.950537
;4.950537 -> 4.318707
;4.318707 -> 4.721779
;4.721779 -> 4.450341
;4.450341 -> 4.626821
;4.626821 -> 4.509361
;4.509361 -> 4.586350
;4.586350 -> 4.535373
;4.535373 -> 4.568901
;4.568901 -> 4.546751
;4.546751 -> 4.561342
;4.561342 -> 4.551712
;4.551712 -> 4.558060
;4.558060 -> 4.553872
;4.553872 -> 4.556633
;4.556633 -> 4.554812
;4.554812 -> 4.556013
;4.556013 -> 4.555221
;4.555221 -> 4.555743
;4.555743 -> 4.555399
;4.555399 -> 4.555626
;4.555626 -> 4.555476
;4.555476 -> 4.555575
;4.555575 -> 4.555510
;4.555510 -> 4.555553
;4.555553 -> 4.555524
;4.555524 -> 4.555543
;4.555543 -> 4.555531
;4.555531 -> 4.555539
;4.555538934848503

;user=> (fixed-point (fn [x] (average x (/ (Math/log 1000) (Math/log x)))) 1.1)
;1.100000 -> 36.788287
;36.788287 -> 19.352176
;19.352176 -> 10.841834
;10.841834 -> 6.870048
;6.870048 -> 5.227225
;5.227225 -> 4.701960
;4.701960 -> 4.582197
;4.582197 -> 4.560134
;4.560134 -> 4.556320
;4.556320 -> 4.555669
;4.555669 -> 4.555558
;4.555558 -> 4.555540
;4.555540 -> 4.555536
;4.555536364911781

;1.37
(defn cont-frac [n d k]
    (letfn [(cont-iter [k t]
		(if (zero? k) t
		    (cont-iter (dec k)
				(/ (n k) (+ (d k) t)))))]
	(cont-iter k 0)))

;user=> (cont-frac (fn [i] 1.0) (fn [i] 1.0) 10)
;0.6179775280898876
;user=> (/ 1 1.618)                             
;0.6180469715698392
;user=> (cont-frac (fn [i] 1.0) (fn [i] 1.0) 20)
;0.6180339850173578

;1.38
;   i = 1 2 3 4 5 6 7 8 9 10
; D_i = 1,2,1,1,4,1,1,6,1,1,8
;       (i+1) % 3 == 0 2*(i+1)/3
(defn eulerD [i]
    (if (zero? (mod (+ i 1) 3)) 
	(* 2 (/ (+ i 1) 3.0))
	1.0))

;user=> (+ 2 (cont-frac (fn [i] 1.0) eulerD 20))
;2.718281828459045
;user=> (+ 2 (cont-frac (fn [i] 1.0) eulerD 200))
;2.7182818284590455
;user=> (+ 2 (cont-frac (fn [i] 1.0) eulerD 10)) 
;2.7182817182817183

;1.39
(defn tan-cf [x k]
    (letfn [(tanD [i] (inc (* 2 (dec i))))
	    (tanN [i]
		(- (if (= i 1) x (square x))))]
	(- (cont-frac tanN tanD k))))

;user=> (Math/tan 0.5)
;0.5463024898437905
;user=> (tan-cf 0.5 2)
;0.5454545454545455
;user=> (tan-cf 0.5 3)
;0.5462962962962963
;user=> (tan-cf 0.5 10)
;0.5463024898437905

(defn average-damp [f]
    (fn [x] (average x (f x))))

(defn cube-root [x]
    (fixed-point (average-damp (fn [y] (/ x (square y)))) 1.0))

;user=> (cube-root 64)
;1.000000 -> 32.500000
;32.500000 -> 16.280296
;16.280296 -> 8.260881
;8.260881 -> 4.599359
;4.599359 -> 3.812388
;3.812388 -> 4.107882
;4.107882 -> 3.950272
;3.950272 -> 4.025807
;4.025807 -> 3.987344
;3.987344 -> 4.006388
;4.006388 -> 3.996821
;3.996821 -> 4.001593
;4.001593 -> 3.999204
;3.999204 -> 4.000398
;4.000398 -> 3.999801
;3.999801 -> 4.000100
;4.000100 -> 3.999950
;3.999950 -> 4.000025
;4.000025 -> 3.999988
;3.999988 -> 4.000006
;4.000006 -> 3.999997
;3.9999968907104337

(def dx 0.00001)
(defn deriv [g]
    (fn [x] (/ (- (g (+ x dx)) (g x))
	       dx)))

(defn newton-transform [g]
    (fn [x]
	(- x (/ (g x) ((deriv g) x)))))
(defn newtons-method [g guess]
    (fixed-point (newton-transform g) guess))

(defn cube [x] (* x x x))

(defn cubert [x]
    (newtons-method (fn [y] (- (cube y) x)) 1.0))

;1.40
(defn cubic [a b c]
    (fn [x]
	(+ (cube x) (* a (square x)) (* b x) c)))

;user=> (newtons-method (cubic 0 1 2) 1.0)
;1.000000 -> 0.000007
;0.000007 -> -2.000000
;-2.000000 -> -1.384613
;-1.384613 -> -1.082583
;-1.082583 -> -1.004779
;-1.004779 -> -1.000017
;-1.000017 -> -1.000000
;-1.000000 -> -1.000000
;-0.9999999999999993

;1.41
(defn double [g]
    (fn [x] (g (g x))))

;user=> ((double inc) 2)
;4
;user=> (((double (double double)) inc) 5)
;21

;1.42
(defn compose [f g]
    (fn [x] (f (g x))))

;user=> ((compose square inc) 6)
;49

;1.43
(defn repeated [f n]
    (if (= n 1) 
	f
	(fn [x] (f ((repeated f (dec n)) x)))))

;user=> ((repeated square 2) 5)                
;625

;And iterative
(defn repeated [f n]
    (letfn [(repeat-iter [g n]
		(if (= n 1)
		    g
		    (repeat-iter (compose f g) (dec n))))]
	(repeat-iter f n)))

;user=> ((repeated square 2) 5)                
;625

;1.44
(defn smooth [f]
    (fn [x]
	(/ (+ (f (- x dx))
	      (f x)
	      (f (+ x dx)))
	   3.0)))

;(((repeated smooth n) f) x)

;1.45
(defn log2 [x] (/ (Math/log x) (Math/log 2.0)))

(defn nth-root 
    ([x n] (nth-root x n (int (log2 n))))
    ([x n d]
     (fixed-point ((repeated average-damp d) 
			(fn [y] (/ x (Math/pow y (dec n)))))
		 1.0)))

;1.46
(defn iterative-improve [close-enough? next]
    (fn [x]
	(if (close-enough? x) 
	    x
	    ((iterative-improve close-enough? next) (next x)))))

(defn average [x y]
    (/ (+ x y) 2))

(defn sqrt [x]
    (letfn [(improve [guess]
		(average guess (/ x guess)))
	    (good-enough? [guess]
		(< (abs (- (square guess) x)) 0.001))]
	((iterative-improve good-enough? improve) x)))


(def tolerance 0.00001)
(defn fixed-point [f first-guess]
    (letfn [(close? [v1]
		(< (Math/abs (- v1 (f v1))) tolerance))]
	((iterative-improve close? f) first-guess)))

;I don't like using (f v1) in close enough... calculating it twice... otherwise
; have to change iterative improve to do close-enough? of two values, but
; thats also not quite right.

;user=> (fixed-point (fn [y] (average y (/ 81 y))) 1.0)
;9.000000000007091


