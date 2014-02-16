#lang planet neil/sicp

;; exercise solutions for the first chapter of sicp:

;; 1.1)
(+ 5 3 4) ;; => 12
(- 9 1) ;; => 8
(/ 6 2) ;; => 3
(+ (* 2 4) (- 4 6)) ;; => 6
(define a 3)
(define b (+ a 1)) ;; => b = 4
(+ a b (* a b)) ;; => 19
(= a b) ;; => #f
(if (and (> b a) (< b (* a b)))
    b
    a) ;; => 4
(cond ((= a 4) 6)
      ((= b 6) (+ 6 7 a))
      (else 25)) ;; => 25
(+ 2 (if (> b a) b a)) ;; => 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;; => 16

;; 1.2)
(/ (+ 5 4
      (- 2
         (- 3
            (+ 6 (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7))) ;; => -37/150

;; 1.3)
(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (largest-sum-of-squares x y z)
  (cond ((> x y)
         (if (> y z) (sum-of-squares x y)
             (sum-of-squares x z)))
        ((> x z)
         (if (> y z) (sum-of-squares x y)
             (sum-of-squares x z)))
        (else (sum-of-squares y z))))

;; 1.4)
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; Shows that racket does not need 'funcall' like elisp to call a
;; symbol as a function.

;; 1.5)
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
;; Racket goes into endless recursion when running (test 0 (p))
;; So it must be applicative-order -> replacing (p) with (p) until
;; only 'primitive' operations are remaining (which never happens).
;; A substitution-model interpreter should short-circuit on the
;; (= x 0) part and return 0.

;; 1.6
;;
;; Redefining `if` via higher-order functions will prevent the program
;; from completing, as it will run into endless recursion in the
;; new-ifs application.
;; This stems from the applicative-order evaluation of the arguments.

;; Why does it work with the `if` form then?

;; 1.7
;; The guess for 0.000001 is very far off:
;; (sqrt-i 0.000001)
;; => 0.031260655525445276

;; When using the new technique it is far better:
;; => 0.0010000000000000117

;; Overall it works better for small numbers

;; 1.8

(define (cube-root x y)
  (if (good-enough? x y)
      y
      (cube-root x (improve-cube-root x y))))

(define (good-enough? x y)
  (if (< (abs (- x (* y y y))) 0.000001)
      #t
      #f))

(define (improve-cube-root x y)
  (/ (+ (/ x (square y)) (* 2 y))
     3))

;; 1.9

;; (+ 4 5)
;; (inc (+ (dec 4) 5))
;; (inc (+ 3 5))
;; (inc (inc (+ (dec 3) 5)))
;; (inc (inc (+ 2 5)))
;; (inc (inc (inc (+ (dec 2) 5))))
;; (inc (inc (inc (+ 1 5))))
;; (inc (inc (inc (inc (+ (dec 1) 5)))))
;; (inc (inc (inc (inc (+ 0 5)))))
;; (inc (inc (inc (inc 5))))
;; The process is recursive as it builds up deferred operations.

;; (+ 4 5)
;; (+ (dec 4) (inc 5))
;; (+ 3 6)
;; (+ (dec 3) (inc 6))
;; (+ 2 7)
;; (+ (dec 2) (inc 7))
;; (+ 1 8)
;; (+ (dec 1) (inc 8))
;; (+ 0 9)
;; The process is iterative and can be described solely by its state.

;; 1.10
(define (Acker x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (Acker (- x 1) (Acker x (- y 1))))))
(Acker 1 10) ;; 1024
(Acker 2 4) ;; 65536
(Acker 3 3) ;; 65536
;; (A 1 10)
;; (A 0 (A 1 9))
;; (A 0 (A 0 (A 1 8)))
;; (A 0 (A 0 (A 0 (A 1 7))))
;; ...
;; 1024
(define (f n) (Acker 0 n)) ;; 2 * n
(define (g n) (Acker 1 n)) ;; n ** 2
;; (Acker 0 (Acker 1 (- n 1)))
;; (* 2 (Acker 1 (- n 1)))
;; (* 2 (Acker 0 (Acker 1 (- n 2))))
;; (* 2 (* 2 (Acker 1 (- n 2))))
;; 2 ** n
(define (h n) (Acker 2 n))
;; (A 1 (A 2 (- n 1)))
;; (A 0 (A 1 (A 2 (- n 1))) (A 1 (A 2 (- n 2))))
;; (A 0 (A 1 (A 2 (- n 1))) (A 1 (A 2 (- n 2))))
;; 2^2^2^...{n}
(define (k n) (* 5 n n))

;; 1.11
(define (t-r-f n)
  (if (< n 3) n
      (+ (t-r-f (- n 1)) (* 2 (t-r-f (- n 2))) (* 3 (t-r-f (- n 3))))))

(define (f-iter n)
  (define (f-iter-int a b c n)
    (if (< n 3) a
        (f-iter-int (+ a (* 2 b) (* 3 c)) a b (- n 1))))
  (if (< n 3)
      n
      (f-iter-int 2 1 0 n)))

;; 1.12

(define (pascal row col)
  (cond ((and (= row 0) (= col 0)) 1)
        ((= row 0) 0)
        (else (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))))

;; 1.13

;; Some checks to recall basic algebra ;)
(expt (/ 2 (/ (- 1 (sqrt 5)) 2)) 2)
(/ 1 (expt (/ (- 1 (sqrt 5)) 2) 2))
(/ 1 (/ (- 6 (* 2 (sqrt 5))) 4))
(/ 4 (- 6 (* 2 (sqrt 5))))

;; 1.14

(define (cube x) (* x x x))
(define (approx x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (approx (sine (/ angle 3.0)))))

;; (sine 12.15)
;; (p (sine (/ 12.15 3.0)))
;; (p (sine 4.05))
;; (p (p (sine 1.349)))
;; (p (p (p (sine 0.44999))))
;; (p (p (p (p (sine 0.1666666)))))
;; (p (p (p (p (p (sine 0.05555))))))

;; The procedure is applied 5 times.
;; In general we require (/ (log angle * 10) (log 3)) steps as long as
;; the precision stays at 0.1.

;; 1.16

(define (expt-fast-iter base n)
  (define (is-even? n)
    (= (remainder n 2) 0))
  (define (expt-fast-iter-i base n sum)
    (cond ((= n 0) sum)
          ((= n 1) (* base sum))
          ((is-even? n) (expt-fast-iter-i
                         (square base)
                         (/ n 2)
                         sum))
          (else (expt-fast-iter-i
                 base
                 (- n 1)
                 (* base sum)))))
  (expt-fast-iter-i base n 1))


;; 1.17

(define (mult-fast-rec a b)
  (define (is-even? n)
    (= (remainder n 2) 0))
  (define (double x)
    (+ x x))
  (define (halve x)
    (/ x 2))
  (cond ((= b 0) a)
        ((= b 1) a)
        ((is-even? b) (mult-fast-rec (double a) (halve b)))
        (else (+ a (mult-fast-rec a (- b 1))))))

;; 1.18

(define (mult-fast-iter a b)
  (define (is-even? n)
    (= (remainder n 2) 0))
  (define (double x)
    (+ x x))
  (define (halve x)
    (/ x 2))
  (define (mult-fast-iter-i a b c)
    (cond ((= b 0) c)
          ((is-even? b) (mult-fast-iter-i (double a) (halve b) c))
          (else (mult-fast-iter-i a (- b 1) (+ c a)))))
  (mult-fast-iter-i a b 0))

;; 1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square q) (square q))
                   (+ (square q) (* 2 p q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;; 1.20

;; (gcd 206 40)
;; (gcd 40 (remainder 206 40))
;; (gcd 40 6)
;; (gcd 6 (remainder 40 6))
;; (gcd 6 4)
;; (gcd 4 (remainder 6 4))
;; (gcd 4 2)
;; (gcd 2 (remainder 4 2))
;; (gcd 2 0)

;; 1.21

(define (smallest-divisor n) (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;; 1.22
(define (prime? n)
  (= n (smallest-divisor n)))
(define (timed-prime-test n)
  (newline) (display n) (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ") (display elapsed-time))
(define (search-for-primes start end)
  (cond ((> start end) #f)
        ((odd? start)
         (timed-prime-test start)
         (search-for-primes (+ 2 start) end))
        (else (search-for-primes (+ 1 start) end))))

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

;; 1.23

(define (new-find-divisor n test-divisor)
  (define (next-divisor n)
    (if (= 2 n) 3
        (+ 2 n)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

;; 1.24

(define (prime-fermat? n)
  (define (expmod base exp m)
    (cond ((= exp 0)
           1)
          ((even? exp)
           (remainder
            (square
             (expmod base (/ exp 2) m))
            m))
          (else
           (remainder
            (* base
               (expmod base (- exp 1) m))
            m))))
  (define (fermat-test n)
    (define (try-it a)
      (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
  (define (fast-prime? n times)
    (cond ((= times 0) true)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else false)))
  (fast-prime? n 100))
(define (timed-prime-test-fermat n)
  (newline) (display n) (start-prime-test-fermat n (runtime)))
(define (start-prime-test-fermat n start-time)
  (if (prime-fermat? n)
      (report-prime (- (runtime) start-time))))

(timed-prime-test-fermat 1009)
(timed-prime-test-fermat 1013)
(timed-prime-test-fermat 1019)
(timed-prime-test-fermat 10007)
(timed-prime-test-fermat 10009)
(timed-prime-test-fermat 10037)
(timed-prime-test-fermat 100003)
(timed-prime-test-fermat 100019)
(timed-prime-test-fermat 100043)
(timed-prime-test-fermat 1000003)
(timed-prime-test-fermat 1000033)
(timed-prime-test-fermat 1000037)

;; The output greatly depends on the number of times we require the
;; fermat test to run.

;; 1.25

;; In theory: it will work.
;; 
;; In practice: it will first compute the exponential which will result
;; in big arithmetic operations being performed, which will in turn be a
;; lot slower than using the special procedure.

;; 1.26

;; Calling (expmod a b m) twice will result in every computation being
;; performed twice in the even? case (for every step of the way!)
;; This will loose all benefits of the fast-expt trick in that
;; replaces the operation a^n with a single operation
;; (a ^ (/ n 2)) ^ 2

;; 1.27

(define carmichael-numbers '(561 1105 1729 2465 2821 6601))

(define (full-fermat-prime? n)
  (define (expmod base exp m)
    (cond ((= exp 0)
           1)
          ((even? exp)
           (remainder
            (square
             (expmod base (/ exp 2) m))
            m))
          (else
           (remainder
            (* base
               (expmod base (- exp 1) m))
            m))))
  (define (full-fermat-aux n a)
    (cond ((= n a) #t)
          ((= (expmod a n n) (remainder a n))
           (full-fermat-aux n (+ a 1)))
          (else #f)))
  (full-fermat-aux n 2))

(map full-fermat-prime? carmichael-numbers)

;; 1.28

(define (miller-rabin n)
  (define (miller-exp-mod base exp n)
    (cond ((= exp 0) 1)
          ((even? exp)
           (let* ((num (square (miller-exp-mod base (/ exp 2) n)))
                  (rem (remainder num n)))
             (if (and (not (= num 1))
                      (not (= num (- n 1)))
                      (= num (remainder 1 n)))
                 0
                 rem)))
           (else (remainder (* base (miller-exp-mod base (- exp 1) n)) n))))
    (define (miller-rabin-aux n a)
      (cond ((= a (- n 1))
             #t)
            ((= (miller-exp-mod a (- n 1) n) 0)
             #f)
            ((not (= (miller-exp-mod a (- n 1) n) (remainder 1 n)))
             #f)
            (else (miller-rabin-aux n (+ a 1)))))
    (miller-rabin-aux n 2))

(map miller-rabin carmichael-numbers)

;; 1.29

(define (sum a term next b)
  (if (> a b)
      0
      (+ (term a) (sum (next a) term next b))))

;; Function to test that the sum function works

(define (inc x)
  (+ x 1))

(define (easy-sum a b)
  (define (identity x) x)
  (sum a identity inc b))

(define (simpsons-rule f a b n)
  (define h (/ (- b a) n))
  (define (simpsons-f k)
    (let ((mult (cond ((= k 0) 1)
                      ((= k n) 1)
                      ((even? k) 2)
                      (else 4))))
      (* mult (f (+ a (* k h))))))
  (define (next s)
    (+ s 1))
  (* (/ h 3) (sum 0 simpsons-f next n)))

;; 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (easy-sum-iter a b)
  (define (identity x) x)
  (sum-iter identity a inc b))

;; 1.31

(define (product term a next b)
  (if (> a b) 1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial a b)
  (define (identity a) a)
  (product identity a inc b))

(define (factorial-iter a b)
  (define (identity a) a)
  (product-iter identity a inc b))

(define (approx-pi n)
  (define (fast-term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (define (term x)
    (/ (* 2 (+ (ceiling (/ x 2)) 1))
       (+ (* 2 (ceiling (/ (+ x 1) 2))) 1)))
  (product fast-term 1 inc n))

;; 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (combiner a result))))
  (iter a null-value))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(define (product-acc term a next b)
  (accumulate * 1 term a next b))

;; 1.33

(define (filtered-accumulate combiner null-value term a next b filter)
  (if (> a b) null-value
      (if (filter a)
          (combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter))
          (combiner null-value (filtered-accumulate combiner null-value term (next a) next b filter)))))

(define (sum-prime a b)
  (filtered-accumulate + 0 identity a inc b prime?))

(define (rel-prime n)
  (define (relative-prime x)
    (= (gcd x n) 1))
  (filtered-accumulate * 1 identity 1 inc n relative-prime))

;; 1.34

(define (f_34 g)
  (g 2))

;; 1.35

(define (fixed-point f guess)
  (define (good-enough? estimate previous)
    (< (abs (- estimate previous)) 0.000001))
  (let ((next (f guess)))
    (if (good-enough? next guess) next
        (fixed-point f next))))

(define (average a b)
  (/ (+ a b) 2))

(define (fixed-point-square x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(define (fixed-point-phi)
  (fixed-point (lambda (y) (+ 1 (/ 1 y))) 0.1))

;; 1.36

(define (logged-fixed-point f guess)
  (define (good-enough? estimate previous)
    (< (abs (- estimate previous)) 0.000001))
  (let ((next (f guess)))
    (display next)
    (newline)
    (if (good-enough? next guess) next
        (logged-fixed-point f next))))

(define (x-power-x y)
  (logged-fixed-point (lambda (x) (average x (/ (log y) (log x)))) 10.0))
