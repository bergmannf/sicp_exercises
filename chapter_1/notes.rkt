(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess previous x)
  (if (good-enough-previous? guess previous)
      guess
      (sqrt-iter (improve guess x) guess x)))

(define (good-enough-previous? guess previous-guess)
  (< (abs (- guess previous-guess)) 0.0000001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- x (* guess guess)) 0.001)))

(define (sqrt-i x)
  (sqrt-iter 1.0 0.0 x))

(define (expt-iter base n)
  (define (expt-i base n expon)
    (if (= n 0) expon
        (expt-i base (- n 1) (* expon base))))
  (expt-i base n 1))
