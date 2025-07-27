#lang sicp
; defining a square function - an example of abstraction
(define (square x) (* x x))
; defining a sum-of-squares function - an example of combination and abstraction
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))
; conditional procedure
(define (ab x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (sum-of-squares-of-larger-numbers x y z)
  (cond ((> x y) (cond ((> y z) (sum-of-squares x y))
                       (else (sum-of-squares x y))))
        (else (cond ((> x z) (sum-of-squares x y))
                    (else (sum-of-squares y z))))))

; Newton's method of computing square root

#;(define (sqrt-iter guess x)
  (if (good-enough? guess x) guess (sqrt-iter (improve guess x) x)))
#;(define (improve guess x) (average guess (/ x guess)))
(define (average x y) (/ (+ x y) 2))
#;(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; defining a new-if statement to replace if
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

#;(sqrt-iter 1 100)

(new-if (> 2 3) 5 6)
; defining a better good-enough? function
#;(define (good-enough?? guess change-in-guess)
  (< (abs (/ change-in-guess guess)) 0.001))
; redefining the sqrt-iter function using the update good-enough? function
#;(define (sqrt-iter1 guess x)
  (if (good-enough?? guess (abs (- (average (/ x guess) guess) guess)))
      (average (/ x guess) guess)
      (sqrt-iter1 (average (/ x guess) guess) x)))
;(sqrt-iter1 1 100)
;(sqrt-iter 1.0 0.0001)
;(sqrt-iter1 1.0 0.0001)
; defining a cube root function using Newton's method
#;(define (cube-root-iter guess x)
  (if (good-enough?? guess (abs (- (/ (+ (/ x (square guess)) (* 2 guess)) 3) guess)))
      (/ (+ (/ x (square guess)) (* 2 guess)) 3)
      (cube-root-iter (/ (+ (/ x (square guess)) (* 2 guess)) 3) x)))

;(cube-root-iter 1.0 27)
; defining a square-root function where the procedures are localised rather than global.
(define (square-root x)
  (define (good-enough? guess change-in-guess) (< (/ change-in-guess guess) 0.001))
  (define (improve guess x) (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (let* ((new-guess (improve guess x))
           (change-in-guess (abs (- guess new-guess))))
    (if (good-enough? guess change-in-guess) new-guess (sqrt-iter new-guess x))))
  (sqrt-iter 1.0 x))

(define (square-root-sicp x)
  (define (good-enough? guess change-in-guess) (< (/ change-in-guess guess) 0.001))
  (define (improve guess x) (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (define new-guess (improve guess x))
    (if (good-enough? guess (abs (- guess new-guess))) new-guess (sqrt-iter new-guess x)))
  (sqrt-iter 1.0 x))

(square-root-sicp 9)
(square-root 2)

(define (factorial x)
  (cond ((= x 0) 1)
        (else (* x (factorial (- x 1))))))
(factorial 0)
(factorial 10)

(define (fact n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) (fib (- n 2))))))

(fib 5)

(define (fibo n)
  (fibo-iter 1 0 n))

(define (fibo-iter a b count)
  (if (= count 0)
      b
      (fibo-iter (+ a b) a (- count 1))))

(define (count-change amount) (cc amount 5))

(define (cc amount kinds-of-denominations)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-denominations 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-denominations 1))
                 (cc (- amount (first-denomination kinds-of-denominations)) kinds-of-denominations)))))

(define (first-denomination kind-of-denomination)
  (cond ((= kind-of-denomination 1) 1)
        ((= kind-of-denomination 2) 5)
        ((= kind-of-denomination 3) 10)
        ((= kind-of-denomination 4) 25)
        ((= kind-of-denomination 5) 50)))

(count-change 100)