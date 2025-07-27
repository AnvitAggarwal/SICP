#lang sicp
(define (f n)
  (if (< n 3)
      n
      (+ (+ (f (- n 1))
            (* 2 (f (- n 2))))
         (* 3 (f (- n 3))))))

(define (g n)
  (g-iter 2 1 0 n))

(define (g-iter a b c n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        ((= n 2) a)
        (else (g-iter (+ (+ a (* 2 b)) (* 3 c))
                      a
                      b
                      (- n 1)))))

(g 1)
(g 0)
(g 4)
(g 5)

