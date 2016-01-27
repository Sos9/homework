#lang racket

(define (product-digits n)
  (cond
    [(zero? n) 1]
    [else (* (remainder n 10) (product-digits  (quotient n 10)))]

    ))


(define (circle? cx cy r px py)
  (>= (* r r) (+ (* (- px cx) (- px cx))
                           (* (- py cy) (- py cy))
                           )))


(define (area a b c)
  (sqrt (* (/ (+ a b c) 2) (- (/ (+ a b c) 2) a) (- (/ (+ a b c) 2) b)
           (- (/ (+ a b c) 2) c))))


(define (prime? n)
  (define (sum-divisors s i p)
    (if (> i p)
        s
        (if (= (remainder n i) 0)
            (sum-divisors (+ s i) (+ i 1) p)
            (sum-divisors s (+ i 1) p)))
  )
  (= (sum-divisors (+ n 0) 1 (quotient n 2)) (+ n 1))
)




(define (cube-sums? n)
  (define (cube a) (* a a a))
  (define (helper i)
    (define (helper1 j)
      (if (= (+ (cube i) (cube j)) n)
          #t
          (if (= j i)
              #f
              (helper1 (+ j 1))))
    )
    (if (= n i)
        #f
        (if (helper1 1)
            #t
            (helper (+ i 1))))
  )
  (helper 1)
)
