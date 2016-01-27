#lang racket

(define (string-repeat str n)
  (define (string-repeat-inside counter result)
    (if (= counter (- n 1)) (string-append result str)
        (string-repeat-inside (+ counter 1) (string-append result str))))
  (string-repeat-inside 0 "")
 )

(define (nth-beast-number n)
  (string->number (string-repeat "666" n)))






(define (reverse-int n)
  (define (rev-iter n result)
    (cond [(= n 0) result]
          [else (rev-iter (quotient n 10) (+ (* result 10) (remainder n 10)))]))
  (rev-iter n 0))

(define (palindrome? n)
  (= n (reverse-int n)))

(define (binary n)
  (string->number(number->string n 2)))

(define (occurrences a n)
  (define (occ-iter n result)
    (cond [(= n 0) result]
          [(= (remainder n 10) a) (occ-iter (quotient n 10) (+ result 1))]
          [else (occ-iter (quotient n 10) result)]))
  (occ-iter n 0))

(define (next-hack-number n)
  (define(hack? x)
    (if (and (palindrome? (binary x)) (= (remainder(occurrences 1 (binary x)) 2)1) )
        x
        (hack? (add1 x))))
  (hack? (add1 n)))

(define (p_score n)
  (define (helper x result)
    (cond
      [(palindrome? x) (add1 result)]
      [else (helper (+ x (reverse-int x)) (add1 result))]))
  (helper n 0))
