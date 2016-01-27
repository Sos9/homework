#lang racket


(define (series a b n)
  (define (series-iter a b i)
    (if (> i n) b
        (series-iter b (+ a b) (+ i 1))))
  (series-iter a b 3))

(define (lucas n)
  (if (= n 1) 2
  (series 2 1 n)))


(define (fibonacci n)
(series 1 1 n))


(define (summed-member n)
(+ (fibonacci n) (lucas n)))


(define (nth-lucas-sum n)
  (define (sum-iter a b i j)
    (if (> i (+ n 1)) j
        (sum-iter b (+ a b) (+ i 1) (+ j b))))
  (sum-iter 2 1 3 2))


(define (nth-fibonacci-sum n)
  (define (sum-iter a b i j)
    (if (> i (+ n 1)) j
        (sum-iter b (+ a b) (+ i 1) (+ j b))))
  (sum-iter 1 1 3 1))


(define (lucas-fib-diff n)
(-(lucas n) (fibonacci n)))




(define (string-repeat str n)
  (define (helper str i res)
    (if (= (- n 1) i)
        res
        (helper str (+ i 1) (string-append str res))))
  (helper str 0 str))
