#lang racket
(define (group xs)
  (define (helper view temp result)
    (cond
      [(= (length xs) 0) '() ]
      [(empty? view) (reverse (cons temp result))]
      [(or (empty? temp) (= (first view) (first temp)))
                                                        (helper (rest view) (cons (first view) temp) result)]
     
      [else (helper (rest view) (list (first view)) (cons temp result))]    
    )
  )

  (helper xs '() '())
)
