#lang racket

;; tree levels
(define (mktree n l r)
  (list n l r))

(define (mkleaf x)
  (mktree x '() '()))

(define (empty-tree? tree)
  (empty? tree))

(define (height t)
  (cond
    [(empty-tree? t) 0]
    [else (+ 1 (max (height (left t)) (height (right t))))]
    ))

(define (root tree)
  (first tree))

(define (left tree)
  (first (rest tree)))

(define (right tree)
  (first (rest (rest tree))))

(define (tree-level level tree)
  (cond
    [(empty-tree? tree) '()]
    [(= level 1) (list (root tree))]
    [else (append (tree-level (- level 1) (left tree))
                  (tree-level (- level 1) (right tree)))]
                  ))

(define (tree-levels tree)
  (define (help level res)
    (cond
      [(= level 0) res]
      [else (help (- level 1)
                  (cons (tree-level level tree) res))]
                  ))
  (help (height tree) '()))

;; map tree
(define (tree-map f tree)
  (cond
    [(empty-tree? tree) '()]
    [else (mktree (f (root tree)) (tree-map f (left tree))
                  (tree-map f (right tree)))]
    ))


;; binary search tree

(define (bst-insert x tree)
  (cond
    [(empty-tree? tree) (mkleaf x)]
    [(< x (root tree)) (mktree (root tree)
                               (bst-insert x (left tree))
                               (right tree))]
    [else (mktree (root tree)
                  (left tree)
                  (bst-insert x (right tree)))]
                  ))

(define (bst-element? x tree)
  (cond
    [(empty-tree? tree) #f]
    [(= x (root tree)) #t]
    [(< x (root tree)) (bst-element? x (left tree))]
    [else (bst-element? x (right tree))]
    ))

(define (bst->list tree)
  (cond
    [(empty-tree? tree) '()]
    [else (append (bst->list (left tree)) (list (root tree)) (bst->list (right tree)))]
    ))

(define (bst? tree)
  (define (ascending l)
    (cond
      [(= (length l) 1) #t]
      [(> (first l) (second l)) #f]
      [else (ascending (rest l))]))
  (ascending (bst->list tree)
  ))
