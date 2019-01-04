(define (number->bintree node)
    (list node '() '()))

(define (current-element btree)
    (car btree))

(define (move-to-left-son btree)
    (cadr btree))

(define (move-to-right-son btree)
    (caddr btree))

(define (at-leaf? node)
    (if (and 
            (eqv? (move-to-left-son node) '()) 
            (eqv? (move-to-right-son node) '()))
        #t
        #f))

(define (insert-to-left node btree)
    (if (eqv? (move-to-left-son btree) '())
        (list (car btree) (number->bintree node) (caddr btree))
        (list 
            (car btree)
            (list node (cadr btree) '())
            (caddr btree))))

(define (insert-to-right node btree)
    (if (eqv? (move-to-right-son btree) '())
        (list (car btree) (cadr btree) (number->bintree node))
        (list
            (car btree)
            (cadr btree)
            (list node '() (caddr btree)))))

(define t1 '(13 (12 () ()) (14 () ())))








