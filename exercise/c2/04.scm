(define (empty-stack)
    `())

(define (empty-stack? stack)
    (if (equal? stack `())
        #t
        #f))

(define (push stack val)
    (cons val stack))

(define (pop stack)
    (if (empty-stack? stack)
        #f
        (cdr stack)))

(define (top stack)
    (if (empty-stack? stack)
        #f
        (car stack)))




