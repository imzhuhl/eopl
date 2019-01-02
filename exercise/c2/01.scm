(define base 16)
(define bzero `())
(define (bzero? lst)
    (if (null? lst)
        #t
        #f))

(define (successor n)
    (if (bzero? n)
        `(1)
        (let ((t (+ 1 (car n))))
            (if (= t base)
                (cons 0 (successor (cdr n)))
                (cons t (cdr n))))))

(define (predecessor n)
    (cond 
        ((bzero? n) #f)
        ((equal? n `(1)) zero)
        ((= 0 (car n))
            (if (null? (cdr n))
                #f
                (cons (- base 1) (predecessor (cdr n)))))
        (else 
            (cons (- (car n) 1) (cdr n)))))

; 构造 bigit, 如 (make 17)
(define (bmake n)
    (if (zero? n)
        `()
        (successor (bmake (- n 1)))))


