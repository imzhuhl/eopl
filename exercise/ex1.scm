; 1.15
(define (duple n x)
    (if (= n 0)
        `()
        (cons x (duple (- n 1) x))))

; 1.16
(define (invert lst)
    (if (null? lst)
        `()
        (cons
            (invert-a (car lst))
            (invert (cdr lst)))))
(define (invert-a ls)
    (list (cadr ls) (car ls)))

; 1.17
(define (down lst)
    (if (null? lst)
        `()
        (cons 
            (cons (car lst) `())
            (down (cdr lst)))))

; 1.18
(define (swapper s1 s2 slist)
    (if (null? slist)
        `()
        (cons
            (swapper-a s1 s2 (car slist))
            (swapper s1 s2 (cdr slist)))))
(define (swapper-exp s1 s2 sl)
    (if (symbol? sl)
        (cond ((eqv? s1 sl) s2)
            ((eqv? s2 sl) s1)
            (else sl))
        (swapper s1 s2 sl)))

; 1.19
(define (list-set lst n x)
    (if (= n 0)
        (cons x (cdr lst))
        (cons
            (car lst)
            (list-set (cdr lst) (- n 1) x))))

; 1.20
(define (count-occurrences s slist)
    (count-occurrences-rec s slist 0))
(define (count-occurrences-rec s slist cnt)
    (if (null? slist)
        cnt
        (if (symbol? slist)
            (if (eqv? s slist)
                (+ cnt 1)
                cnt)
            (+ (count-occurrences-rec s (car slist) cnt) (count-occurrences-rec s (cdr slist) cnt)))))

; 1.21
(define (product sos1 sos2)
    (if (null? sos1)
        `()
        (append (product-a (car sos1) sos2) (product (cdr sos1) sos2))))
(define (product-a x sos2)
    (if (null? sos2)
        `()
        (cons
            (list x (car sos2))
            (product-a x (cdr sos2)))))








