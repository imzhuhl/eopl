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

; 1.22
(define (filter-in pred lst)
    (if (null? lst)
        `()
        (if (pred (car lst))
            (cons (car lst) (filter-in pred (cdr lst)))
            (filter-in pred (cdr lst)))))

; 1.23
(define (list-index pred lst)
        (list-index-a pred lst 0))
(define (list-index-a pred lst cnt)
    (if (null? lst)
        #f
        (if (pred (car lst))
            cnt
            (list-index-a pred (cdr lst) (+ cnt 1)))))

; 1.24
(define (every? pred lst)
    (if (null? lst)
        #t
        (if (pred (car lst))
            (every? pred (cdr lst))
            #f)))

; 1.25
(define (exists? pred lst)
    (if (null? lst)
        #f
        (if (pred (car lst))
            #t
            (exists? pred (cdr lst)))))

; 1.26
(define (up lst)
    (if (null? lst)
        `()
        (if (list? (car lst))
            (append (car lst) (up (cdr lst)))
            (cons (car lst) (up (cdr lst))))))

; 1.27
(define (flatten slist)
    (if (null? slist)
        `()
        (if (symbol? slist)
            (list slist)
            (append (flatten (car slist)) (flatten (cdr slist))))))

; 1.28
(define (merge loi1 loi2)
    (cond ((and (not (null? loi1)) (not (null? loi2)))
        (if (< (car loi1) (car loi2))
            (append (list (car loi1)) (merge (cdr loi1) loi2))
            (append (list (car loi2)) (merge loi1 (cdr loi2)))))
        ((null? loi1) loi2)
        ((null? loi2) loi1)))

; 1.29
; 选择排序
(define (sort loi)
    (sort-a `() loi))
(define (sort-a nls loi)
    (if (null? loi)
        nls
        (sort-a (insert nls (car loi)) (cdr loi))))
(define (insert lst x)
    (cond 
        ((null? lst) (list x))
        ((< x (car lst)) (cons x lst))
        (else (cons (car lst) (insert (cdr lst) x)))))

; 1.30
(define (sort-pred pred loi)
    (sort-pred-a pred `() loi))
(define (sort-pred-a pred nls loi)
    (if (null? loi)
        nls
        (sort-pred-a pred (insert-a pred nls (car loi)) (cdr loi))))
(define (insert-a pred lst x)
    (cond
        ((null? lst) (list x))
        ((pred x (car lst)) (cons x lst))
        (else (cons (car lst) (insert-a pred (cdr lst) x)))))













