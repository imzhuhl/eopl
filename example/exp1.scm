; page 2
; List -> Int
; (list-length ls) = the length of ls
(define (list-length ls)
    (if (null? ls)
        0
        (+ 1 (list-length (cdr ls)))))

; page 14
; (nth-element lst n) = the n-th element of lst
(define (nth-element lst n)
    (if (null? lst)
        #f
        (if (zero? n)
            (car lst)
            (nth-element (cdr lst) (- n 1)))))

; page 16
; (remove-first s los) returns a list with the same elements arranged in the same
; order as los, except that the first ccurrence of the symbol s is removed.
(define (remove-first s los)
    (if (null? los)
        `()
        (if (eqv? s (car los))
            (cdr los)
            (cons car(los) (remove-first s (cdr los))))))

; page 
; a new list is returned that is similar to slist but 
; with all occurrences of old replaced by instances of new.
(define (subst new old slist)
    (if (null? slist)
        `()
        (cons 
            (subst-in-s-exp new old (car slist))
            (subst new old (cdr slist)))))
(define (subst-in-s-exp new old sexp)
    (if (symbol? sexp)
        (if (eqv? old sexp) 
            new 
            sexp)
        (subst new old sexp)))

; page 23
; (number-elements-from ’(v0 v1 v2 ...) n) = ((n v0) (n+1 v1) (n+2 v2) ...)
(define (number-elements-from lst n)
    (if (null? lst)
        `()
        (cons
            (list n (car lst))
            (number-elements-from (cdr lst) (+ n 1)))))

;