#lang eopl

(define value?
  (lambda (v)
    #t))

(define-datatype stack stack?
  (empty-stack)
  (push-stack
   (e value?)
   (s stack?))
  (pop-stack
   (s stack?)))

(define push
  (lambda (e s)
    (push-stack e s)))

(define (pop st)
  (cases stack st
    (empty-stack ()
                 (eopl:error 'pop "Empty Stack"))
    (push-stack (e s) s)
    (pop-stack (s) s)))

(define (top st)
  (cases stack st
    (empty-stack ()
                 (eopl:error 'top "Empty Stack"))
    (push-stack (e s) e)
    (pop-stack (s) (top s))))
    
(define (empty-stack? st)
  (cases stack st
    (empty-stack () #t)
    (push-stack (e s) #f)
    (pop-stack (s) (empty-stack? s))))

(define e (empty-stack))
(set! e (push 1 e))
(set! e (push 2 e))
(set! e (push 3 e))





