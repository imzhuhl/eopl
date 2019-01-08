#lang eopl

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define (tree-sum bt)         
  (cases bintree bt
    (leaf-node (num)
               (list 'leaf num))
    (interior-node (key left right)
                   (let* ([left-sum (tree-sum left)]
                          [right-sum (tree-sum right)]
                          [curr-sum (+
                                     (if (eqv? (car left-sum) 'leaf) (cadr left-sum) (cadar left-sum))
                                     (if (eqv? (car right-sum) 'leaf) (cadr right-sum) (cadar right-sum)))])
                     (list (list key curr-sum) left-sum right-sum)))))

(define (find-max lst)
  (cond
    ((and (eqv? (caadr lst) 'leaf) (eqv? (caaddr lst) 'leaf)) (car lst))
    ((eqv? (caadr lst) 'leaf)
     (let ([right-lst (find-max (caddr lst))])
       (if (< (cadar lst) (cadr right-lst))
           right-lst
           (car lst))))
    ((eqv? (caaddr lst) 'leaf)
     (let ([left-lst (find-max (cadr lst))])
       (if (< (cadar lst) (cadr left-lst))
           left-lst
           (car lst))))
    (else
     (let ([left-lst (find-max (caddr lst))]
           [right-lst (find-max (cadddr lst))])
       (if (and (>= (cadar lst) (cadr left-lst)) (>= (cadar lst) (cadr (right-lst))))
           (car lst)
           (if (>= (cadr left-lst) (cadr (right-lst)))
               left-lst
               right-lst))))))

(define (max-interior bt)
  (car (find-max (tree-sum bt))))

(define t1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define t2
  (interior-node 'bar (leaf-node -1) t1))
(define t3
  (interior-node 'baz t2 (leaf-node 1)))








