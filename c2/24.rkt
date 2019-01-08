#lang eopl

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define (bintree->list tree)
  (cases bintree tree
    (leaf-node (num)
               (list 'leaf-node num))
    (interior-node (key left right)
                   (list 'interior-node
                         key
                         (bintree->list left)
                         (bintree->list right)))))

(define tree (interior-node 'a (leaf-node 3) (leaf-node 4)))






  