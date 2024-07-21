#lang typed/racket

(provide Dye Blockdef symbol->blockdef fixnum->blockdef
         (struct-out blockdef)
         symbol->ambiguities)

(require "blockdef-raw.rkt"
         "../ufx.rkt")

(define all-blockdefs : (Listof Blockdef)
  (let ([temp (for/list : (Listof (U #f Blockdef))
                ([i : Fixnum (ufx-in-range 2047)])
                (fixnum->blockdef i))])
    (filter blockdef? temp)))

(define ambiguous-symbols : (Immutable-HashTable Symbol (Listof Fixnum))
  (let* ([groups : (Listof (Listof Blockdef))
                 (group-by blockdef-symbol all-blockdefs)]
         [groups (filter (lambda ([grp : (Listof Any)])
                           (not (empty? (cdr grp))))
                         groups)]
         [pairs : (Listof (Pairof Symbol (Listof Fixnum)))
                (map (lambda ([grp : (Listof Blockdef)])
                       (let ([a (blockdef-symbol (first grp))]
                             [b (map blockdef-id grp)])
                         (ann (cons a b)
                              (Pairof Symbol (Listof Fixnum)))))
                     groups)])
    (make-immutable-hash pairs)))

(: symbol->ambiguities (-> Symbol (U #f (Listof Fixnum))))
(define (symbol->ambiguities sym)
  (hash-ref ambiguous-symbols sym (lambda () #f)))
