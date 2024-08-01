#lang typed/racket

(provide Chunk-Layout
         IoA
         small-soggy-skerry
         med-soggy-skerry
         large-soggy-skerry
         small-coral-cay
         med-coral-cay
         large-coral-cay)

; Each inner vector is one row, holding chunk IDs from east to west.
; The outer vector contains all rows from north to south.
; A chunk ID of false indicates out-of-bounds.
(define-type Chunk-Layout (Vectorof (Vectorof (U #f Integer))))

(define (parse-map [rows : (Listof (Listof (U '_ 'X)))])
  (let ([chunk-id -1])
    (for/vector : (Vectorof (Vectorof (U #f Integer)))
      ([row rows])
      (for/vector : (Vectorof (U #f Integer))
        ([cell row])
        (case cell
          [(_) #f]
          [(X) (begin (set! chunk-id (+ 1 chunk-id))
                      chunk-id)]
          [else (error "assert fail")])))))

(define IoA : Chunk-Layout
  (parse-map '((_ _ _ _ _ _ _ X X X X X X X X _ _ _ X X X _ _ _ _ _ _)
               (_ _ _ _ _ _ X X X X X X X X X X X X X X X X X _ _ _ _)
               (_ _ _ _ X X X X X X X X X X X X X X X X X X X _ _ _ _)
               (_ _ _ X X X X X X X X X X X X X X X X X X X X _ _ _ _)
               (_ _ X X X X X X X X X X X X X X X X X X X X X X X X _)
               (_ X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (_ X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X _)
               (_ X X X X X X X X X X X X X X X X X X X X X X X _ _ _)
               (_ _ X X X X X X X X X X X X X X X X X X X X X _ _ _ _)
               (_ _ X X X X X X X X X X X X X X X X X X X _ _ _ _ _ _)
               (_ _ _ _ X X X X X X X X X X X X X X X _ _ _ _ _ _ _ _)
               (_ _ _ _ _ _ _ _ _ X X X X X X _ _ _ _ _ _ _ _ _ _ _ _)
               (_ _ _ _ _ _ _ _ _ X X X X X _ _ _ _ _ _ _ _ _ _ _ _ _)
               (_ _ _ _ _ _ _ _ _ _ X X X _ _ _ _ _ _ _ _ _ _ _ _ _ _))))

(define small-soggy-skerry : Chunk-Layout
  (parse-map '((X X X X X X X)
               (X X X X X X X)
               (X X X X X X X)
               (X X X X X X X)
               (X X X X X X X)
               (X X X X X X X)
               (X X X X X X X)
               (_ X X _ _ _ _)
               (_ X X _ _ _ _))))

(define small-coral-cay : Chunk-Layout
  (parse-map '((X X X _ X X X)
               (X X X X X X X)
               (X X X X X X X)
               (X X X X X X X)
               (X X X X X X X)
               (X X X X X X X)
               (X X X X X _ _)
               (_ X X _ _ _ _)
               (_ X X _ _ _ _))))

(define med-soggy-skerry : Chunk-Layout
  (parse-map '((X X X X X X X X X X)
               (X X X X X X X X X X)
               (X X X X X X X X X X)
               (X X X X X X X X X X)
               (X X X X X X X X X X)
               (X X X X X X X X X X)
               (X X X X X X X X X X)
               (X X X X X X X X X X)
               (X X X X X X X X X X)
               (X X X X X X X X X X)
               (_ X X _ _ _ _ _ _ _)
               (_ X X _ _ _ _ _ _ _))))

(define med-coral-cay : Chunk-Layout
  (parse-map '((X X X X X X X X _ _)
               (X X X X X X X X X X)
               (X X X X X X X X X X)
               (X X X X X X X X X X)
               (X X X X X X X X X _)
               (X X X X X X X X X _)
               (X X X X X X X X X _)
               (X X X X X X X X X _)
               (X X X X X X X X X _)
               (_ _ _ X X X _ _ _ _)
               (_ X X _ _ _ _ _ _ _)
               (_ X X _ _ _ _ _ _ _))))

(define large-soggy-skerry : Chunk-Layout
  (parse-map '((X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X)
               (X X X X X X X X X X X X _)
               (X X X X X X X X X X X X _)
               (X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X)
               (_ X X _ _ _ _ _ _ _ _ _ _)
               (_ X X _ _ _ _ _ _ _ _ _ _))))

(define large-coral-cay : Chunk-Layout
  (parse-map '((X X X X X X X X X X X)
               (X X X X X X X X X X X)
               (X X X X X X X X X X X)
               (X X X X X X X X X X X)
               (X X X X X X X X X X X)
               (X X X X X X X X X X X)
               (X X X X X X X X X X X)
               (X X X X X X X X X X X)
               (X X X X X X X X X X X)
               (X X X X X X X X X X X)
               (X X X X X X X X X X X)
               (_ _ _ _ _ _ _ _ _ _ _)
               (X X _ _ _ _ _ _ _ _ _)
               (X X _ _ _ _ _ _ _ _ _))))
