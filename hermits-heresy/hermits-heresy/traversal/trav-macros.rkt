#lang racket

(provide with-selection)

(require "untyped-traversal.rkt"
         "../selection.rkt"
         "../basics.rkt")

(define-syntax-rule (with-selection [id sel] body ...)
  (HHEXPR
   (let* ([p (make-point (xz XXX ZZZ) YYY)]
          [id (selection-ref sel p)])
     (and id
          (simple? id)
          (begin body ...))))
  ; TODO should use in-area? once that exists...
  #;(HHEXPR
     (and (in-area? (lift (selection-area sel)))
          (let ([blk (or (selection-ref selection POINT)
                         (error "assert fail"))])
            (and (simple? blk)
                 (begin body ...))))))
