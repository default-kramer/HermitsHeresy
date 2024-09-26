#lang racket

(provide with-selection)

(require "untyped-traversal.rkt"
         "../selection.rkt"
         "../chunky-area.rkt"
         "../basics.rkt")

(define-syntax-rule (with-selection [id sel] body ...)
  (HHEXPR
   (and (in-area? (selection-dst-area sel))
        (let* ([p (make-point (xz XXX ZZZ) YYY)]
               [id (selection-ref sel p)])
          #;(when (not id)
              ; TODO but is this check actually beneficial?
              (error "assert fail: in-area? should have handled this"))
          (and id
               (simple? id)
               (begin body ...))))))
