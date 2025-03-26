#lang typed/racket

(provide simple?)

(require "ufx.rkt")

; The precise definition of `simple?` is "any block which can be placed
; into the blockdata without adding extra information anywhere else."
; The discovery of the "item families" (11*8+1) makes me reasonably
; sure that `simple?` is now correct and complete. [knock on Softwood]
(: simple? (-> Fixnum Boolean))
(define (simple? block)
  (ufx< (ufxand block #x7FF) 1158))

{module+ test
  (require typed/rackunit)
  (check-true (simple? 0))
  (check-true (simple? 1157))
  (check-false (simple? 1158))
  (check-false (simple? 2047))
  ; wrap around
  (check-true (simple? 2048))
  (check-true (simple? 2049))
  (check-true (simple? (+ 1157 2048)))
  (check-false (simple? (+ 1158 2048)))
  }
