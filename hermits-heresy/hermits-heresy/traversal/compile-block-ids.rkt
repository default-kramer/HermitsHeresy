#lang racket

(provide compile-block-ids
         (struct-out compiled-blocks))

(require "../blockdata/blockdef.rkt")

(module+ test (require rackunit))

; A block is a 2-byte value
;   cccc ?iii iiii iiii
; The highest 4 bits are the chisel status
; The next bit is a mystery, I wondered if it was a "placed by player" flag
; but I no longer believe that is correct.
; The remaining 11 bits identify which block it is.
;
; The 4-bit chisel status can be one of:
; * 0 - no chisel
; * 1/3/5/7 - diagonal chisel N/E/S/W, matches (blueprint.chisel_status << 4)
; * 2/4/6/8 - diagonal chisel SW/SE/NW/NE
; * 9/a/b/c - concave chisel NW/SW/SE/NE
; * d/e - flat chisel hi/lo
;
; I think it makes sense to handle the block and the chisel as two separate things.
; For example, have block-matches? and chisel-matches? stay independent of each other.
; And set-block! and set-chisel! also stay independent of each other.

(struct compiled-blocks (constant-values ; (Listof Fixnum)
                         runtime-exprs ; (Listof Syntax)
                         )
  #:transparent)

(define (compile-block-ids stx)
  (define (number->syntax num)
    (datum->syntax #f num))
  (define constant-values (mutable-set))
  (define runtime-exprs (list))
  (define QUOTE (datum->syntax stx 'quote))
  (syntax-case stx ()
    [(some-exprs ...)
     (begin
       (for ([expr (syntax-e stx)])
         (syntax-case expr ()
           [(QUOTE id)
            (and (identifier? #'id)
                 (identifier? #'QUOTE)
                 (free-identifier=? #'QUOTE (datum->syntax stx 'quote)))
            (let* ([sym (syntax-e #'id)]
                   [blockdef (symbol->blockdef sym)]
                   [ambigs (symbol->ambiguities sym)])
              (cond
                [ambigs
                 (set-union! constant-values (list->set ambigs))]
                [blockdef
                 (set-add! constant-values (blockdef-id blockdef))]
                [else
                 (raise-syntax-error #f "Invalid block ID" #'id)]))]
           [_
            (let ([datum (syntax-e expr)])
              (cond
                [(fixnum? datum)
                 (set-add! constant-values datum)]
                [(or (pair? datum)
                     (symbol? datum))
                 (set! runtime-exprs (cons expr runtime-exprs))]
                [else
                 (raise-syntax-error #f "Invalid block" expr)]))]))
       (compiled-blocks (set->list constant-values)
                        runtime-exprs))]
    [else
     (raise-syntax-error 'compile-block-ids "needed a list of block IDs" stx)]))

{module+ test
  (define-syntax-rule (check-constants result num ...)
    (check-equal? (sort (compiled-blocks-constant-values result) <)
                  (sort (list num ...) <)))

  (let ([result (compile-block-ids #'('Chert 'Lava 1234))])
    (check-constants result 149 165 174 25 621 1234))
  }
