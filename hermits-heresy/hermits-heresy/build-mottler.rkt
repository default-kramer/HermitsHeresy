#lang racket

(provide build-mottler)

(require (only-in "NEW-API.rkt" block))
(module+ test
  (require rackunit))

(define mottle-arg? (or/c symbol?
                          fixnum?
                          (list/c (or/c symbol? fixnum?)
                                  exact-positive-integer?)))

(define/contract (build-mottler #:prng [in-prng #f] arg . args)
  (->* (mottle-arg?)
       (#:prng (or/c #f pseudo-random-generator?))
       #:rest (listof mottle-arg?)
       any)
  (define (arg->list arg)
    #;(-> mottle-arg? (listof fixnum?))
    (define (make-block x)
      (if (symbol? x)
          (block x)
          x))
    (match arg
      [(list b count) (make-list count (make-block b))]
      [b (list (make-block b))]))
  (define spec (map arg->list (cons arg args)))
  (define vec (list->vector (flatten spec)))
  (define len (vector-length vec))
  (define prng (or in-prng (make-pseudo-random-generator)))
  (lambda ()
    (vector-ref vec (random len prng))))

{module+ test
  (let* ([prng (vector->pseudo-random-generator
                '#(1508418425 1036687506 3886168459 3055413891 3472368804 728140711))]
         [mottle (build-mottler #:prng prng
                                '[Ice 3] ; 'Ice is 20
                                'Earth   ; 'Earth is 2
                                '[44 2]
                                55)])
    (check-false (chaperone? mottle))
    (check-false (impersonator? mottle))
    (check-equal? (for/list ([i (in-range 25)]) (mottle))
                  ; Looks reasonable to me:
                  '(2 44 55 55 20 2 44 20 20 20 20 20 2 44 20 20 55 44 2 2 20 2 44 20 20)))
  }
