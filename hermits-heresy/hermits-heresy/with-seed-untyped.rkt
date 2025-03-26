#lang racket

(provide with-absolute-seed)

(module+ test
  (require rackunit))

; Maybe someday I will want `with-relative-seed` which would also be affected
; (deterministically of course) by any enclosing uses of `with-*-seed`.
; Hence the name `with-absolute-seed` here.
(define-syntax-rule (with-absolute-seed [seed:expr] body ...)
  (let* ([seed seed:expr]
         [seed (if (positive? seed)
                   seed
                   (error "seed must be positive, got:" seed))]
         [vec (vector-immutable 1 1 1 1 1 1)]
         [prng (vector->pseudo-random-generator vec)])
    (parameterize ([current-pseudo-random-generator prng])
      (random-seed seed)
      body ...)))

{module+ test
  (check-equal? (with-absolute-seed [53]
                  (random 777))
                528)
  (check-equal? (with-absolute-seed [6137]
                  (random 777))
                47)
  }
