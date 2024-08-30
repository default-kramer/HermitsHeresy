#lang racket

(require hermits-heresy
         (submod "../hermits-heresy/hermits-heresy/traversal/traversal.rkt" for-testing)
         (only-in (submod hermits-heresy undocumented) simple?)
         rackunit
         syntax/macro-testing)

(check-exn #rx"block-matches\\?: Cannot be used outside of a traversal"
           (lambda () (convert-compile-time-error (block-matches? 0))))

(check-exn #rx"set-block!: Cannot be used outside of a traversal"
           (lambda () (convert-compile-time-error (set-block! 0))))

(check-exn #rx"foo: Invalid block ID"
           (lambda () (convert-compile-time-error
                       (traversal
                        (when (block-matches? 'foo)
                          (void))))))

(define-syntax-rule (inc! id)
  (set! id (+ 1 id)))

(let* ([match-count 0]
       ; The expression that mutates the following counter gets lifted outside the loop.
       ; I'm not yet convinced this is great default behavior,
       ; but I still want a test to let me know if I accidentally change it.
       [lifted-count 0]
       [trav (traversal (when (block-matches? 3 4 5 6)
                          (inc! match-count)
                          (set-block! (let ()
                                        (inc! lifted-count)
                                        99))))]
       [f (make-testable trav)])
  (for ([block '(1 2 3 4 5 6 7 8 9)])
    (f 'setblock block)
    (let ([result (f 'step)])
      (check-equal? result (case block
                             [(3 4 5 6) 99]
                             [else block]))))
  (check-equal? match-count 4)
  (check-equal? lifted-count 1))


; Make sure we do not overwrite items
(let* ([count-a 0]
       [count-b 0]
       [trav (traversal (inc! count-a)
                        (set-block! 55)
                        (when (block-matches? 55)
                          (inc! count-b)))]
       [f (make-testable trav)])
  (for ([i (in-range 2048)])
    (f 'setblock i)
    (let ([result (f 'step)])
      (check-equal? result (cond [(simple? i) 55]
                                 [else i]))))
  (check-equal? count-a 2048)
  ; I'm not sure if `simple?` is stable, but this range should survive any changes to it:
  (check-true (and (> count-b 500)
                   (< count-b 2000))))
