#lang racket

(provide compile-traversal
         ; Try to provide as little as possible here, and build
         ; more complex things elsewhere using these primitives:
         lift nolift do! block-matches? set-block!
         )

(require "traversal.rkt"
         (prefix-in unsafe: (submod "traversal.rkt" unsafe))
         racket/fixnum
         racket/stxparam
         (for-syntax "compile-block-ids.rkt"
                     racket/list))

(module+ test (require rackunit))

; Sometimes we definitely want to lift exprs outside of the loop.
; Other times we definitely want to keep them inside the loop.
; I'm not yet sure what the default behavior should be, so let's be explicit
; whenever it matters.
(define-syntax-rule (lift a) a)
(define-syntax-rule (nolift a) a)

; For example, here is how `mottle` could be implemented without
; requiring any changes to hermit's heresy!
#;(define-syntax-rule (mottle [blocksym weight] ...)
    (let ([prng (lift (make-pseudo-random-generator))]
          [vec (lift (build-mottle-vector [blocksym weight] ...))]
          [len (lift (vector-length vec))])
      (nolift (let ([idx (random len prng)])
                (vector-ref vec idx)))))

; And then do! is a special kind of nolift indicating that the body
; is being called for side effects and thus cannot be optimized away.
; (Other void/constant expressions that appear to do nothing should
;  be optimized away.)
(define-syntax-rule (do! a b ...)
  (let () a b ...))

(define-syntax-parameter BLOCK #f)

(define-syntax-rule (::block-matches? [constant ...]
                                      [runtime ...])
  ; We know that BLOCK is always a fixnum, but it doesn't seem
  ; that using unsafe-fxand gives any significant boost here.
  (case (fxand BLOCK #x7FF)
    [(constant ...) #t]
    [else (member BLOCK (list runtime ...))]))

(define-syntax-rule (set-block! block)
  (do! (set! BLOCK block)))

(define-syntax (block-matches? stx)
  (syntax-case stx ()
    [(_ block ...)
     (let* ([result (compile-block-ids #'(block ...))]
            [constants (compiled-blocks-constant-values result)]
            [runtime (compiled-blocks-runtime-exprs result)])
       (println result)
       (when (not (empty? runtime))
         (println (list "RUNTIME BLOCK IDS" runtime)))
       (quasisyntax/loc stx
         (::block-matches? [#,@constants]
                           [#,@runtime])))]))

(define-syntax-rule (compile-callback expr ...)
  (lambda (args)
    (when (not (argbox? args))
      ; This check will only be executed once per traversal and it allows
      ; us to confidently use the unsafe: procs that follow.
      (error "assert fail - expected callback args"))
    (let (; here is where lifted expressions will go
          )
      (lambda ()
        (let ()
          (syntax-parameterize ([BLOCK (make-set!-transformer
                                        (lambda (stx)
                                          (syntax-case stx (set!)
                                            [(set! a b)
                                             #'(unsafe:validate-fixnum-and-set-block! args b)]
                                            [a
                                             (identifier? #'a)
                                             #'(unsafe:argbox-block args)])))])
            expr ...))))))

(define-syntax (compile-traversal stx)
  (syntax-case stx ()
    [(_ expr ...)
     (syntax/loc stx
       (unsafe:make-traversal (compile-callback expr ...)))]))
