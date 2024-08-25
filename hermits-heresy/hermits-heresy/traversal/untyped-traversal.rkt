#lang racket

(provide compile-traversal
         ; Try to provide as little as possible here, and build
         ; more complex things elsewhere using these primitives:
         lift nolift do! block-matches? set-block!
         )

(require "traversal.rkt"
         "../TEMP.rkt"
         (prefix-in unsafe: (submod "traversal.rkt" unsafe))
         racket/fixnum
         racket/stxparam
         (for-syntax "compile-block-ids.rkt"
                     racket/match
                     racket/list))

(module+ test (require rackunit))

(define-syntax-rule (HHEXPR a) a)

; Sometimes we definitely want to lift exprs outside of the loop.
; Other times we definitely want to keep them inside the loop.
; I'm not yet sure what the default behavior should be, so let's be explicit
; whenever it matters.
; NOPE - I think the correct thing to do is to use lift to request that the
; expression be lifted, but if it contains a nolift then it cannot be.
; This would mean you have to be very careful with nolift to avoid accidentally
; thwarting an important lift...
; AHA Maybe what we really need is 3 things: mustlift, lift, and nolift.
; Both mustlift and lift request that the expression be lifted,
; but only mustlift will cause an error if it contains a nolift.
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

(define-syntax-rule (set-block! id)
  (HHEXPR (do! (set! BLOCK (lift (block id))))))

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


; = myexpand =
; Performs partial expansion.
; If a macro expands to (HHEXPR expr) it will accept the expansion and
; replace it with just `expr` (dropping the HHEXPR wrapper).
; All other macros are left unexpanded.
(define-for-syntax (myexpand stx)
  (define (step stx)
    (local-expand stx 'expression (list #'HHEXPR)))
  (syntax-case stx ()
    [(a b ...)
     (let ([exp (step stx)])
       (syntax-case exp (HHEXPR)
         [(HHEXPR a)
          (let ()
            ;(println "expand step:")
            ;(println exp)
            (myexpand #'a))]
         [anything
          (datum->syntax stx (map myexpand (syntax->list stx)) stx stx)]))]
    [anything stx]))

; = rewrite =
; Replaces each (lift expr) with a generated identifier.
; The lift-accum is a boxed list that will contain pairs
; of (cons generated-id expr) for each (lift expr) that was replaced.
(define-for-syntax (rewrite orig-stx lift-accum)
  (define (recurse stx)
    ;(println (list "recursing" stx))
    (rewrite stx lift-accum))
  (define stx
    (syntax-case orig-stx ()
      [(stuff ...)
       (let ([items (syntax->list orig-stx)])
         (datum->syntax orig-stx (map recurse items) orig-stx orig-stx))]
      [_ orig-stx]))
  (syntax-case stx (lift)
    [(lift expr)
     (let ([id (datum->syntax #f (gensym))])
       (set-box! lift-accum (cons (cons id #'expr)
                                  (unbox lift-accum)))
       id)]
    [_ stx]))

(define-syntax-rule (callback-body args body)
  (syntax-parameterize ([BLOCK (make-set!-transformer
                                (lambda (stx)
                                  (syntax-case stx (set!)
                                    [(set! a b)
                                     #'(unsafe:validate-fixnum-and-set-block! args b)]
                                    [a
                                     (identifier? #'a)
                                     #'(unsafe:argbox-block args)])))])
    body))

(define-syntax (compile-traversal stx)
  (syntax-case stx ()
    [(_ expr ...)
     (let* ([orig #'(let () expr ...)]
            [expanded (myexpand orig)]
            [lift-accum (box (list))]
            [rewritten (rewrite expanded lift-accum)])
       (with-syntax ([(lifted-id ...) (map car (unbox lift-accum))]
                     [(lifted-expr ...) (map cdr (unbox lift-accum))])
         (quasisyntax/loc stx
           (unsafe:make-traversal
            (lambda (args)
              (when (not (argbox? args))
                ; This check will only be executed once per traversal and it allows
                ; us to confidently use the unsafe: procs inside callback-body.
                (error "assert fail - expected callback args"))
              (let ([lifted-id lifted-expr]
                    ...)
                (lambda () (callback-body args #,rewritten))))
            #'#,expanded
            #'#,rewritten
            ))))]))
