#lang racket

(provide compile-traversal area-key-param area-proc-param
         ; Try to provide as little as possible here, and build
         ; more complex things elsewhere using these primitives:
         lift do! block-matches? set-block! HHEXPR YYY XXX ZZZ
         in-area?
         )

(require "traversal.rkt"
         "../block.rkt"
         "../selection.rkt"
         "../chunky-area.rkt"
         "../basics.rkt"
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
; I think the correct default behavior is not to lift unless we can prove that
; it is correct to do so. For example, we can analyze
#;(set-block! 'Chert)
; and decide that it should expand to
#;(set-block! (lift (block 'Chert)))
; But if we have something like
#;(set-block! (my-mottle-proc))
; we should not lift it because we can't know whether it is constant or not.
(define-syntax-rule (lift a) a)

; Idea for the future: do! indicates that the body
; is being called for side effects and thus cannot be optimized away.
; (Other void/constant expressions that appear to do nothing should
;  be optimized away.)
(define-syntax-rule (do! a b ...)
  (let () a b ...))

(define-syntax-parameter BLOCK #f)
(define-syntax-parameter YPARAM #f)
(define-syntax-parameter XPARAM #f)
(define-syntax-parameter ZPARAM #f)

(define-for-syntax disable-context-check? (make-parameter #f))

(define-for-syntax (check-context stx)
  (when (not (disable-context-check?))
    (let ([val (syntax-parameter-value #'BLOCK)])
      (when (not (syntax-parameter-value #'BLOCK))
        (raise-syntax-error #f "Cannot be used outside of a traversal" stx)))))

(define-syntax-rule (::block-matches? [constant ...]
                                      [runtime ...])
  ; We know that BLOCK is always a fixnum, but it doesn't seem
  ; that using unsafe-fxand gives any significant boost here.
  (let ([b (fxand BLOCK #x7FF)])
    (case b
      [(constant ...) #t]
      [else (member b (list runtime ...))])))

(define-syntax (block-matches? stx)
  (check-context stx)
  (syntax-case stx ()
    [(_ block ...)
     (let* ([result (compile-block-ids #'(block ...))]
            [constants (compiled-blocks-constant-values result)]
            [runtime (compiled-blocks-runtime-exprs result)])
       (quasisyntax/loc stx
         (HHEXPR (::block-matches? [#,@constants]
                                   [#,@runtime]))))]))

(define-syntax-rule (::set-block! block)
  ; Keep the other bits untouched (chisel status plus the mystery bit)
  (set! BLOCK (fxior (fxand BLOCK #xF800)
                     (fxand block #x07FF))))

(define-syntax (set-block! stx)
  (check-context stx)
  (syntax-case stx (quote)
    [(_ (quote id))
     (syntax/loc stx
       ; Using (lift (block 'id)) has some nice properties:
       ; 1) It reuses the "but other values were possible" message
       ; 2) It shifts that message to runtime to avoid duplication
       ; 3) It lifts (block 'id) which will run faster than calling it inside the loop
       (HHEXPR (do! (::set-block! (lift (block 'id))))))]
    [(_ val)
     (fixnum? (syntax-e #'val))
     (syntax/loc stx
       (HHEXPR (do! (::set-block! val))))]
    [(_ val)
     ; Don't lift by default here. For example
     #;(set-block! (my-mottle-proc))
     ; should not be lifted and it would definitely surprise someone if we did it by default.
     (syntax/loc stx
       (HHEXPR (do! (::set-block! val))))]))

(define area-key-param (make-parameter #f))
(define area-proc-param (make-parameter #f))

(define (get-area-key area)
  (let ([key ((area-key-param) area)])
    #;(println (list "got area key:" key))
    key))

(define (get-area-proc)
  (let ([proc (area-proc-param)])
    (println (list "got area proc:" proc (impersonator? proc)))
    proc))

(define-syntax (in-area? stx)
  (check-context stx)
  (syntax-case stx ()
    [(_ area)
     (syntax/loc stx
       (HHEXPR
        (let ([:in-area? (lift (get-area-proc))]
              [:area (lift (get-area-key area))])
          (:in-area? :area))))]))

(define-syntax (YYY stx)
  (check-context stx)
  (syntax-case stx ()
    [id
     (identifier? #'id)
     (syntax/loc stx (HHEXPR YPARAM))]))

(define-syntax (XXX stx)
  (check-context stx)
  (syntax-case stx ()
    [id
     (identifier? #'id)
     (syntax/loc stx (HHEXPR XPARAM))]))

(define-syntax (ZZZ stx)
  (check-context stx)
  (syntax-case stx ()
    [id
     (identifier? #'id)
     (syntax/loc stx (HHEXPR ZPARAM))]))


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

; = collect-areas =
; Finds all (get-area-key area) forms and collect them into area-accum,
; a boxed list of areas.
(define-for-syntax (collect-areas orig-stx area-accum)
  (define (recurse stx)
    ;(println (list "recursing" stx))
    (collect-areas stx area-accum))
  (syntax-case orig-stx (get-area-key)
    [(get-area-key area)
     (let ()
       (set-box! area-accum (cons #'area (unbox area-accum)))
       (void))]
    [(stuff ...)
     (let ([items (syntax->list orig-stx)])
       (map recurse items)
       (void))]
    [_ (void)]))

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
                                     #'(unsafe:argbox-block args)])))]
                        ; we already enforce that these can only be used as identifiers
                        [YPARAM (lambda (stx) #'(unsafe:argbox-y args))]
                        [XPARAM (lambda (stx) #'(unsafe:argbox-x args))]
                        [ZPARAM (lambda (stx) #'(unsafe:argbox-z args))])
    body))

(define-syntax (compile-traversal stx)
  (syntax-case stx ()
    [(_ expr ...)
     (let* ([orig #'(let () expr ...)]
            [expanded (parameterize ([disable-context-check? #t])
                        (myexpand orig))]
            [rewritten expanded]
            [area-accum (box (list))]
            [_ (collect-areas rewritten area-accum)]
            [lift-accum (box (list))]
            [rewritten (rewrite rewritten lift-accum)])
       (with-syntax ([(lifted-id ...) (map car (unbox lift-accum))]
                     [(lifted-expr ...) (map cdr (unbox lift-accum))]
                     [(collected-area ...) (unbox area-accum)])
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
            (list collected-area ...)
            #'#,expanded
            #'#,rewritten
            ))))]))
