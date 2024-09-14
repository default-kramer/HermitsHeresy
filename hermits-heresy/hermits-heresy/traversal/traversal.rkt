#lang typed/racket

(provide Argbox (struct-out argbox) make-empty-argbox
         Traversal (struct-out traversal)
         )

(module+ unsafe
  (provide argbox-x
           argbox-z
           argbox-y
           argbox-block
           validate-fixnum-and-set-block!
           make-traversal))

(require "../basics.rkt"
         "../chunky-area.rkt"
         "../ufx.rkt")

(require/typed racket/base [prop:authentic Struct-Type-Property])

; Using Typed Racket for end-user scripts is too slow for my taste;
; the runtime speedup is outweighed by the slower compile times.
; Plus, I don't want end users to have to understand TR.
; So traversal callback code will be untyped.


; This Argbox struct is used to pass data from typed to untyped code.
; Typed code will create an instance of this struct and pass it to
; untyped code which will close over it and return a callback
; proc having type (-> Any).
; Then the typed and untyped code can read and write to the shared
; argbox to communicate.
(struct argbox ([x : Fixnum]
                [z : Fixnum]
                [y : Fixnum]
                [block : Fixnum]
                [skipped-item-count : Fixnum])
  #:transparent #:mutable #:type-name Argbox
  #:property prop:authentic #t)

(define (make-empty-argbox)
  (argbox 0 0 0 0 0))

(define (validate-fixnum-and-set-block! [args : Argbox] [val : Any])
  ; This proc will be provided unsafely.
  ; The untyped code will prove that `args` is actually a Callback-Args,
  ; but it cannot prove that `val` is actually a Fixnum. For example:
  #;(set-block! (string-returning-proc))
  ; So we must do a runtime check of val here, and this also allows us
  ; to give the user a custom error message.
  ; UPDATE
  ; This actually isn't true anymore, the untyped code does an fxior to preserve
  ; the chisel status, so we actually can be assured that `val` is a fixnum.
  ; But dropping this check has no noticable performance impact, so I'll keep it for now.
  (when (not (fixnum? val))
    (error "During traversal, cannot set block to:" val))
  (cond
    [(simple? (argbox-block args))
     (set-argbox-block! args val)]
    [else
     (let ([i (argbox-skipped-item-count args)])
       (set-argbox-skipped-item-count! args (ufx+ 1 i)))]))

(define-type Callback (-> AnyValues))

(struct traversal ([callback-maker : (-> Argbox Callback)]
                   [areas : (Listof Chunky-Area)]
                   [expanded : (Syntaxof Any)]
                   [rewritten : (Syntaxof Any)])
  #:transparent #:type-name Traversal
  #:property prop:authentic #t)

(define (make-traversal [callback-maker : (-> Argbox Callback)]
                        [areas : Any]
                        [expanded : Any]
                        [rewritten : Any])
  ; This proc will be provided unsafely (internally, not to the world).
  ; I wouldn't mind the impersonator on the (-> Callback-Args Callback) maker proc,
  ; but the Callback itself also gets impersonated.
  ; I see no reason to impersonate the callback proc because
  ; 1) doing so produces observable slowdown
  ; 2) the type (-> AnyValues) needs no impersonation -- the only thing
  ;    that could go wrong is passing in >0 arguments which will safely
  ;    fail with a "wrong arity" exception at runtime.
  (when (not (and (list? areas)
                  (andmap chunky-area? areas)))
    (error "TODO"))
  (println (list "got some areas" (length areas)))
  (when (not (syntax? expanded))
    (error "assert fail - expanded was not syntax"))
  (when (not (syntax? rewritten))
    (error "assert fail - rewritten was not syntax"))
  (traversal callback-maker (remove-duplicates areas) expanded rewritten))


{module+ for-testing
  (provide make-testable)
  (define (make-testable [trav : Traversal])
    (let* ([args (make-empty-argbox)]
           [callback ((traversal-callback-maker trav) args)])
      (lambda cmd
        (match cmd
          [(list 'setblock b)
           (set-argbox-block! args (cast b Fixnum))]
          [(list 'step)
           (begin (callback) (argbox-block args))]
          [else
           (error "bad command:" cmd)]))))
  }
