#lang typed/racket

(provide Argbox (struct-out argbox)
         Traversal (struct-out traversal)
         )

(module+ unsafe
  (provide argbox-x
           argbox-z
           argbox-y
           argbox-block
           validate-fixnum-and-set-block!
           make-traversal))

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
                [block : Fixnum])
  #:transparent #:mutable #:type-name Argbox
  #:property prop:authentic #t)

(define (validate-fixnum-and-set-block! [args : Argbox] [val : Any])
  ; This proc will be provided unsafely.
  ; The untyped code will prove that `args` is actually a Callback-Args,
  ; but it cannot prove that `val` is actually a Fixnum. For example:
  #;(set-block! (string-returning-proc))
  ; So we must do a runtime check of val here, and this also allows us
  ; to give the user a custom error message.
  (when (not (fixnum? val))
    (error "During traversal, cannot set block to:" val))
  (set-argbox-block! args val))

(define-type Callback (-> AnyValues))

(struct traversal ([callback-maker : (-> Argbox Callback)]
                   [expanded : (Syntaxof Any)]
                   [rewritten : (Syntaxof Any)])
  #:transparent #:type-name Traversal
  #:property prop:authentic #t)

(define (make-traversal [callback-maker : (-> Argbox Callback)]
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
  (when (not (syntax? expanded))
    (error "assert fail - expanded was not syntax"))
  (when (not (syntax? rewritten))
    (error "assert fail - rewritten was not syntax"))
  (traversal callback-maker expanded rewritten))
