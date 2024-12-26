#lang typed/racket

(provide (all-from-out (submod 'builtin))
         :ufxior :ufx+ :ufx- :ufx*
         ufx-in-range)

; Rename and provide "unsafe-fx" procedures with a "ufx" naming convention.
(module builtin typed/racket
  (require racket/fixnum
           racket/unsafe/ops)

  ; Using these /wraparound procedures doesn't seem to make a significant difference.
  #;(require typed/racket/unsafe)
  #;(unsafe-require/typed
     racket/unsafe/ops
     [(unsafe-fx+/wraparound ufx+) (-> Fixnum Fixnum Fixnum)]
     [(unsafe-fx-/wraparound ufx-) (-> Fixnum Fixnum Fixnum)]
     [(unsafe-fx*/wraparound ufx*) (-> Fixnum Fixnum Fixnum)])
  #;(provide ufx+ ufx- ufx*)

  (provide (rename-out [unsafe-fx+ ufx+]
                       [unsafe-fx- ufx-]
                       [unsafe-fx* ufx*]
                       [unsafe-fxquotient ufxquotient]
                       [unsafe-fxremainder ufxremainder]
                       [unsafe-fxnot ufxnot]
                       [unsafe-fxand ufxand]
                       [unsafe-fxxor ufxxor]
                       [unsafe-fxior ufxior]
                       [unsafe-fxlshift ufxlshift]
                       [unsafe-fxrshift ufxrshift]
                       [unsafe-fxmodulo ufxmodulo]
                       ; The following procedures have no unsafe variant,
                       ; but use the "ufx" name anyway so I don't have to remember.
                       [fx=  ufx= ]
                       [fx>= ufx>=]
                       [fx>  ufx> ]
                       [fx<= ufx<=]
                       [fx<  ufx< ]
                       ))
  )

(require (submod 'builtin)
         typed/racket/unsafe)

(define-syntax-rule (define-stars ooo [:id id] ...)
  (begin
    (define-syntax (:id stx)
      (syntax-case stx ()
        [(_ a)
         (syntax/loc stx a)]
        [(_ a b)
         (syntax/loc stx
           (id a b))]
        [(_ a b c ooo)
         (syntax/loc stx
           (id a (:id b c ooo)))]))
    ...))

(define-stars ...
  [:ufxior ufxior]
  [:ufx+ ufx+]
  [:ufx- ufx-]
  [:ufx* ufx*])

(unsafe-require/typed racket
                      [(in-range ufx-in-range)
                       (case->
                        (-> Fixnum (Sequenceof Fixnum))
                        (-> Fixnum Fixnum (Sequenceof Fixnum))
                        (-> Fixnum Fixnum Fixnum (Sequenceof Fixnum)))])
