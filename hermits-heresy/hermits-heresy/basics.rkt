#lang typed/racket

(provide Chunky (struct-out chunky)
         XZ (struct-out xz)
         Point point? make-point point-y point-x point-z)

; Indicates that the contained value (e.g. an XZ or a Point)
; is relative to the chunk-id.
(struct (A) chunky ([chunk-id : Integer]
                    [val : A])
  #:type-name Chunky #:transparent)

(struct xz ([x : Fixnum]
            [z : Fixnum])
  #:type-name XZ #:transparent #:extra-constructor-name make-xz)

; OUCH - constructor is now x z y which is confusing!
; Should hide this... use a generic interface? No - use composition instead.
(struct point xz ([y : Fixnum])
  #:type-name Point #:transparent)

(define (make-point [xz : XZ] [y : Fixnum])
  (point (xz-x xz) (xz-z xz) y))

(define point-x xz-x)
(define point-z xz-z)
