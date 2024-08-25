#lang typed/racket

(provide Chunky (struct-out chunky)
         XZ (struct-out xz) xz->values
         Rect (struct-out rect)
         Point point? make-point point-y point-x point-z
         Chunk-Layout chunk-translate chunk-count
         simple?
         )

(require "ufx.rkt"
         "simple.rkt")

; Indicates that the contained value (e.g. an XZ or a Point)
; is relative to the chunk-id.
(struct (A) chunky ([chunk-id : Integer]
                    [val : A])
  #:type-name Chunky #:transparent)

(struct xz ([x : Fixnum]
            [z : Fixnum])
  #:type-name XZ #:transparent #:extra-constructor-name make-xz)

(define-syntax-rule (xz->values xz-expr)
  (let ([xz : XZ xz-expr])
    (values (xz-x xz)
            (xz-z xz))))

; OUCH - constructor is now x z y which is confusing!
; Should hide this... use a generic interface? No - use composition instead.
(struct point xz ([y : Fixnum])
  #:type-name Point #:transparent)

(define (make-point [xz : XZ] [y : Fixnum])
  (point (xz-x xz) (xz-z xz) y))

(define point-x xz-x)
(define point-z xz-z)


; Each inner vector is one row, holding chunk IDs from east to west.
; The outer vector contains all rows from north to south.
; A chunk ID of false indicates out-of-bounds.
(define-type Chunk-Layout (Vectorof (Vectorof (U #f Integer))))

(: chunk-translate (-> Chunk-Layout XZ (U #f (Chunky XZ))))
(define (chunk-translate chunk-layout xz)
  (let*-values ([(x-offset x) (quotient/remainder (xz-x xz) 32)]
                [(z-offset z) (quotient/remainder (xz-z xz) 32)])
    (let* ([row (vector-ref chunk-layout z-offset)]
           [chunk-id (vector-ref row x-offset)])
      (and chunk-id
           (chunky chunk-id (make-xz x z))))))

(: chunk-count (-> Chunk-Layout Fixnum))
(define (chunk-count layout)
  (define count : Fixnum 0)
  (for ([row layout])
    (for ([cell row])
      (when cell (set! count (ufx+ 1 count)))))
  count)

(struct rect ([start : XZ]
              [end : XZ])
  #:type-name Rect #:transparent)
