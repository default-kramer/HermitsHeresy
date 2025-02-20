#lang typed/racket

(provide Chunky (struct-out chunky)
         XZ (struct-out xz) xz->values
         Rect make-rect rect-relative-xz
         rect-start rect-end rect-width rect-height rect-contains?
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

; Note: `start is inclusive, `end` is exclusive.
; Otherwise a zero-size rect would need end < start which would be weird.
(struct rect ([start : XZ]
              [end : XZ])
  #:type-name Rect #:transparent)

(define (make-rect [a : XZ] [b : XZ])
  ; Ensure that `start` is always the upper left and `end` the lower right
  (define-values (x1 z1) (xz->values a))
  (define-values (x2 z2) (xz->values b))
  (rect (xz (min x1 x2) (min z1 z2))
        (xz (max x1 x2) (max z1 z2))))

(define (rect-width [rect : Rect])
  (ufx- (xz-x (rect-end rect))
        (xz-x (rect-start rect))))

(define (rect-height [rect : Rect])
  (ufx- (xz-z (rect-end rect))
        (xz-z (rect-start rect))))

(define (rect-contains? [rect : Rect] [xz : XZ])
  (define-values (x z) (xz->values xz))
  (define-values (x1 z1) (xz->values (rect-start rect)))
  (define-values (x2 z2) (xz->values (rect-end rect)))
  (and (ufx>= x x1)
       (ufx>= z z1)
       (ufx< x x2)
       (ufx< z z2)))

(: rect-relative-xz (-> Rect XZ (U #f XZ)))
; If the given XZ is inside the given rect, returns its offset from the top left corner.
(define (rect-relative-xz rect xz)
  (define-values (x z) (xz->values xz))
  (define-values (x1 z1) (xz->values (rect-start rect)))
  (define-values (x2 z2) (xz->values (rect-end rect)))
  (and (ufx>= x x1)
       (ufx>= z z1)
       (ufx< x x2)
       (ufx< z z2)
       (make-xz (ufx- x x1) (ufx- z z1))))
