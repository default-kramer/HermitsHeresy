#lang typed/racket

; I think I have implemented bilinear interpolation here.
; https://en.wikipedia.org/wiki/Bilinear_interpolation
; But the exact implementation is not guaranteed.

(provide make-interpolator interpolate)

(require "basics.rkt"
         "ufx.rkt"
         racket/flonum)

; An interpolator is a rectangle that is divided into a grid.
; Each point on the grid will have a value from 0.0 to 1.0 assigned.
; To calculate a value for any given XZ, interpolate the 4 grid points that enclose it.
; The scale defines how far apart each point of the grid is.
; So, for example, if the scale is 12 then the grid will create 12x12 squares.
(struct interpolator ([big-rect : Rect]
                      [scale : Positive-Fixnum]
                      [grid-values : FlVector]
                      [width : Fixnum])
  #:transparent #:type-name Sampler)

(define (make-interpolator [big-rect : Rect] [scale : Positive-Fixnum])
  (define big-w (rect-width big-rect))
  (define big-h (rect-height big-rect))
  ; add 1 so we can always sample a neighbor
  (define width (ufx+ 1 (ufxquotient big-w scale)))
  (define height (ufx+ 1 (ufxquotient big-h scale)))
  (define size (ufx* width height))
  (define vec (make-flvector size))
  (define prng (current-pseudo-random-generator))
  (for ([i (in-range size)])
    (flvector-set! vec i (flrandom prng)))
  (interpolator big-rect scale vec width))

(: interpolate (-> Sampler XZ (U #f Flonum)))
(define (interpolate interpolator big-xz-global)
  (define big-rect (interpolator-big-rect interpolator))
  (define big-xz (rect-relative-xz big-rect big-xz-global))
  (and
   big-xz
   (let ()
     (define scale (interpolator-scale interpolator))
     (define grid-values (interpolator-grid-values interpolator))
     (define width (interpolator-width interpolator))
     (define-values (x z rx rz)
       (let-values ([(big-x big-z) (xz->values big-xz)])
         (values (ufxquotient big-x scale)
                 (ufxquotient big-z scale)
                 (ufxremainder big-x scale)
                 (ufxremainder big-z scale))))

     (define idxnorth (ufx+ x (ufx* z width)))
     (define idxsouth (ufx+ idxnorth width))
     (define ne (flvector-ref grid-values idxnorth))
     (define nw (flvector-ref grid-values (ufx+ 1 idxnorth)))
     (define se (flvector-ref grid-values idxsouth))
     (define sw (flvector-ref grid-values (ufx+ 1 idxsouth)))

     (define flx (fl/ (->fl rx) (->fl scale)))
     (define flz (fl/ (->fl rz) (->fl scale)))

     (define EE (fl+ ne (fl* flz (fl- se ne))))
     (define WW (fl+ nw (fl* flz (fl- sw nw))))
     (fl+ EE (fl* flx (fl- WW EE)))
     )))
