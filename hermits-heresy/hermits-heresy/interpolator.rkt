#lang typed/racket

; I think I have implemented bilinear interpolation here.
; https://en.wikipedia.org/wiki/Bilinear_interpolation
; But the exact implementation is not guaranteed.

(provide make-interpolator interpolate)

(require "basics.rkt"
         "ufx.rkt"
         racket/flonum)

(module+ test
  (require typed/rackunit))

; An interpolator is a rectangle that is divided into a grid.
; Each point on the grid will have a value from 0.0 to 1.0 assigned.
; To calculate a value for any given XZ, interpolate the 4 grid points that enclose it.
; The scale defines how far apart each point of the grid is.
; So, for example, if the scale is 12 then the grid will create 12x12 squares.
(struct interpolator ([big-rect : Rect]
                      [scale : Positive-Fixnum]
                      [grid-values : FlVector]
                      [width : Fixnum])
  #:transparent #:type-name Interpolator)

(define (make-interpolator [big-rect : Rect] [scale : Positive-Fixnum])
  (define big-w (rect-width big-rect))
  (define big-h (rect-height big-rect))
  ; Width and height need to be at least 2 even if the quotient is zero.
  ; More generally, adding 2 is needed to ensure the grid covers the entire rect.
  (define width (ufx+ 2 (ufxquotient big-w scale)))
  (define height (ufx+ 2 (ufxquotient big-h scale)))
  (define size (ufx* width height))
  (define vec (make-flvector size))
  (define prng (current-pseudo-random-generator))
  (for ([i (in-range size)])
    (flvector-set! vec i (flrandom prng)))
  (interpolator big-rect scale vec width))

(: interpolate (-> Interpolator XZ (U #f Flonum)))
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

{module+ test
  (let* ([scale 5]
         [rect (make-rect (xz 100 100) (xz 106 106))]
         [i (make-interpolator rect scale)]
         [vec (interpolator-grid-values i)])
    (check-equal? 9 (flvector-length vec)) ; expect a 3x3 grid
    (flvector-set! vec 0 0.0) ;(xz 100 100)
    (flvector-set! vec 1 0.5) ;(xz 105 100)
    ; index 2 doesn't matter   (xz 110 100)
    (flvector-set! vec 3 0.5) ;(xz 100 105)
    (flvector-set! vec 4 1.0) ;(xz 105 105)
    (define (get [xz : XZ])
      (interpolate i xz))
    ; Make sure the corners are correct:
    (check-equal? (get (xz 100 100)) 0.0)
    (check-equal? (get (xz 100 105)) 0.5)
    (check-equal? (get (xz 105 100)) 0.5)
    (check-equal? (get (xz 105 105)) 1.0)
    ; Now test interpolation along the sides:
    (check-equal? (get (xz 102 100)) 0.2)
    (check-equal? (get (xz 100 102)) 0.2)
    (check-equal? (get (xz 103 100)) 0.3)
    (check-equal? (get (xz 100 103)) 0.3)
    ; Now test interpolation along the diagonal:
    (check-equal? (get (xz 101 101)) 0.2)
    (check-equal? (get (xz 102 102)) 0.4)
    (check-equal? (get (xz 103 103)) 0.6)
    (check-equal? (get (xz 104 104)) 0.8)
    (void))
  }
