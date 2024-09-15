#lang typed/racket

; An xarea (aka Transformed-Area) is basically a source area with some
; transformations applied.
; Rotation and reflection are totally independent of translation;
; the top-left XZ coordinate is an "anchor" which stays in the same place
; during reflection and rotation.
; The Y coordinate is deliberately excluded to avoid repeating the computation
; 96 times per XZ coordinate.
(provide Transformed-Area Transform
         make-xarea xarea-ref xarea-dst-area)

(require "basics.rkt"
         "chunky-area.rkt"
         "transformer2d.rkt"
         "ufx.rkt")

(define-type Transform (U (List 'rotate Integer)
                          (List 'translate Fixnum Fixnum) ; delta X Z
                          (List 'translate-to Fixnum Fixnum) ; absolute X Z
                          'mirror-x
                          'mirror-z))

(struct xarea ([src-area : Chunky-Area]
               [WW : Fixnum] ; width of src-area minus 1
               [HH : Fixnum] ; height of src-area minus 1
               [src-translation : XZ] ; translates from (xz 0 0) to src coordinates
               [transformer : Transformer2D] ; applied at (xz 0 0)
               [dst-translation : XZ] ; translates from dst coordinates to (xz 0 0)
               [dst-area-promise : (Promise Chunky-Area)])
  #:type-name Transformed-Area #:transparent)

(: standardize (-> Integer Any Fixnum Fixnum (Listof Transform)
                   (Values Integer Any Fixnum Fixnum)))
; Standardize the stack of Transforms such that rotation is one of 0,90,180,270.
; Replace mirror-z with an equivalent mirror-x +180 rotation.
(define (standardize [rotation : Integer]
                     [mirror-x? : Any]
                     [abs-x : Fixnum]
                     [abs-z : Fixnum]
                     [transforms : (Listof Transform)])
  (if (empty? transforms)
      (values rotation mirror-x? abs-x abs-z)
      (match transforms
        [(list (list 'rotate arg) more ...)
         (let ([rotation (remainder (+ rotation arg) 360)])
           (when (not (member rotation '(0 90 180 270)))
             (error "Invalid rotation:" arg))
           (standardize rotation
                        mirror-x?
                        abs-x
                        abs-z
                        more))]
        [(list 'mirror-x more ...)
         (standardize rotation
                      (not mirror-x?)
                      abs-x
                      abs-z
                      more)]
        [(list 'mirror-z more ...)
         (standardize (remainder (+ rotation 180) 360)
                      (not mirror-x?)
                      abs-x
                      abs-z
                      more)]
        [(list (list 'translate dx dz) more ...)
         (standardize rotation
                      mirror-x?
                      (ufx+ dx abs-x)
                      (ufx+ dz abs-z)
                      more)]
        [(list (list 'translate-to x z) more ...)
         (standardize rotation mirror-x? x z more)])))

(: make-xarea (-> Chunky-Area (Listof Transform) Transformed-Area))
(define (make-xarea area transforms)
  (define src-rect (chunky-area-bounds area))
  (define WW (ufx+ -1 (rect-width src-rect)))
  (define HH (ufx+ -1 (rect-height src-rect)))
  (define top-left (rect-start src-rect))
  (define-values (start-x start-z)
    (xz->values top-left))
  (define-values (rotation mirror-x? abs-x abs-z)
    (standardize 0 #f start-x start-z transforms))
  (define transformer : Transformer2D
    (if mirror-x?
        (case rotation
          [(0) reflect+0]
          [(90) reflect+90]
          [(180) reflect+180]
          [(270) reflect+270]
          [else (error "assert fail")])
        (case rotation
          [(0) rotate0]
          [(90) rotate90]
          [(180) rotate180]
          [(270) rotate270]
          [else (error "assert fail")])))
  (define (make-dst-area)
    ; The dst-area is kind of redundant -- it will specify exactly the same
    ; set of XZ coordinates for which `xarea-ref` would return non-false.
    ; But it can be used in traversals to enable an `in-area?` optimization.
    ; So we'll delay the computation until needed.
    (let-values ([(w h) (case rotation
                          [(90 270) (values (rect-height src-rect)
                                            (rect-width src-rect))]
                          [(0 180) (values (rect-width src-rect)
                                           (rect-height src-rect))]
                          [else (error "assert fail")])])
      (define new-bounds (rect (xz abs-x abs-z)
                               (xz (ufx+ w abs-x)
                                   (ufx+ h abs-z))))
      (chunky-area-transform area transformer new-bounds)))
  (xarea area
         WW HH
         top-left
         transformer
         (xz (ufx- 0 abs-x)
             (ufx- 0 abs-z))
         (delay (make-dst-area))))

(: xarea-ref (-> Transformed-Area XZ (U #f XZ)))
; Given a transformed XZ coordinate, returns the corresponding original
; XZ coordinate or false if it would lie outside the original area.
(define (xarea-ref xa coord)
  (let* ([src-area (xarea-src-area xa)]
         [WW (xarea-WW xa)]
         [HH (xarea-HH xa)]
         [src-translation (xarea-src-translation xa)]
         [transformer (xarea-transformer xa)]
         [dst-translation (xarea-dst-translation xa)])
    (let*-values ([(x z) (xz->values coord)]
                  ; translate to 0,0:
                  [(x) (ufx+ x (xz-x dst-translation))]
                  [(z) (ufx+ z (xz-z dst-translation))]
                  ; now we are at 0,0 so apply the transformer:
                  [(x z) (transformer x z WW HH)]
                  ; and now move translate from 0,0 to src coords:
                  [(x) (ufx+ x (xz-x src-translation))]
                  [(z) (ufx+ z (xz-z src-translation))]
                  [(src-xz) (xz x z)])
      (and (chunky-area-contains? src-area src-xz)
           src-xz))))

(: xarea-dst-area (-> Transformed-Area Chunky-Area))
(define (xarea-dst-area xarea)
  (force (xarea-dst-area-promise xarea)))
