#lang typed/racket

(provide make-selection selection-ref)

(require "basics.rkt"
         "stage.rkt"
         "chunky-area.rkt"
         "ufx.rkt"
         "transformer2d.rkt")

; Rotation and reflection are totally independent of translation.
; The top-left XZ coordinate is the "anchor" which will remain unchanged.

(define-type Transform (U (List 'rotate Integer)
                          'mirror-x
                          'mirror-z
                          (List 'translate Fixnum Fixnum)
                          (List 'translate-to Fixnum Fixnum)))


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

(struct selection ([src-stage : Stage]
                   [src-area : Chunky-Area]
                   [WW : Fixnum] ; width of src-area minus 1
                   [HH : Fixnum] ; height of src-area minus 1
                   [src-translation : XZ] ; translates from (xz 0 0) to src coordinates
                   [transformer : Transformer2D] ; applied at (xz 0 0)
                   [dst-translation : XZ] ; translates from dst coordinates to (xz 0 0)
                   #;[delta-y : Fixnum]
                   #;[dst-area : Chunky-Area])
  #:transparent #:type-name Selection)

(: make-selection (-> Stage Chunky-Area (Listof Transform) Selection))
(define (make-selection stage area transforms)
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
  (selection stage
             area
             WW HH
             top-left
             transformer
             (xz (ufx- 0 abs-x)
                 (ufx- 0 abs-z))))

(: selection-ref (-> Selection Point (U #f Fixnum)))
(define (selection-ref selection point)
  (let* ([src-stage (selection-src-stage selection)]
         [src-area (selection-src-area selection)]
         [WW (selection-WW selection)]
         [HH (selection-HH selection)]
         [src-translation (selection-src-translation selection)]
         [transformer (selection-transformer selection)]
         [dst-translation (selection-dst-translation selection)])
    (let*-values ([(x z) (xz->values point)]
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
           (stage-read src-stage (make-point src-xz (point-y point)))))))
