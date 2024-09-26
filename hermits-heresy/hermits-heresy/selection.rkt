#lang typed/racket

(provide make-selection selection-ref selection-dst-area)

(require "basics.rkt"
         "ufx.rkt"
         "transformed-area.rkt"
         "stage.rkt"
         "chunky-area.rkt")

(define-type Selection-Transform (U Transform
                                    (List 'adjust-y Fixnum)))

(struct selection ([src-stage : Stage]
                   [xarea : Transformed-Area]
                   [adjust-y : Fixnum])
  #:transparent #:type-name Selection)

(: make-selection (-> Stage Chunky-Area (Listof Selection-Transform) Selection))
(define (make-selection stage area sel-transforms)
  (: get-y-adjustment (-> (Listof Selection-Transform)
                          Fixnum
                          (Listof Transform)
                          (Values Fixnum (Listof Transform))))
  (define (get-y-adjustment sel-transforms y accum)
    (match sel-transforms
      [(list)
       (values y (reverse accum))]
      [(list item more ...)
       (match item
         [(list 'adjust-y dy)
          (get-y-adjustment more (ufx+ y dy) accum)]
         [else
          ; Seems like `ann` should work here, but it doesn't.
          ; Oh well, this code is not speed-critical and `cast` will  be fast anyway.
          (get-y-adjustment more y (cons (cast item Transform)
                                         accum))])]))
  (define-values (adjust-y transforms)
    (get-y-adjustment sel-transforms 0 (list)))
  (define xarea (make-xarea area transforms))
  (selection stage xarea adjust-y))

(: selection-ref (-> Selection Point (U #f Fixnum)))
(define (selection-ref selection point)
  (let ([y (ufx- (point-y point)
                 (selection-adjust-y selection))])
    (and (ufx>= y 0)
         (ufx< y 96)
         (let* ([src-stage (selection-src-stage selection)]
                [xarea (selection-xarea selection)]
                [src-xz (xarea-ref xarea point)])
           (and src-xz
                (stage-read src-stage (make-point src-xz y)))))))

(: selection-dst-area (-> Selection Chunky-Area))
(define (selection-dst-area sel)
  (xarea-dst-area (selection-xarea sel)))
