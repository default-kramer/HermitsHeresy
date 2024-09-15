#lang typed/racket

(provide make-selection selection-ref selection-dst-area)

(require "basics.rkt"
         "transformed-area.rkt"
         "stage.rkt"
         "chunky-area.rkt")

(struct selection ([src-stage : Stage]
                   [xarea : Transformed-Area])
  #:transparent #:type-name Selection)

(: make-selection (-> Stage Chunky-Area (Listof Transform) Selection))
(define (make-selection stage area transforms)
  (define xarea (make-xarea area transforms))
  (selection stage xarea))

(: selection-ref (-> Selection Point (U #f Fixnum)))
(define (selection-ref selection point)
  (let* ([src-stage (selection-src-stage selection)]
         [xarea (selection-xarea selection)]
         [src-xz (xarea-ref xarea point)])
    (and src-xz
         (stage-read src-stage (make-point src-xz (point-y point))))))

(: selection-dst-area (-> Selection Chunky-Area))
(define (selection-dst-area sel)
  (xarea-dst-area (selection-xarea sel)))
