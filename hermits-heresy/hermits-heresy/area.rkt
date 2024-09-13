#lang typed/racket

(provide (all-defined-out))

(require "basics.rkt"
         "chunky-area.rkt")

; WARNING - Legacy code. Should migrate everything to the Chunky-Area eventually...
(struct area ([bounds2 : Rect]
              [contains-func : (-> XZ Any)]
              [chunky-area : (U #f Chunky-Area)])
  #:transparent #:type-name Func-Area)

(define-type Area (U Chunky-Area Func-Area))
