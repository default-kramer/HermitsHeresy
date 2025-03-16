#lang typed/racket

(provide (all-defined-out))

(require "basics.rkt"
         "chunky-area.rkt"
         "ufx.rkt")

(define-syntax-rule (in-rect/x rect)
  (ufx-in-range (xz-x (rect-start rect))
                (xz-x (rect-end rect))))
(define-syntax-rule (in-rect/z rect)
  (ufx-in-range (xz-z (rect-start rect))
                (xz-z (rect-end rect))))

(define-type Area Chunky-Area)

(define area? chunky-area?)
(define area-bounds chunky-area-bounds)
(define area-contains? chunky-area-contains?)

(define-syntax-rule (for/area ([xz-id area-expr])
                      body ...)
  (let* ([area (ann area-expr Area)]
         [bounds (area-bounds area)])
    (for ([z : Fixnum (in-rect/z bounds)])
      (for ([x : Fixnum (in-rect/x bounds)])
        (let ([xz-id (xz x z)])
          (when (chunky-area-contains? area xz-id)
            body ...))))))
