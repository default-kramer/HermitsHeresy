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

; WARNING - Legacy code. Should migrate everything to the Chunky-Area eventually...
(struct area ([bounds2 : Rect]
              [contains-func : (-> XZ Any)]
              [chunky-area : (U #f Chunky-Area)])
  #:transparent #:type-name Func-Area)

(define-type Area (U Chunky-Area Func-Area))

(define (area-bounds [area : Area])
  (cond
    [(area? area) (area-bounds2 area)]
    [else (chunky-area-bounds area)]))

(: area->contains-func (-> Area (-> XZ Any)))
(define (area->contains-func area)
  (cond
    [(area? area) (area-contains-func area)]
    [else (lambda ([xz : XZ]) (chunky-area-contains? area xz))]))

(: area-contains? (-> Area XZ Any))
(define (area-contains? area xz)
  ((area->contains-func area) xz))

(define-syntax-rule (for/area ([xz-id area-expr])
                      body ...)
  (let* ([area (ann area-expr Area)]
         [bounds (area-bounds area)]
         [contains? (cond
                      [(area? area)
                       (lambda ([xz : XZ]) ((area-contains-func area) xz))]
                      [else
                       (lambda ([xz : XZ]) (chunky-area-contains? area xz))])])
    (for ([z : Fixnum (in-rect/z bounds)])
      (for ([x : Fixnum (in-rect/x bounds)])
        (let ([xz-id (xz x z)])
          (when (contains? xz-id)
            body ...))))))
