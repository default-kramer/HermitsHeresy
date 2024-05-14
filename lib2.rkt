#lang racket

(provide draw-topography topography->pict)

(require "lib.rkt"
         pict
         (only-in racket/draw make-color))

(define (topography->pict top)
  (define peaks (topography-peaks top))
  (define keys (hash-keys peaks))
  (define-values (min-x min-z max-x max-z)
    (get-bounds keys))
  (define-values (min-y max-y)
    (let ([heights (hash-values peaks)])
      (values (apply min heights)
              (apply max heights))))
  (define (make-row z)
    (apply hc-append
           (for/list ([x (in-range min-x (add1 max-x))])
             (let ([item (hash-ref peaks (cons x z) #f)])
               (if item
                   (let* ([a 0]
                          [b (+ 1 (- max-y min-y))]
                          [c (+ 1 (- item min-y))]
                          [ratio (/ c b)]
                          [ratio (+ 0.4 (* 0.6 ratio))])
                     (filled-rectangle 1 1 #:draw-border? #f
                                       #:color (make-color 0 0 0 ratio)))
                   (blank 1 1))))))
  (apply vc-append (for/list ([z (in-range min-z (add1 max-z))])
                     (make-row z))))

(define-syntax-rule (draw-topography id ...)
  (handle-drawn-topography '#(id ...)))
