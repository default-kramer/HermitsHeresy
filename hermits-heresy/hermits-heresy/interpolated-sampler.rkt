#lang typed/racket

(provide make-interpolated-sampler)

(require "basics.rkt"
         "area.rkt"
         "ufx.rkt"
         "interpolator.rkt"
         racket/flonum
         racket/fixnum)

(module+ test
  (require typed/rackunit))

(struct interpolated-sampler fixnum-sampler
  () ; maybe for future use
  #:type-name Interpolated-Sampler
  #:property prop:authentic #t
  #:transparent)

(: make-interpolated-sampler (-> (U Rect Area)
                                 Positive-Fixnum
                                 (U (Listof Fixnum)
                                    (List Fixnum '.. Fixnum))
                                 Interpolated-Sampler))
(define (make-interpolated-sampler area-arg scale range)
  (define bounding-rect : Rect
    (cond
      [(rect? area-arg) area-arg]
      [else (area-bounds area-arg)]))
  (define interpolator (make-interpolator bounding-rect scale))

  (define-syntax-rule (make-sample-func [flonum:id] body ...)
    (let ()
      (define-syntax-rule (do-sample xz)
        (let ([sample (interpolate interpolator xz)])
          (and sample
               (let ([flonum:id : Flonum sample])
                 body ...))))
      (ann (cond
             [(area? area-arg)
              (lambda ([xz : XZ])
                (and (area-contains? area-arg xz)
                     (do-sample xz)))]
             [else (lambda ([xz : XZ]) (do-sample xz))])
           (-> XZ (U #f Fixnum)))))

  (define sample-func
    (match range
      [(list inclusive-lo '.. inclusive-hi)
       (when (ufx< inclusive-hi inclusive-lo)
         (error "invalid range; must be '[lo .. hi] but got:" range))
       (let* ([len (ufx- inclusive-hi inclusive-lo)]
              [len (fx->fl (ufx+ 1 len))])
         (make-sample-func
          [flonum]
          (let ([adder (fl->fx (fl* flonum len))])
            (ufx+ inclusive-lo adder))))]
      [(list fixnums ...)
       #:when (andmap fixnum? fixnums)
       (let* ([vec (ann (list->vector fixnums)
                        (Vectorof Fixnum))]
              [len (fx->fl (vector-length vec))])
         (make-sample-func
          [flonum]
          (let ([idx (fl->fx (fl* flonum len))])
            (vector-ref vec idx))))]
      [else
       (error "assert fail: unexpected range:" range)]))

  (interpolated-sampler sample-func bounding-rect))

{module+ test
  (let* ([sampler (with-absolute-seed [2222]
                    (make-interpolated-sampler (make-rect (xz 100 100) (xz 200 200))
                                               8
                                               '[10 .. 19]))]
         [sample (fixnum-sampler-func sampler)])
    ; upper left corner:
    (check-false (sample (xz 99 100)))
    (check-false (sample (xz 100 99)))
    (check-not-false (sample (xz 100 100)))
    ; lower right corner:
    (check-not-false (sample (xz 199 199)))
    (check-false (sample (xz 200 199)))
    (check-false (sample (xz 199 200)))

    ; Test distributions.
    ; This should be deterministic due to with-absolute-seed.
    ; But, the implementation of the interpolated sampler is NOT guaranteed
    ; at this time (because I think I'm doing bilinear interpolation wrong)
    ; so this would not necessarily indicate a breaking change:
    (let ([dist (make-vector 21 0)])
      (for ([z (in-range 100 200)])
        (for ([x (in-range 100 200)])
          (let ([val (sample (xz x z))])
            (when (not val)
              (error "assert fail" x z))
            (vector-set! dist val (+ 1 (vector-ref dist val))))))
      (check-equal? (vector->list dist)
                    '(0 0 0 0 0 0 0 0 0 0 ; [0 .. 9] out of range
                        80   ; 10
                        529  ; 11
                        1090 ; 12
                        1639 ; 13
                        1991 ; 14
                        1890 ; 15
                        1377 ; 16
                        876  ; 17
                        417  ; 18
                        111  ; 19
                        0    ; 20 out of range
                        ))))
  }
