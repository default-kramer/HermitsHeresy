#lang typed/racket

(provide make-sampler sample) ; TODO rename?

(require "basics.rkt"
         "ufx.rkt"
         racket/flonum)

(struct sampler ([big-rect : Rect]
                 [scale : Positive-Fixnum]
                 [small-values : FlVector]
                 [width : Fixnum])
  #:transparent #:type-name Sampler)

(define (make-sampler [big-rect : Rect] [scale : Positive-Fixnum])
  (define big-w (rect-width big-rect))
  (define big-h (rect-height big-rect))
  ; add 1 so we can always sample a neighbor
  (define width (ufx+ 1 (ufxquotient big-w scale)))
  (define height (ufx+ 1 (ufxquotient big-h scale)))
  (define size (ufx* width height))
  (define vec (make-flvector size))
  (define prng (current-pseudo-random-generator))
  (for ([i (in-range size)])
    (flvector-set! vec i (flrandom prng)))
  (sampler big-rect scale vec width))

(: sample (-> Sampler XZ (U #f Flonum)))
(define (sample sampler big-xz-global)
  (define big-rect (sampler-big-rect sampler))
  (define big-xz (rect-relative-xz big-rect big-xz-global))
  (and
   big-xz
   (let ()
     (define scale (sampler-scale sampler))
     (define small-values (sampler-small-values sampler))
     (define width (sampler-width sampler))
     (define-values (x z rx rz)
       (let-values ([(big-x big-z) (xz->values big-xz)])
         (values (ufxquotient big-x scale)
                 (ufxquotient big-z scale)
                 (ufxremainder big-x scale)
                 (ufxremainder big-z scale))))

     (define idxnorth (ufx+ x (ufx* z width)))
     (define idxsouth (ufx+ idxnorth width))
     (define ne (flvector-ref small-values idxnorth))
     (define nw (flvector-ref small-values (ufx+ 1 idxnorth)))
     (define se (flvector-ref small-values idxsouth))
     (define sw (flvector-ref small-values (ufx+ 1 idxsouth)))

     (define flx (fl/ (->fl rx) (->fl scale)))
     (define flz (fl/ (->fl rz) (->fl scale)))

     (define EE (fl+ ne (fl* flz (fl- se ne))))
     (define WW (fl+ nw (fl* flz (fl- sw nw))))
     (fl+ EE (fl* flx (fl- WW EE)))
     )))

#;{begin
    (define s (make-sampler (make-rect (xz 0 0) (xz 300 300)) 10))
    (for/list : (Listof (U #f Flonum)) ([i (in-range 30)])
      (sample s (xz i 0)))

    (define worst : Flonum 0.0)

    (for ([x (ufx-in-range 299)])
      (for ([z (ufx-in-range 299)])
        (define (samp [dx : Fixnum] [dz : Fixnum])
          (or (sample s (xz (ufx+ dx x) (ufx+ dz z)))
              (error "assert fail")))
        (let* ([me (samp 0 0)]
               [east (samp 1 0)]
               [south (samp 0 1)]
               [se (samp 1 1)]
               [minval (min me east south se)]
               [maxval (max me east south se)]
               [diff1 (- me minval)]
               [diff2 (- maxval me)])
          (set! worst (flmax diff1 worst))
          (set! worst (flmax diff2 worst))
          (when (> diff1 0.17)
            (println (list "WHOA diff1" diff1 x z me east south se)))
          (when (> diff2 0.17)
            (println (list "WHOA diff2" diff2 x z me east south se))))))
    }

#;{begin
    (let* ([rect (make-rect (xz 0 0) (xz 10 10))]
           [scale 10]
           [vec (flvector 0.0 1.0 1.0 1.0)]
           [width 2]
           [s (sampler rect scale vec width)])
      (for ([z : Fixnum (ufx-in-range 10)])
        (println (for/list : (Listof (U #f Flonum)) ([x (in-range 10)])
                   (sample s (xz x z)))))
      (println (for/list : (Listof (U #f Flonum)) ([i (in-range 10)])
                 (sample s (xz 0 i))))
      (println (for/list : (Listof (U #f Flonum)) ([i (in-range 10)])
                 (sample s (xz i 0))))
      (println (for/list : (Listof (U #f Flonum)) ([i (in-range 10)])
                 (sample s (xz i i)))))
    }
