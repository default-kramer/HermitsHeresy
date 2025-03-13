#lang typed/racket

(provide Bitmap-Sampler make-bitmap-sampler
         RGB-Spec Normalize-Spec Project-Spec)

(require "basics.rkt"
         "ufx.rkt"
         typed/pict
         (only-in typed/racket/draw Bitmap%))

(struct bitmap-sampler fixnum-sampler
  () ; may want to add filename for error messaging someday
  #:type-name Bitmap-Sampler
  #:property prop:authentic #t
  #:transparent)

; Convenient name for function compositions in this file.
(define-type Sampler (-> XZ (U #f Fixnum)))

; Describes how to convert from RGB to a single grayscale value.
(define-type RGB-Spec (U 'r 'g 'b 'max 'min))

(define-type Normalize-Spec
  (U
   ; No normalization.
   ; The range is defined as [0 .. 255] regardless of which grayscale
   ; values actually occur in the bitmap.
   #f

   ; If the bitmap contains N distinct grayscale colors, the darkest
   ; value will be remapped to 0 and the lightest to N-1.
   ; The range will be this inclusive range.
   '[0 1 ... N-1]
   ))

(define-type Project-Spec
  ; During the projection, we have a range of grayscale values.
  ; Without any normalization, the range will always be [0 .. 255] inclusive.
  ; With normalization, the range could be something like [0 1 2 3] for a
  ; bitmap containing 4 distinct grayscale colors.
  ; When the project spec mentions 'lightest and 'darkest it is referring
  ; to the largest and smallest values of this range.
  (U
   #f ; identity; the grayscale value is projected directly

   #;("for example" '([lightest 40] [step -1/3]))
   ; would mean "the lightest value projects to 40, and every 3rd step
   ; taken towards the darkest value adds -1 to the projected value."
   ; You can only specify one end of the range and this implies that
   ; you are "stepping" towards the other end of the range.
   (List (List (U 'lightest 'darkest) Fixnum)
         (List 'step Exact-Rational))

   #;("for example" '[28 29 30 31])
   ; would mean "I assert that the grayscale range has exactly 4
   ; values. The darkest value will project to 28 and the lightest will
   ; project to 31."
   (Listof Fixnum)))

(: invert (-> Sampler Sampler))
(define (invert sampler)
  (lambda (xz)
    (let ([byte (sampler xz)])
      (and byte (ufx- 255 byte)))))

(: normalize (-> Normalize-Spec Sampler Fixnum Fixnum (Values Fixnum Fixnum Sampler)))
#;(#:returns (inclusive-range-lo inclusive-range-hi sampler))
(define (normalize spec sampler width height)
  (match spec
    [#f
     (values 0 255 sampler)]
    [(list 0 1 '... 'N-1)
     ; Scan the entire sample space and remap all non-false samples
     ; such that the smallest value is remapped to 0,
     ; the second-smallest value is remapped to 1, and so on.
     (let ([lookup : (Mutable-Vectorof (U #f Fixnum))
                   (make-vector 256 #f)])
       (for ([z : Fixnum (ufx-in-range height)])
         (for ([x : Fixnum (ufx-in-range width)])
           (let ([sample (sampler (xz x z))])
             (when sample
               ; set to any non-false value for now
               (vector-set! lookup sample 42)))))
       ; Now reassign the actual values
       (define counter : Fixnum 0)
       (for ([i (ufx-in-range 256)])
         (when (vector-ref lookup i)
           (vector-set! lookup i counter)
           (set! counter (ufx+ 1 counter))))
       (values 0 (ufx+ -1 counter)
               (lambda (xz)
                 (let ([byte (sampler xz)])
                   (and byte (vector-ref lookup byte))))))]
    [else
     (error "assert fail: unexpected normalize spec:" spec)]))

(: project (-> Project-Spec Sampler Fixnum Fixnum Sampler))
(define (project spec sampler inclusive-range-lo inclusive-range-hi)
  (define-syntax-rule (handle-lightest/darkest start-val step-val [byte] raw-steps-expr)
    (let ([d (cast (denominator step-val) Fixnum)]
          [n (cast (numerator step-val) Fixnum)])
      (lambda (xz)
        (let ([byte (sampler xz)])
          (and byte
               (let* ([steps raw-steps-expr]
                      [steps (ufx* steps n)]
                      [steps (ufxquotient steps d)])
                 (ufx+ start-val steps)))))))
  (match spec
    [#f
     sampler]
    [(list (list 'lightest start-val)
           (list 'step step-val))
     (handle-lightest/darkest start-val step-val [byte]
                              (ufx- inclusive-range-hi byte))]
    [(list (list 'darkest start-val)
           (list 'step step-val))
     (handle-lightest/darkest start-val step-val [byte]
                              (ufx- byte inclusive-range-lo))]
    [(list fixnums ...)
     #:when (andmap fixnum? fixnums)
     (let* ([vec : (Vectorof Fixnum) (list->vector fixnums)]
            [need-count (ufx+ 1 (ufx- inclusive-range-hi inclusive-range-lo))]
            [have-count (vector-length vec)])
       (when (ufx< have-count need-count)
         ; NOMERGE include need-count and echo back the given list
         ; and write a test for this error message
         (error "Insufficient #:values were provided."))
       (lambda (xz)
         (let ([byte (sampler xz)])
           (and byte
                (vector-ref vec byte)))))]
    [else (error "assert fail: unexpected project spec:" spec)]))

(: make-bitmap-sampler (->* [(U (Instance Bitmap%) Path-String)
                             #:rgb RGB-Spec
                             #:project Project-Spec
                             ]
                            [#:invert? Any
                             #:normalize Normalize-Spec
                             ]
                            Bitmap-Sampler))
(define (make-bitmap-sampler arg
                             #:rgb rgb-spec
                             #:invert? [rgb-invert? #f]
                             #:normalize [normalize-spec #f]
                             #:project project-spec
                             )
  (define bmp (bitmap arg))
  (define width : Fixnum
    (let ([w (pict-width bmp)])
      (or (and (fixnum? w) (cast w Fixnum))
          (error "bad width" w))))
  (define height : Fixnum
    (let ([h (pict-height bmp)])
      (or (and (fixnum? h) (cast h Fixnum))
          (error "bad height" h))))
  (define pixels (pict->argb-pixels bmp))

  (define-syntax-rule (make-rgb-func [a r g b] body ...)
    (ann (lambda ([xz : XZ])
           (define-values (x z) (xz->values xz))
           (and (ufx> x 0)
                (ufx< x width)
                (ufx> z 0)
                (ufx< z height)
                (let* ([index (ufx+ x (ufx* z width))]
                       [index (ufx* 4 index)]
                       [a (bytes-ref pixels index)]
                       [r (bytes-ref pixels (ufx+ 1 index))]
                       [g (bytes-ref pixels (ufx+ 2 index))]
                       [b (bytes-ref pixels (ufx+ 3 index))])
                  (and (ufx> a 0)
                       body ...))))
         Sampler))

  (define sampler : Sampler
    (case rgb-spec
      [(r) (make-rgb-func [a r g b] r)]
      [(g) (make-rgb-func [a r g b] g)]
      [(b) (make-rgb-func [a r g b] b)]
      [(max) (make-rgb-func [a r g b] (max r g b))]
      [(min) (make-rgb-func [a r g b] (min r g b))]))

  (let* ([sampler (if rgb-invert?
                      (invert sampler)
                      sampler)])
    (let-values ([(inclusive-start inclusive-end sampler)
                  (normalize normalize-spec sampler width height)])
      (let ([sampler (project project-spec sampler inclusive-start inclusive-end)])
        (bitmap-sampler sampler
                        (make-rect (xz 0 0) (xz width height)))))))
