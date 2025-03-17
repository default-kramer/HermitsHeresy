#lang typed/racket

(provide Chunky (struct-out chunky)
         XZ (struct-out xz) xz->values
         Rect rect? make-rect rect-relative-xz
         rect-start rect-end rect-width rect-height rect-contains?
         rect-intersect rect-union
         for/rect
         Point point? make-point point-y point-x point-z
         Chunk-Layout chunk-translate chunk-count
         simple?
         (struct-out fixnum-sampler) Fixnum-Sampler sampler?
         prop:authentic
         with-absolute-seed
         )

(require "ufx.rkt"
         "simple.rkt")

(module+ test
  (require typed/rackunit))

; Indicates that the contained value (e.g. an XZ or a Point)
; is relative to the chunk-id.
(struct (A) chunky ([chunk-id : Integer]
                    [val : A])
  #:type-name Chunky #:transparent)

(struct xz ([x : Fixnum]
            [z : Fixnum])
  #:type-name XZ #:transparent #:extra-constructor-name make-xz)

(define-syntax-rule (xz->values xz-expr)
  (let ([xz : XZ xz-expr])
    (values (xz-x xz)
            (xz-z xz))))

; OUCH - constructor is now x z y which is confusing!
; Should hide this... use a generic interface? No - use composition instead.
(struct point xz ([y : Fixnum])
  #:type-name Point #:transparent)

(define (make-point [xz : XZ] [y : Fixnum])
  (point (xz-x xz) (xz-z xz) y))

(define point-x xz-x)
(define point-z xz-z)


; Each inner vector is one row, holding chunk IDs from east to west.
; The outer vector contains all rows from north to south.
; A chunk ID of false indicates out-of-bounds.
(define-type Chunk-Layout (Vectorof (Vectorof (U #f Integer))))

(: chunk-translate (-> Chunk-Layout XZ (U #f (Chunky XZ))))
(define (chunk-translate chunk-layout xz)
  (let*-values ([(x-offset x) (quotient/remainder (xz-x xz) 32)]
                [(z-offset z) (quotient/remainder (xz-z xz) 32)])
    (let* ([row (vector-ref chunk-layout z-offset)]
           [chunk-id (vector-ref row x-offset)])
      (and chunk-id
           (chunky chunk-id (make-xz x z))))))

(: chunk-count (-> Chunk-Layout Fixnum))
(define (chunk-count layout)
  (define count : Fixnum 0)
  (for ([row layout])
    (for ([cell row])
      (when cell (set! count (ufx+ 1 count)))))
  count)

; Note: `start is inclusive, `end` is exclusive.
; Otherwise a zero-size rect would need end < start which would be weird.
(struct rect ([start : XZ]
              [end : XZ])
  #:type-name Rect #:transparent)

(define zero-rect (rect (xz 0 0) (xz 0 0)))

(define (make-rect [a : XZ] [b : XZ])
  ; Ensure that `start` is always the upper left and `end` the lower right
  (define-values (x1 z1) (xz->values a))
  (define-values (x2 z2) (xz->values b))
  (rect (xz (min x1 x2) (min z1 z2))
        (xz (max x1 x2) (max z1 z2))))

(define (rect-width [rect : Rect])
  (ufx- (xz-x (rect-end rect))
        (xz-x (rect-start rect))))

(define (rect-height [rect : Rect])
  (ufx- (xz-z (rect-end rect))
        (xz-z (rect-start rect))))

(define (rect-contains? [rect : Rect] [xz : XZ])
  (define-values (x z) (xz->values xz))
  (define-values (x1 z1) (xz->values (rect-start rect)))
  (define-values (x2 z2) (xz->values (rect-end rect)))
  (and (ufx>= x x1)
       (ufx>= z z1)
       (ufx< x x2)
       (ufx< z z2)))

(: rect-relative-xz (-> Rect XZ (U #f XZ)))
; If the given XZ is inside the given rect, returns its offset from the top left corner.
(define (rect-relative-xz rect xz)
  (define-values (x z) (xz->values xz))
  (define-values (x1 z1) (xz->values (rect-start rect)))
  (define-values (x2 z2) (xz->values (rect-end rect)))
  (and (ufx>= x x1)
       (ufx>= z z1)
       (ufx< x x2)
       (ufx< z z2)
       (make-xz (ufx- x x1) (ufx- z z1))))

(define-values (rect-intersect rect-union)
  (let ()
    (define-syntax-rule (find f get-rect get-coord a b)
      (let loop : Fixnum ([val : Fixnum (get-coord (get-rect a))]
                          [rest : (Listof Rect) b])
        (if (empty? rest)
            val
            (loop (f val (get-coord (get-rect (car rest))))
                  (cdr rest)))))

    (: rect-intersect (-> Rect Rect * Rect))
    (define (rect-intersect a . b)
      (let ([start-x (find max rect-start xz-x a b)]
            [start-z (find max rect-start xz-z a b)]
            [end-x (find min rect-end xz-x a b)]
            [end-z (find min rect-end xz-z a b)])
        (cond
          [(or (ufx>= start-x end-x)
               (ufx>= start-z end-z))
           zero-rect]
          [else
           (make-rect (xz start-x start-z)
                      (xz end-x end-z))])))

    (: rect-union (-> Rect Rect * Rect))
    (define (rect-union a . b)
      (let ([start-x (find min rect-start xz-x a b)]
            [start-z (find min rect-start xz-z a b)]
            [end-x (find max rect-end xz-x a b)]
            [end-z (find max rect-end xz-z a b)])
        (make-rect (xz start-x start-z)
                   (xz end-x end-z))))

    (values rect-intersect rect-union)))

{module+ test
  (let ([r1 (make-rect (xz 20 21) (xz 50 41))]
        [r2 (make-rect (xz 30 31) (xz 40 51))]
        [r3 (make-rect (xz 10 11) (xz 35 36))])
    (check-equal? (rect-intersect r1 r2 r3)
                  (make-rect (xz 30 31) (xz 35 36)))
    (check-equal? (rect-union r1 r2 r3)
                  (make-rect (xz 10 11) (xz 50 51))))
  (let* ([r1 (make-rect (xz 110 110) (xz 120 120))]
         [r2 (make-rect (xz 410 410) (xz 420 420))]
         [i (rect-intersect r1 r2)])
    ; The use of 0,0 isn't super important here...
    (check-equal? i (make-rect (xz 0 0) (xz 0 0)))
    ; ... but the fact that it doesn't contain that XZ is important:
    (check-false (rect-contains? i (xz 0 0))))
  }

(define-syntax-rule (for/rect ([#:z z:id #:x x:id #:rect rect]) body ...)
  (let* ([rect-id rect]
         [start (rect-start rect-id)]
         [end (rect-end rect-id)]
         [start-x (xz-x start)]
         [start-z (xz-z start)]
         [end-x (xz-x end)]
         [end-z (xz-z end)])
    (for ([z:id : Fixnum (ufx-in-range start-z end-z)])
      (for ([x:id : Fixnum (ufx-in-range start-x end-x)])
        body ...))))


; Exposing a generic sampler struct to untyped code seems unwise.
; So let's only create non-generic structs.
; And use prop:authentic for extra safety.
(require/typed racket/base [prop:authentic Struct-Type-Property])

(struct fixnum-sampler ([func : (-> XZ (U #f Fixnum))]
                        [bounding-rect : Rect])
  #:type-name Fixnum-Sampler
  #:property prop:authentic #t
  #:transparent)

(define sampler? fixnum-sampler?)


; Maybe someday I will want `with-relative-seed` which would also be affected
; (deterministically of course) by any enclosing uses of `with-*-seed`.
; Hence the name `with-absolute-seed` here.
(define-syntax-rule (with-absolute-seed [seed:expr] body ...)
  (let* ([seed (ann seed:expr Integer)]
         [seed (if (positive? seed)
                   seed
                   (error "seed must be positive, got:" seed))]
         [vec (vector-immutable 1 1 1 1 1 1)]
         [prng (vector->pseudo-random-generator vec)])
    (parameterize ([current-pseudo-random-generator prng])
      (random-seed seed)
      body ...)))

{module+ test
  (when #f
    ; one-time test to make sure that we're getting a reasonably
    ; random-looking distribution
    (let* ([range 256]
           [trials 987654]
           [counts (make-vector range 0)])
      (for ([i (in-range 1 trials)])
        (with-absolute-seed [i]
          (let ([idx (random range)])
            (vector-set! counts idx (+ 1 (vector-ref counts idx))))))
      (let ([lo (apply min (vector->list counts))]
            [hi (apply max (vector->list counts))])
        (println (list "min:" lo "max:" hi "should surround:" (/ trials range))))))

  ; Any of these failing would indicate a breaking change:
  (check-equal? (with-absolute-seed [3] (random 100))
                94)
  (check-equal? (with-absolute-seed [88112233] (random 100))
                41)
  (check-equal? (with-absolute-seed [7654321] (random 100))
                9)
  ; min seed:
  (check-equal? (with-absolute-seed [1] (random 100))
                50)
  ; max seed:
  (check-equal? (with-absolute-seed [2147483647] (random 100))
                48)
  }
