#lang typed/racket

(provide Chunky-Area chunky-area?
         bitmap->chunky-area
         chunky-area-contains?
         chunky-area-bounds
         empty-chunky-area
         chunky-area-transform
         )

(require typed/pict
         (only-in typed/racket/draw Bitmap% read-bitmap)
         typed/racket/unsafe
         "basics.rkt"
         "transformer2d.rkt"
         "ufx.rkt")

(unsafe-require/typed
 racket/struct
 [make-constructor-style-printer (-> Any Any (-> Any Any Any Any))])

(define print-chunky-area
  (let ([printer (make-constructor-style-printer
                  (lambda (obj) '<area>)
                  (lambda ([obj : Chunky-Area])
                    (list (chunky-area-xz-count obj) (chunky-area-bounds obj))))])
    (lambda ([area : Chunky-Area] [port : Any] [mode : Any])
      (printer area port mode))))

(define bytes-per-chunk (ufxquotient (ufx* 32 32) 8))

; The bytes should be immutable, but there is no Immutable-Bytes type.
; So just be careful!
;
; Note: This is agnostic of any stage's Chunk-Layout; it simply borrows
; the concept of 32x32 chunks which will have similar performance benefits.
; But don't assume that any chunk-id will match a chunk-id relative to a "real" Chunk-Layout.
; (Wait, why not? Because)
; The bytevec is relative to a 0,0 top left corner and W32;
; the bounds is irrelevant to finding which bit corresponds to a given coordinate.
; This means that translating an area southeast will likely require a larger bytevec
; to represent the new area even though it is the same size. (This is "likely", not certain,
; because a small translation could be contained by the same chunks as the original.)
;
; Note: This might be a questionable optimization. I measured that it performed better
; than a HashTable. Then after some more time I realized that most call sites should only test
; the XZ coordinate once, not 96 times for each Y, duh!
; But faster is better so I'm keeping it.
(struct chunky-area ([bytevec : (Immutable-Vectorof Bytes)]
                     [W32 : Fixnum] ; number of chunks wide
                     [bounds : Rect]
                     [xz-count : Fixnum])
  #:transparent #:type-name Chunky-Area
  #:property prop:custom-write print-chunky-area)

(define (translate [W32 : Fixnum] [bytevec : VectorTop] [xz-in : XZ])
  (define-values (x z) (xz->values xz-in))
  (define chunk-num (ufx+ (ufx* W32 (ufxquotient z 32))
                          (ufxquotient x 32)))
  ; Negative x or z is always out of bounds!
  (and (ufx>= x 0)
       (ufx>= z 0)
       (ufx< chunk-num (vector-length bytevec))
       (chunky chunk-num (xz (ufxmodulo x 32)
                             (ufxmodulo z 32)))))

(define empty-bytes (make-bytes bytes-per-chunk 0))
(define full-bytes (make-bytes bytes-per-chunk 255))

(define empty-chunky-area (chunky-area (vector->immutable-vector (make-vector 0 empty-bytes))
                                       0
                                       (make-rect (xz 0 0) (xz 0 0))
                                       0))

(define (deduplicate [bytes : Bytes])
  ; save some memory
  (cond
    [(equal? bytes empty-bytes) empty-bytes]
    [(equal? bytes full-bytes) full-bytes]
    [else bytes]))

(define-syntax-rule (index+mask coord-expr)
  (let* ([coord : (Chunky XZ) coord-expr]
         [xz : XZ (chunky-val coord)]
         [x (xz-x xz)]
         [z (xz-z xz)]
         [total (ufx+ z (ufx* x 32))])
    (values (ufxquotient total 8)
            (ufxlshift 1 (ufxmodulo total 8)))))

(: chunky-area-contains? (-> Chunky-Area XZ Boolean))
(define (chunky-area-contains? area xz)
  (define bytevec (chunky-area-bytevec area))
  (define coord (translate (chunky-area-W32 area) bytevec xz))
  (and coord
       (let ([bytes (vector-ref bytevec (chunky-chunk-id coord))])
         (define-values (index mask) (index+mask coord))
         (ufx= mask (ufxand mask (bytes-ref bytes index))))))

(: chunk-layout->chunky-area (-> Chunk-Layout Chunky-Area))
(define (chunk-layout->chunky-area layout)
  (define W32 (vector-length (vector-ref layout 0)))
  (define H32 (vector-length layout))
  (define bytevec : (Vectorof Bytes)
    (make-vector (ufx* W32 H32) empty-bytes))
  (define i : Fixnum 0)
  (define xz-count : Fixnum 0)
  (for ([z32 (in-range H32)])
    (define row (vector-ref layout z32))
    (for ([x32 (in-range W32)])
      (when (vector-ref row x32)
        (vector-set! bytevec i full-bytes)
        (set! xz-count (ufx+ xz-count (ufx* 32 32))))
      (set! i (ufx+ 1 i))))
  (define max-x (ufx* 32 W32))
  (define max-z (ufx* 32 H32))
  (chunky-area (vector->immutable-vector bytevec)
               W32
               (make-rect (xz 0 0)
                          (xz max-x max-z))
               xz-count))

(: build-chunky-area (-> Fixnum Fixnum
                         (-> XZ Any) ; in-area?
                         (-> Any Any Any) ; all-empty? all-full? -> any
                         Chunky-Area))
(define (build-chunky-area width depth in-area? on-done-callback)
  (define all-empty? : Boolean #t)
  (define all-full? : Boolean #t)
  (define W32 (ufx+ 1 (ufxquotient width 32)))
  (define H32 (ufx+ 1 (ufxquotient depth 32)))
  (define vec-length (ufx* W32 H32))
  (define bytevec : (Vectorof Bytes)
    (make-vector vec-length empty-bytes))
  (define xz-count : Fixnum 0)
  ; Find bounds
  (define min-x : Fixnum (ufx+ 1 width))
  (define max-x : Fixnum -1)
  (define min-z : Fixnum (ufx+ 1 depth))
  (define max-z : Fixnum -1)

  (let loopz ([z-start : Fixnum 0])
    (cond
      [(ufx>= z-start depth) #f]
      [else
       (let loopx ([x-start : Fixnum 0])
         (define chunky-xz (translate W32 bytevec (xz x-start z-start)))
         (cond
           [(ufx>= x-start width) #f]
           [(not chunky-xz) #f]
           [else
            (define chunk-id (chunky-chunk-id chunky-xz))
            (define temp-bytes (make-bytes bytes-per-chunk 0))
            (for ([fine-z (ufx-in-range 32)])
              (define z (ufx+ z-start fine-z))
              (for ([fine-x (ufx-in-range 32)])
                (define x (ufx+ x-start fine-x))
                (cond
                  [(ufx>= x width) #f]
                  [(ufx>= z depth) #f]
                  [(in-area? (xz x z))
                   (set! xz-count (ufx+ 1 xz-count))
                   (set! all-empty? #f)
                   (set! min-x (min min-x x))
                   (set! max-x (max max-x x))
                   (set! min-z (min min-z z))
                   (set! max-z (max max-z z))
                   (define-values (index mask)
                     (index+mask (chunky chunk-id (xz fine-x fine-z))))
                   (bytes-set! temp-bytes index (ufxior mask (bytes-ref temp-bytes index)))]
                  [else
                   (set! all-full? #f)])))
            (vector-set! bytevec chunk-id (deduplicate temp-bytes))
            (loopx (ufx+ 32 x-start))]))
       (loopz (ufx+ 32 z-start))]))

  (on-done-callback all-empty? all-full?)
  (chunky-area (vector->immutable-vector bytevec)
               W32
               (make-rect (xz min-x min-z)
                          (xz max-x max-z))
               xz-count))

(: bitmap->chunky-area (-> (U (Instance Bitmap%) Path-String) Chunky-Area))
(define (bitmap->chunky-area arg)
  (define bmp
    ; Don't pass a path-string into `bitmap` as it will return a pict
    ; containing an error message! Use read-bitmap instead.
    (if (path-string? arg)
        (bitmap (read-bitmap arg))
        (bitmap arg)))
  (define width : Fixnum
    (let ([w (pict-width bmp)])
      (or (and (fixnum? w) (cast w Fixnum))
          (error "bad width" w))))
  (define depth : Fixnum
    (let ([h (pict-height bmp)])
      (or (and (fixnum? h) (cast h Fixnum))
          (error "bad height" h))))
  (define widthX4 (ufx* 4 width))
  (define pixels (pict->argb-pixels bmp))

  (define (in-area? [xz : XZ])
    (define-values (x z) (xz->values xz))
    (define pixel-index (ufx+ (ufx* widthX4 z)
                              (ufx* 4 x)))
    (define alpha (bytes-ref pixels pixel-index))
    (> alpha 0))

  (define (on-done all-empty? all-full?)
    (when (or all-empty? all-full?)
      (error (format "Expected some fully-transparent pixels and some other pixels, but ~a pixels are fully-transparent."
                     (if all-empty? "all" "zero")))))

  (build-chunky-area width depth in-area? on-done))


(: chunky-area-transform (-> Chunky-Area Transformer2D Rect Chunky-Area))
; This function might not be wise... it may place to much burder on the caller
; to get the `transformer` and `new-bounds` combination correct.
(define (chunky-area-transform area transformer new-bounds)
  (define-values (width depth)
    (xz->values (rect-end new-bounds)))
  (define orig-bounds (chunky-area-bounds area))

  ; These are needed to apply the transformer:
  (define W (rect-width orig-bounds))
  (define H (rect-height orig-bounds))
  (define-values (orig-x-offset orig-z-offset)
    (xz->values (rect-start orig-bounds)))
  (define-values (new-x-offset new-z-offset)
    (xz->values (rect-start new-bounds)))

  (define (in-area? [coord : XZ])
    (and (rect-contains? new-bounds coord)
         (let*-values ([(x z) (xz->values coord)]
                       ; translate from new space to 0,0
                       [(x) (ufx- x new-x-offset)]
                       [(z) (ufx- z new-z-offset)]
                       ; apply transformer
                       [(x z) (transformer x z W H)]
                       ; translate from 0,0 to old space
                       [(x) (ufx+ x orig-x-offset)]
                       [(z) (ufx+ z orig-z-offset)])
           (chunky-area-contains? area (xz x z)))))

  (define (on-done all-empty? all-full?)
    (println (list "TODO empty/full flags:" all-empty? all-full?)))

  (build-chunky-area width depth in-area? on-done))
