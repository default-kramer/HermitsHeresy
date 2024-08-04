#lang typed/racket

(provide Chunky-Area chunky-area?
         bitmap->chunky-area
         chunky-area-contains?
         chunky-area-bounds
         empty-chunky-area
         )

(require typed/pict
         (only-in typed/racket/draw Bitmap%)
         "basics.rkt"
         "ufx.rkt")

(define bytes-per-chunk (ufxquotient (ufx* 32 32) 8))

; The bytes should be immutable, but there is no Immutable-Bytes type.
; So just be careful!
;
; Note: This is agnostic of any stage's Chunk-Layout; it simply borrows
; the concept of 32x32 chunks which will have similar performance benefits.
; But don't assume that any chunk-id will match a chunk-id relative to a "real" Chunk-Layout.
;
; Note: This might be a questionable optimization. I measured that it performed better
; than a HashTable. Then after some more time I realized that most call sites should only test
; the XZ coordinate once, not 96 times for each Y, duh!
; But faster is better so I'm keeping it.
(struct chunky-area ([bytevec : (Immutable-Vectorof Bytes)]
                     [W32 : Fixnum] ; number of chunks wide
                     [bounds : Rect])
  #:transparent #:type-name Chunky-Area)

(define (translate [W32 : Fixnum] [bytevec : VectorTop] [xz-in : XZ])
  (define-values (x z) (xz->values xz-in))
  (define chunk-num (ufx+ (ufx* W32 (ufxquotient z 32))
                          (ufxquotient x 32)))
  (and (>= chunk-num 0)
       (< chunk-num (vector-length bytevec))
       (chunky chunk-num (xz (ufxmodulo x 32)
                             (ufxmodulo z 32)))))

(define empty-bytes (make-bytes bytes-per-chunk 0))
(define full-bytes (make-bytes bytes-per-chunk 255))

(define empty-chunky-area (chunky-area (vector->immutable-vector (make-vector 0 empty-bytes))
                                       0
                                       (rect (xz 0 0) (xz 0 0))))

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

(: bitmap->chunky-area (-> (U (Instance Bitmap%) Path-String) Chunky-Area))
(define (bitmap->chunky-area arg)
  (define bmp (bitmap arg))
  (define width : Fixnum
    (let ([w (pict-width bmp)])
      (or (and (fixnum? w) (cast w Fixnum))
          (error "bad width" w))))
  (define widthX4 (ufx* 4 width))
  (define depth : Fixnum
    (let ([h (pict-height bmp)])
      (or (and (fixnum? h) (cast h Fixnum))
          (error "bad height" h))))
  (define pixels (pict->argb-pixels bmp))
  (define all-empty? : Boolean #t)
  (define all-full? : Boolean #t)
  (define W32 (ufx+ 1 (ufxquotient width 32)))
  (define H32 (ufx+ 1 (ufxquotient depth 32)))
  (define vec-length (ufx* W32 H32))
  (define bytevec : (Vectorof Bytes)
    (make-vector vec-length empty-bytes))
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
                (define pixel-index (ufx+ (ufx* widthX4 z)
                                          (ufx* 4 x)))
                (define alpha (bytes-ref pixels pixel-index))
                (cond
                  [(> alpha 0)
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

  (when (or all-empty? all-full?)
    (error (format "Expected some fully-transparent pixels and some other pixels, but ~a pixels are fully-transparent."
                   (if all-empty? "all" "zero"))))

  (chunky-area (vector->immutable-vector bytevec)
               W32
               (rect (xz min-x min-z)
                     (xz max-x max-z))))
