#lang typed/racket

; A chunk is a 32x32x96 (X-Z-Y) collection of blocks.
; Each cell holds a block ID; zero indicates vacancy.
; This is how the DQB2 save file structures its own data
; and we use the same idea for convenience and performance.
; We maintain the count of each block ID (masking off chisel status)
; allowing us to skip the entire chunk if we know that it does not contain
; the block we are looking for.
; (Masking the chisel status means we might search a chunk we don't actually
;  need to, but should still be fast enough and saves a ton of memory.)
;
; Be careful not to provide anything that could circumvent the indexing.
(provide Chunk make-empty-chunk load-chunk! unload-chunk!
         chunk-ref chunk-set! chunk-countof)

(require "ufx.rkt")

(struct chunk ([data : Bytes] ; mutable
               [index : (Mutable-Vectorof Fixnum)])
  #:transparent #:type-name Chunk)

(define data-size #x30000) ; 32x32x96 times 2 bytes per block

(define-syntax-rule (data-offset x z y)
  (let ([result (:ufx+ (:ufx* y 32 32 2)
                       (:ufx* z 32 2)
                       (:ufx* x 2))])
    (when (or (ufx< result 0)
              (ufx> result (ufx+ data-size -2)))
      (error "bad chunk coords:" x z y))
    result))

; Mask off chisel status when storing counts of each block.
(define index-size #x800)
(define index-mask #x7FF)
(define-syntax-rule (index-offset block)
  (ufxand block index-mask))

(define-syntax-rule (get-block data offset)
  (let ([lo (bytes-ref data (+ 0 offset))]
        [hi (bytes-ref data (+ 1 offset))])
    (ufxior lo (ufxlshift hi 8))))

(define-syntax-rule (set-block! data offset block)
  (begin (bytes-set! data (ufx+ 0 offset) (ufxand #xFF block))
         (bytes-set! data (ufx+ 1 offset) (ufxand #xFF (ufxrshift block 8)))))

(define (chunk-ref [chunk : Chunk] #:x [x : Fixnum] #:z [z : Fixnum] #:y [y : Fixnum])
  (let* ([data (chunk-data chunk)]
         [offset (data-offset x z y)])
    (get-block data offset)))

(define (chunk-set! [chunk : Chunk] #:x [x : Fixnum] #:z [z : Fixnum] #:y [y : Fixnum] #:block [block : Integer])
  (let* ([data (chunk-data chunk)]
         [offset (data-offset x z y)]
         [old (get-block data offset)]
         [index (chunk-index chunk)]
         [old-key (index-offset old)]
         [new-key (index-offset block)])
    (vector-set! index old-key (ufx+ -1 (vector-ref index old-key)))
    (vector-set! index new-key (ufx+ +1 (vector-ref index new-key)))
    (set-block! data offset block)))

(: chunk-countof (-> Chunk Integer Fixnum))
(define (chunk-countof chunk block)
  (let* ([index (chunk-index chunk)]
         [key (index-offset block)])
    (vector-ref index key)))

(define (make-empty-chunk)
  (let* ([index : (Mutable-Vectorof Fixnum) (make-vector index-size 0)])
    ; The chunk is fully vacant right now. Update the count:
    (vector-set! index 0 (* 96 32 32))
    (chunk (make-bytes data-size 0) index)))

; Unfortunately we can't assume that all chunks will be the usual 0x30000 bytes!
; The last chunk might be shorter than we would like.
; See https://github.com/default-kramer/HermitsHeresy/discussions/7
(define (load-chunk! [chunk : Chunk] [dqb2-bytes : Bytes] [addr : Fixnum] [end-addr : Fixnum])
  (define y : Fixnum 0)
  (define z : Fixnum 0)
  (define x : Fixnum 0)
  (define non-empty-count : Fixnum 0)
  (let loop ([addr addr])
    (let ([block (get-block dqb2-bytes addr)])
      (chunk-set! chunk #:x x #:z z #:y y #:block block)
      (when (not (ufx= 0 block))
        (set! non-empty-count (ufx+ 1 non-empty-count)))
      (set! x (ufx+ 1 x))
      (when (= x 32)
        (set! x 0)
        (set! z (ufx+ 1 z)))
      (when (= z 32)
        (set! z 0)
        (set! y (ufx+ 1 y)))
      (when (ufx< addr (ufx+ -2 end-addr)) ; 2 bytes per block
        (loop (ufx+ 2 addr)))))
  non-empty-count)

(define (unload-chunk! [chunk : Chunk] [dqb2-bytes : Bytes] [addr : Fixnum])
  (for ([y (in-range 96)])
    (for ([z (in-range 32)])
      (for ([x (in-range 32)])
        (let ([block (chunk-ref chunk #:x x #:z z #:y y)])
          (set-block! dqb2-bytes addr block)
          (set! addr (ufx+ 2 addr))))))
  (void))
