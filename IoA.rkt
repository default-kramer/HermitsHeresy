#lang typed/racket

(require typed/pict)

; Indicates that the contained value (e.g. an XZ or a Point)
; is relative to the chunk-id.
(struct (A) chunky ([chunk-id : Integer]
                    [val : A])
  #:type-name Chunky #:transparent)

(struct xz ([x : Integer]
            [z : Integer])
  #:type-name XZ #:transparent)

#;(struct point xz ([y : Integer])
    #:type-name Point #:transparent)

(define (parse-map [rows : (Listof (Listof (U '_ 'X)))])
  (let ([chunk-id -1])
    (for/vector : (Vectorof (Vectorof (U #f Integer)))
      ([row rows])
      (for/vector : (Vectorof (U #f Integer))
        ([cell row])
        (case cell
          [(_) #f]
          [(X) (begin (set! chunk-id (+ 1 chunk-id))
                      chunk-id)]
          [else (error "assert fail")])))))

(define chunk-id-array
  (parse-map '((_ _ _ _ _ _ _ X X X X X X X X _ _ _ X X X _ _ _ _ _ _)
               (_ _ _ _ _ _ X X X X X X X X X X X X X X X X X _ _ _ _)
               (_ _ _ _ X X X X X X X X X X X X X X X X X X X _ _ _ _)
               (_ _ _ X X X X X X X X X X X X X X X X X X X X _ _ _ _)
               (_ _ X X X X X X X X X X X X X X X X X X X X X X X X _)
               (_ X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (_ X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X X)
               (X X X X X X X X X X X X X X X X X X X X X X X X X X _)
               (_ X X X X X X X X X X X X X X X X X X X X X X X _ _ _)
               (_ _ X X X X X X X X X X X X X X X X X X X X X _ _ _ _)
               (_ _ X X X X X X X X X X X X X X X X X X X _ _ _ _ _ _)
               (_ _ _ _ X X X X X X X X X X X X X X X _ _ _ _ _ _ _ _)
               (_ _ _ _ _ _ _ _ _ X X X X X X _ _ _ _ _ _ _ _ _ _ _ _)
               (_ _ _ _ _ _ _ _ _ X X X X X _ _ _ _ _ _ _ _ _ _ _ _ _)
               (_ _ _ _ _ _ _ _ _ _ X X X _ _ _ _ _ _ _ _ _ _ _ _ _ _))))

; Total width/depth of the island. Not all coordinates are usable.
(define x-end : Nonnegative-Integer (* 32 (vector-length (vector-ref chunk-id-array 0))))
(define z-end : Nonnegative-Integer (* 32 (vector-length chunk-id-array)))

(: xz->chunky (-> XZ (U #f (Chunky XZ))))
(define (xz->chunky _xz)
  (let ([x (xz-x _xz)]
        [z (xz-z _xz)])
    (and (>= x 0)
         (>= z 0)
         (< x x-end)
         (< z z-end)
         (let*-values ([(x-offset x) (quotient/remainder x 32)]
                       [(z-offset z) (quotient/remainder z 32)])
           (let* ([row (vector-ref chunk-id-array z-offset)]
                  [chunk-id (vector-ref row x-offset)])
             (and chunk-id
                  (chunky chunk-id (xz x z))))))))

(define (create-map-pict)
  (let* ([bytes-length (* x-end z-end 4)] ; 4 bytes per pixel
         [bytes (make-bytes bytes-length)]
         [index 0])
    (for ([z (in-range z-end)])
      (for ([x (in-range x-end)])
        (define grayscale (if (xz->chunky (xz x z))
                              255
                              100))
        (bytes-set! bytes (+ 0 index) grayscale)
        (bytes-set! bytes (+ 1 index) grayscale)
        (bytes-set! bytes (+ 2 index) grayscale)
        (bytes-set! bytes (+ 3 index) grayscale)
        (set! index (+ 4 index))))
    (argb-pixels->pict bytes x-end)))

; Save empty map to current directory
#;{module+ main
    (let* ([bmp (pict->bitmap (create-map-pict) 'unsmoothed)]
           [filename "map.bmp"]
           [quality 100])
      (send bmp save-file filename 'bmp quality))
    }

{module+ main
  (require "lib-dqb.rkt"
           (only-in "lib.rkt" point))
  (define bmp (bitmap "test.bmp"))
  (define pixels (pict->argb-pixels bmp))  
  #;(begin
      (define B00 (open-stgdat 'IoA (string->path "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/B00/STGDAT01.BIN")))
      (clear-map! B00 #:above-y 45 #:keep-items? #f)
      (let ([index 0])
        (for ([z (in-range z-end)])
          (for ([x (in-range x-end)])
            (when (= 0 (bytes-ref pixels (+ 3 index)))
              (when (not (xz->chunky (xz x z)))
                (error "Out of bounds!" x z))
              (for ([y (in-range 1 70)])
                (let ([point (point x y z)])
                  (or (put-block! B00 point (block 'Ice))
                      (begin (println point)
                             (error "WTF"))))))
            (set! index (+ 4 index)))))
      (save-stgdat! B00)
      )}
