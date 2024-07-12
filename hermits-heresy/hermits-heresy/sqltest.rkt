#lang racket

; Prove that stage->pict is much faster using SQLite

(require db "ufx.rkt" pict)

(define conn (sqlite3-connect #:database "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/.hermits-heresy/test.db"))

(define (make-pict rows)
  (define-values (width depth) (values 864 608))
  (define bytes-per-pixel 4)
  (define pict-bytes (make-bytes (* bytes-per-pixel width depth)))
  (for ([row rows])
    (let* ([x (vector-ref row 0)]
           [z (vector-ref row 1)]
           [argb #xFF0000FF]
           [index (ufx* bytes-per-pixel (ufx+ x (ufx* z width)))])
      (bytes-set! pict-bytes (ufx+ 0 index) (bitwise-bit-field argb 24 32))
      (bytes-set! pict-bytes (ufx+ 1 index) (bitwise-bit-field argb 16 24))
      (bytes-set! pict-bytes (ufx+ 2 index) (bitwise-bit-field argb 08 16))
      (bytes-set! pict-bytes (ufx+ 3 index) (bitwise-bit-field argb 00 08))))
  (argb-pixels->pict pict-bytes width))

(define (GO)
  (define rows (query-rows conn "select distinct X,Z from Cell where BlockVal in (2764, 716) order by X, Z;"))
  (make-pict rows))
