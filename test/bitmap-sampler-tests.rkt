#lang racket

(require hermits-heresy
         (only-in hermits-heresy/basics
                  fixnum-sampler-func fixnum-sampler-bounding-rect
                  make-rect xz)
         rackunit)

; check error message when the file does not exist:
(check-exn
 #rx"open-input-file: cannot open input file"
 (lambda () (bitmap-sampler "does-not-exist.bmp" #:rgb 'g #:project '[0])))

; check error message when the #:project list is the wrong size:
(check-exn
 #rx"The #:project list should have 4 elements, but got: '\\(1 2 3\\)"
 (lambda () (bitmap-sampler "images/corners.bmp"
                            #:rgb 'g
                            #:normalize '[0 .. N-1]
                            #:project '[1 2 3])))

(check-exn
 #rx"The #:project list should have 4 elements, but got: '\\(1 2 3 4 5\\)"
 (lambda () (bitmap-sampler "images/corners.bmp"
                            #:rgb 'g
                            #:normalize '[0 .. N-1]
                            #:project '[1 2 3 4 5])))

; check the samples for a simple test image
(let* ([bs (bitmap-sampler "images/corners.bmp"
                           #:rgb 'g
                           #:normalize '[0 .. N-1]
                           #:project '[10 11 12 13])]
       [sample (fixnum-sampler-func bs)])
  (check-equal? (fixnum-sampler-bounding-rect bs)
                (make-rect (xz 0 0) (xz 4 4)))
  (define-syntax-rule (check-samples [x z expect] ...)
    (begin (check-equal? (sample (xz x z)) expect)
           ...))
  (check-samples
   ; upper left
   [0 0 13]
   [1 0 13]
   [0 1 13]
   [1 1 #f]
   ; upper right
   [2 0 12]
   [3 0 12]
   [2 1 #f]
   [3 1 12]
   ; lower right
   [2 2 #f]
   [3 2 11]
   [2 3 11]
   [3 3 11]
   ; lower left
   [0 2 10]
   [1 2 #f]
   [0 3 10]
   [1 3 10]
   ; out of bounds
   [-1 1 #f]
   [1 -1 #f]
   [1 4 #f]
   [4 1 #f]))
