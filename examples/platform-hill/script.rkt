#lang racket

(require hermits-heresy
         (submod hermits-heresy undocumented)
         hermits-heresy/hill2
         (only-in hermits-heresy/basics xz-x xz-z)
         (only-in hermits-heresy/chunky-area build-chunky-area))

(define HH 35)
(define WW 300)
(define ph (generate-platform-hill WW HH))
(define phbytes (platform-hill-array2d ph))

(define (build-area val)
  (define dx 90)
  (define dz 80)
  (build-chunky-area
   (+ WW dx)
   (+ HH dz)
   (lambda (xz)
     (let* ([x (xz-x xz)]
            [z (xz-z xz)]
            [x (- x dx)]
            [z (- z dz)]
            [idx (+ x (* WW z))])
       (and (>= x 0)
            (< x WW)
            (>= z 0)
            (< z HH)
            (= val (bytes-ref phbytes idx)))))
   (lambda args (void))))


; crazy leakage of implementation details here:
(define peak-border (build-area 7))
(define peak (build-area 6))
(define tall-border (build-area 5))
(define tall (build-area 4))
(define short-border (build-area 3))
(define short (build-area 2))


{begin
  (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")
  (define source-slot 'B02)
  (define dest-slot 'B00)

  (copy-all-save-files! #:from source-slot #:to dest-slot)

  (define stage (load-stage 'BT1 dest-slot))

  (define ypeak 40)
  (define ypeak-border (+ ypeak -1))
  (define ytall (+ ypeak -4))
  (define ytall-border (+ ytall -1))
  (define yshort (+ ypeak -6))
  (define yshort-border (+ yshort -1))

  (define trav!
    (traversal
     (cond
       [(and (in-area? peak)
             (< YYY ypeak))
        (set-block! 'Moss)]
       [(and (in-area? peak-border)
             (< YYY ypeak))
        (set-block! 'Chert)
        (when (= YYY ypeak-border)
          (set-chisel! 'flat-lo))]
       [(and (in-area? tall)
             (< YYY ytall))
        (set-block! 'Moss)]
       [(and (in-area? tall-border)
             (< YYY ytall))
        (set-block! 'Chert)
        (when (= YYY ytall-border)
          (set-chisel! 'flat-lo))]
       [(and (in-area? short)
             (< YYY yshort))
        (set-block! 'Moss)]
       [(and (in-area? short-border)
             (< YYY yshort))
        (set-block! 'Chert)
        (when (= YYY yshort-border)
          (set-chisel! 'flat-lo))]
       )))

  (traverse stage trav! #:force-unsound-optimization? #t)

  (save-stage! stage)
  }
