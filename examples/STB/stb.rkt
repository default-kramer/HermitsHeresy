#lang racket

(require "../../NEW-API.rkt"
         (only-in "../../lib-dqb.rkt" block)
         rackunit)

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

(define cs-plateau (bitmap->area "cs-plateau.bmp"))
(define mountain (bitmap->area "mountain.bmp"))

(check-true (area-contains? cs-plateau (xz 300 100)))
(check-false (area-contains? cs-plateau (xz 10 10)))

{begin
  (define B00 (mark-writable (load-stage 'IoA 'B00)))

  (fill-area! B00 cs-plateau (block 'Chert) #:y-min 1 #:y-max 50)
  (clear-area! B00 mountain #:y-min 50)
  (put-hill! B00 mountain (block 'Sand) #:y-min 1 #:y-max 94
             #:step-start 50 #:step-height 4)

  (save-stage! B00)
  }
