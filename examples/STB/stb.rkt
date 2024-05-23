#lang racket

(require "../../NEW-API.rkt"
         (only-in "../../lib-dqb.rkt" block)
         rackunit)

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

(define cs-plateau (bitmap->area "cs-plateau.bmp"))
(define mountain (bitmap->area "mountain.bmp"))
(define sea-repair-area (bitmap->area "sea-repair.bmp"))

(check-true (area-contains? cs-plateau (xz 300 100)))
(check-false (area-contains? cs-plateau (xz 10 10)))

#;{begin
    (define B00 (mark-writable (load-stage 'IoA 'B00)))
    ;(print-column B00 (xz 100 450))
    ;(print-column B00 (xz 101 451))
    ;(put-column! B00 (xz 102 452) 35 (block 'Ice))
    (repair-sea! B00 sea-repair-area)

    #;(put-hill! B00 cs-plateau (block 'Ice) #:y-min 1 #:y-max 50)
    #;(clear-area! B00 mountain #:y-min 50)
    #;(put-hill! B00 mountain (block 'Basalt) #:y-min 1 #:y-max 94
                 #:step-start 50 #:step-height 4)

    (save-stage! B00)
    }
