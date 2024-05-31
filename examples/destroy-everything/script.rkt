#lang racket

(require "../../NEW-API.rkt"
         pict
         rackunit)

; Results:
; y > 90 had nothing so I moved the teleportal and Ward of Erdrick up there
;   TODO also stash the Buggy Buggy up there when you get to it
; y = 90 has the 16x16 square for the flag
; y = 74 has the 3x3 hammer (hermit's mountain)
; 53 <= y <= 56 has the blue tablet
; 1:50 PM Saturday - finished OUT50.bmp
; 44 <= y <= 47 has the green tablet near (xz 519 114)
; 43 <= y <= 46 has the red tablet near (xz 553 287)
; ? <= y <= 45 has the boat near (xz 446 513)
;  (note: y=45 has just one spot I think: the tip of the mast)
; 6:10 PM Thursday - finished OUT40.bmp
{module+ main
  (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")
  (define B01 (load-stage 'IoA 'B01))
  (define platform-block (block 'Seaside-Scene-Block))
  (define the-pict
    (stage->pictOLD
     B01 (lambda (xz column)
           (define anything? #f)
           (for ([y '(41 42 43 44 45 46 47 48 49 50)])
             (let ([block (vector-ref column y)])
               (when (and (not (= block 0))
                          (not (= block platform-block)))
                 ;(println (list xz y block))
                 (set! anything? #t))))
           (if anything? #xFFFF0000 0))))
  (send (pict->bitmap the-pict) save-file "OUT.bmp" 'bmp)
  }
