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

{module+ main
  (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")
  (define B01 (load-stage 'IoA 'B01))
  (define platform-block (block 'Seaside-Scene-Block))
  (define the-pict
    (stage->pict B01 (lambda (xz column)
                       (define anything? #f)
                       (for ([y '(51 52 53 54 55 56 57 58 59 60)])
                         (let ([block (vector-ref column y)])
                           (when (and (not (= block 0))
                                      (not (= block platform-block)))
                             (println (list xz y block))
                             (set! anything? #t))))
                       (if anything? #xFFFF0000 0))))
  (send (pict->bitmap the-pict) save-file "OUT.bmp" 'bmp)
  }
