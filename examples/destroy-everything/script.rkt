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
;
; Resuming work starting with the out40/pass40 file.
; Ran this to remove all sea assuming all sea can be removed:
#;(begin
    (remove-blocks!
     B00 'all
     (let ([top-sea #x1A4] ; The shallow sea, placed at y = sea level
           [full-sea #x155]) ; Full sea, placed at y < sea level
       ; Looks like underwater/falling water is also a block?
       ; I don't really understand what's going on here...
       ; This probably is not the complete list, but it works well enough
       ; for the destroy-everything project:
       (list top-sea
             #x14D #x14E #x14F
             #x150 #x151 #x152 #x153 #x154 full-sea #x15D)))
    ; 1736811 blocks were removed
    (save-stage! B00))
; And I don't think there's any need to create platforms anymore.
;
; Starting with OUT30.bmp, the water is going to leak in and I don't have a good way to deal with that.
; So I'm just going to proceed, and I think when I reach the end I'll be able to easily exclude the
; outskirts using image editing and assume that all the outskirts are destructible (using the
; trowel-to-Shifting-Sands trick).
; So anything else will be considered indestructible.
;
; After pass20, I ran this to remove the platforms:
#;(let ([bid (block 'Seaside-Scene-Block)])
    (remove-blocks! stage 'all (list bid (bitwise-and bid #x7FF))))
; It removed 929078 blocks.

{module+ main
  (save-dir "C:/Users/defau/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")
  (define stage (load-stage 'IoA 'B01))
  (define platform-block (block 'Seaside-Scene-Block))

  (define the-pict
    (stage->pictOLD
     stage (lambda (xz column)
             (define anything? #f)
             (for ([y '(11 12 13 14 15 16 17 18 19 20)])
               (let ([block (vector-ref column y)])
                 (when (and (not (= block 0))
                            (not (= block platform-block)))
                   ;(println (list xz y block))
                   (set! anything? #t))))
             (if anything? #xFFFF0000 0))))
  (send (pict->bitmap the-pict) save-file "OUT.bmp" 'bmp)
  }
