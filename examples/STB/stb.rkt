#lang racket

(require "../../NEW-API.rkt"
         ;(submod "../../NEW-API.rkt" sqlite)
         pict
         rackunit)

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

; How export hills from Paint.NET:
; Disable opacity on all relevant layers, and hide irrelevant layers.
; Save as .bmp, requires flattening.
; Replace paint all transparent pixels white to avoid blurring.
; Gaussian blur with radius=6.
; Crystallize with cell size=3.
; Use magic wand with tolerance ~15% to delete white pixels
;  -- (TODO... this step might be too variable)
; Save as .bmp again.
; WARNING - Don't accidentally apply blur to transparency, as the edges will look wrong.
;
; == UPDATE ==
; Using a single dark color and a Gaussian blur with radius 12 is simpler
; and maybe even looks better...
; TODO it would be much more convenient to have a separate "bumps" layer in which you
; draw the hills for *all* the plateaus. Then exporting the plateaus is dead simple
; (no elevation needed) and you only need to blur+crystallize the hills layer by itself.
; Then I need something like `(hill-adjust the-hill the-bumps #:max 20)`
; which would return a new hill:
; * having the exact same area and xzs as `the-hill`
; * having added anywhere from 0 to 20 based on the intersection with `the-bumps`
; And that should simplify my workflow a lot.

(define cs-plateau (bitmap->area "cs-plateau.bmp"))
(define mountain (bitmap->area "mountain.bmp"))
(define evil (bitmap->area "evil.bmp"))
(define bumps (bitmap->hill "bumps.bmp"))

(define (update-manual-build-pict stage filename)
  (define the-pict
    (stage->pict stage #hash((2764 . #xFF0000FF) ; seaweed (normal and troweled)
                             (716 . #xFF0000FF))))
  (send (pict->bitmap the-pict) save-file filename 'bmp)
  (println (format "updated: ~a" filename))
  (void))

; Pattern matching idea? (Is this easier in SQLite?)
; With this idea, it's easy to recommend "start your patterns with the rarest block for best speed."
; So in this example using #:upwards is better than #:downwards.
#;(from stage
        [(pattern (column #:upwards
                          ; each id will always be bound to a list on a match?
                          [start (one 'Seaweed-Styled-Block)]
                          [fill (greedy (any 'Seaside-Scene-Block 'vacant))]
                          [end (one 'Seaside-Scene-block)]))
         (println (list "blah" fill))
         (for ([point fill])
           (stage-write! stage point (block 'Seaside-Scene-Block)))]
        [(pattern blah ...)
         (void "do something")])

(define-syntax-rule (with-protected-areas [area ...] body ...)
  (parameterize ([protected-areas (append (list area ...)
                                          (protected-areas))])
    body ...))

#;{begin ;module+ main
    (copy-everything! #:from 'B02 #:to 'B00)
    (define B00 (mark-writable (load-stage 'IoA 'B00)))
    (println "loaded stage")
    (update-manual-build-pict B00 "manual-build.bmp")
    (define manual-build (bitmap->area "manual-build.bmp"))

    ;(clear-area! B00 'all #:keep-items? #f)
    ;(repair-sea! B00 'all)

    (with-protected-areas [manual-build]
      (put-hill! B00 (area->hill2 evil bumps) 2065 ; peat
                 (lambda (x) (+ 36 (* 21 x))))
      (put-hill! B00 (area->hill2 cs-plateau bumps) (block 'Ice)
                 (lambda (x) (+ 46 (* 30 x))))
      (put-hill! B00 (area->hill2 mountain bumps) (block 'Chert)
                 (lambda (x) (+ 48 (* 40 x))))
      )

    ;(save-stage! B00)
    }
