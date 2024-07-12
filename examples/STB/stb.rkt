#lang racket

(require hermits-heresy
         pict
         rackunit)

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

(define cs-plateau (bitmap->area "cs-plateau.bmp"))
(define mountain (bitmap->area "mountain.bmp"))
(define evil (bitmap->area "evil.bmp"))

; bumps and bumps2 are basically the same thing, but with different randomness
; courtesy of the Crystallize effect. Use one with Chunky Chert and the other
; with Chert to get a nice blend. (Well, almost. Walls will be entirely
; one block or the other, which doesn't look great with steep walls but would
; probably look fine with more gradual slopes.)
; I'm really enjoying this workflow. Here's how to do the Paint.NET part:
; * Hide all layers except bumps. Increase bumps opacity to fully opaque.
; * Save as bitmap file.
; * Paint background white (to avoid opacity during blur)
; * Gaussian Blur, radius=12
; * Crystallize, cell size=3
; * Use magic wand (tolerance about 40% seems good) to delete white background
(define bumps (bitmap->hill "bumps.bmp"))
(define bumps2 (bitmap->hill "bumps2.bmp"))

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

(define-syntax-rule (with-absolute-seed seed body ...)
  (parameterize ([current-pseudo-random-generator
                  (vector->pseudo-random-generator
                   (vector 42 42 seed 42 42 42))])
    body ...))

#;{begin ;module+ main
    (copy-everything! #:from 'B02 #:to 'B00)
    (define B00 (mark-writable (load-stage 'IoA 'B00)))
    (println "loaded stage")
    (update-manual-build-pict B00 "manual-build.bmp")
    (define manual-build (bitmap->area "manual-build.bmp"))

    ;(clear-area! B00 'all #:keep-items? #f)
    ;(repair-sea! B00 'all)

    ; It seems better *not* to allow the user to customize the (-> ARGB y-elevation) function...
    ; Instead the implementation uses this convention:
    #;(let ([y (- 95 (quotient (max r g b) 2))]) (blah ...))
    ; Doing it this way pushes the work to the image editor, which is the best place to handle it.
    ; So if you want to raise some plateau by N blocks, you just decrease (darken) the color by N*2.
    (with-protected-areas [manual-build]
      (put-hill! B00 (area->hill2 evil bumps) 2065 ; peat
                 #;(lambda (x) (+ 36 (* 21 x))))
      (put-hill! B00 (area->hill2 cs-plateau bumps) (block 'Snow)
                 #;(lambda (x) (+ 52 (* 24 x))))
      (put-hill! B00 (area->hill2 mountain bumps2) (block 'Chunky-Chert)
                 #;(lambda (x) (+ 48 (* 40 x))))
      (put-hill! B00 (area->hill2 mountain bumps) (block 'Chert)
                 #;(lambda (x) (+ 48 (* 40 x))))
      (with-absolute-seed 223344
        (decorate-peaks! B00 mountain
                         (lambda (xz below)
                           (if (not (simple? below))
                               0
                               (case (random 7)
                                 [(0 1) 0] ; vacant
                                 [(2 3) (chisel (block 'Snow) 'flat-lo)]
                                 [(4 5) 18] ; snow cover
                                 [(6) (block 'Snow)])))))
      )

    ;(save-stage! B00)
    }
