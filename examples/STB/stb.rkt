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

(define cs-plateau (bitmap->hill "cs-plateau.bmp"))
(define mountain (bitmap->hill "mountain.bmp"))
(define evil (bitmap->hill "evil.bmp"))

(define (update-manual-build-pict stage blockids filename)
  (define (matches? block) (member block blockids))
  (define the-pict
    (stage->pict stage (lambda (xz column)
                         (if (ormap matches? (vector->list column))
                             #xFF0000FF
                             0))))
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
    ;(update-manual-build-pict B00 '(2764 716) "manual-build.bmp") ; seaweed, TODO this confirms trowel hunch!
    ;(define manual-build (bitmap->area "manual-build.bmp"))

    (clear-area! B00 'all #:keep-items? #f)
    (repair-sea! B00 'all)

    ;(with-protected-areas [manual-build]
    (put-hill! B00 evil 2065 ; peat
               (lambda (x) (+ 30 (* 21 x))))
    (put-hill! B00 cs-plateau (block 'Ice)
               (lambda (x) (+ 38 (* 34 x))))
    (put-hill! B00 mountain (block 'Chert)
               (lambda (x) (+ 33 (* 55 x))))
    ;)

    ;(save-stage! B00)
    }
