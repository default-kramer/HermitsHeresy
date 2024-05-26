#lang racket

(require "../../NEW-API.rkt"
         pict
         rackunit)

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

(define cs-plateau (bitmap->area "cs-plateau.bmp"))
(define mountain (bitmap->area "mountain.bmp"))
(define manual-build (bitmap->area "manual-build.bmp"))

(check-true (area-contains? cs-plateau (xz 300 100)))
(check-false (area-contains? cs-plateau (xz 10 10)))

(define-syntax-rule (with-protected-areas [area ...] body ...)
  (parameterize ([protected-areas (append (list area ...)
                                          (protected-areas))])
    body ...))

{module+ main
  (copy-everything! #:from 'B02 #:to 'B00)
  (define B00 (mark-writable (load-stage 'IoA 'B00)))

  (define manual-floor-block (block 'Seaside-Scene-Block))
  (define (update-manual-build-pict)
    (define (seaside-scene? block) (= block manual-floor-block))
    (define seaside-pict
      (stage->pict B00 (lambda (xz column)
                         (if (ormap seaside-scene? (vector->list column))
                             #xFF0000FF
                             0))))
    (send (pict->bitmap seaside-pict) save-file "manual-build.bmp" 'bmp))
  #;(update-manual-build-pict)

  #;(TODO B00 manual-build manual-floor-block (block 'Old-Skool-Wall-Block))

  ;(clear-area! B00 'all #:keep-items? #f)
  ;(repair-sea! B00 'all)

  ; TODO - adjust protected area. Use Seaweed Style floor (or any new/unused block).
  (with-protected-areas [manual-build]
    (put-hill! B00 cs-plateau (block 'Ice) #:y-min 1 #:y-max 50)
    (put-hill! B00 mountain (block 'Chert) #:y-min 1 #:y-max 94
               #:step-start 50 #:step-height 4))

  ;(save-stage! B00)
  }
