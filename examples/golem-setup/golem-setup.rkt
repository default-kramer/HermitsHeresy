#lang racket

(require "../../NEW-API.rkt"
         pict
         rackunit)

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

; I want to destroy everything possible on the IoA, leaving only
; the indestructible items/blocks. This would allow me to easily grab all
; the coordinates of everything that is indestructible, and thus enable
; a "legal builds only" mode which would not allow you to destroy those coords.
;
; To make this easier, define an area and create a platform for the golem to walk around on.
; Once I get down to sea level, another strategy might be needed but for now I only
; care about stuff above sea level.
(define level-area (bitmap->area "to-level.bmp"))

{module+ main
  (define B01 (mark-writable (load-stage 'IoA 'B01)))
  (fill! B01 level-area (block 'Seaside-Scene-Block))
  (save-stage! B01)
  }
