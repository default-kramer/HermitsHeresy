#lang racket

(require "lib.rkt" "lib-dqb.rkt" pict)

(define (show top)
  (scale (topography->pict top) 10))

(define top1
  (draw-topography
   ----XXX---
   ---XXX----
   ---XXXX---
   --XXXXXX--
   -XXXXX----
   --XXX-----
   ---XX-----))
(show top1)

(define top2
  (expand-topography top1 (basic-hill-expander #:steps 6 #:slope 2)))
(show top2)

(define top3
  (expand-topography top1 (basic-hill-expander #:steps 12 #:slope 1)))
(show top3)

(define-syntax-rule (define-blocks f [id val] ...)
  (define (f a)
    (case a
      [(id) (bitwise-ior #x800 val)]
      ...
      [else (error "Unknown block:" a)])))

; For example, Stony-Soil has a "value" of 9c and could be written to file as 9c 08
; The 08 means "no chisel" (and maybe "placed by user"? because 9c 00 also works).
; Chisel status can be one of (all in hex):
; * 08 - no chisel
; * 18/38/58/78 - diagonal chisel N/E/S/W, matches (blueprint.chisel_status << 4) | 8
; * 28/48/68/88 - diagonal chisel SW/SE/NW/NE
; * 98/a8/b8/c8 - concave chisel NW/SW/SE/NE
; * d8/e8 - flat chisel hi/lo
(define-blocks block
  [Earth #x02]
  [Limegrassy-Earth #x04]
  [Chalk #x08]
  [Chunky-Chalk #x09]
  [Ice #x14]
  [Clodstone #x15]
  [Light-Dolomite #x82]
  [Dark-Dolomite #x83]
  [Stony-Soil #x8D]
  [Arid-Earth #x93]
  [Chert #x95]
  [Chunky-Chert #x99]
  [Spoiled-Soil #x9C]
  [Umber #xD1]
  [Lumpy-Umber #xF1])

(define (put-cliff! path #:max-drop max-drop #:min-drop min-drop)
  (let loop ([path path]
             [blocks (map block '(Chert Light-Dolomite Clodstone Dark-Dolomite))]
             [idx 0])
    (let ([done? #t]
          [block (list-ref blocks idx)])
      (for ([p path])
        (when (> (point-y p) 0)
          (set! done? #f)
          (for ([y (in-range (+ 1 (point-y p)))])
            (put-block! stage (point (point-x p)
                                     y
                                     (point-z p))
                        block))))
      (when (not done?)
        (loop (todo path #:max-drop max-drop #:min-drop min-drop)
              blocks
              (modulo (+ 1 idx)
                      (length blocks)))))))

(define (shift p #:x [dx 0] #:y [dy 0] #:z [dz 0])
  (define (go p)
    (point (+ dx (point-x p))
           (+ dy (point-y p))
           (+ dz (point-z p))))
  (if (list? p)
      (map go p)
      (go p)))

{begin
  (define stage (open-stgdat 'IoA (string->path "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/B00/STGDAT01.BIN")))
  (clear-map! stage #:add-chunk-ids? #f)

  (define floor-y 40)
  (define hilltop-y (+ floor-y 12))

  (define path (steps->path (point 100 40 200) '(E E E E N E E E N
                                                   E E E E E S E N
                                                   E N E E N E N E
                                                   E E E N E E)))
  ;path
  ;(define path2 (todo path))
  ;path2

  {begin
    (put-cliff! path #:min-drop 1 #:max-drop 5)
    (put-cliff! (shift path #:x 40) #:min-drop 2 #:max-drop 7)
    (save-stgdat! stage)}
  #;{begin
      (create-floor! stage #:y floor-y)
      (place-topography! stage top1 #:x 120 #:z 230
                         #:y-start hilltop-y #:y-end floor-y
                         #:block (block 'Ice))
      (place-topography! stage top2 #:x 145 #:z 230
                         #:y-start hilltop-y #:y-end floor-y
                         #:block (block 'Ice))
      (place-topography! stage top3 #:x 170 #:z 230
                         #:y-start hilltop-y #:y-end floor-y
                         #:block (block 'Ice))
      (save-stgdat! stage)
      }
  }
