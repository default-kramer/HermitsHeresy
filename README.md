# HermitsHeresy
Unauthorized utilities for scheming up scenes. Move mountains, carve canyons, or summon superstructures!

(Power Tools for DQB2)

Work in Progress!

# Motivating Example
```
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

(define (block a)
  (case a
    [(Ice) #x814]
    [else (error "TODO")]))

{begin
  (define stage (open-stgdat 'IoA (string->path "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/B00/STGDAT01.BIN")))

  (define floor-y 40)
  (define hilltop-y (+ floor-y 12))

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
```
![20240509142641_1](https://github.com/default-kramer/HermitsHeresy/assets/4582586/2408c676-0430-4cdc-b04b-b8da57e96a9c)
