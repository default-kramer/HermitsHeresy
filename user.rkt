#lang racket

(require (except-in "lib.rkt" draw-topography)
         "lib2.rkt"
         "lib-dqb.rkt"
         pict)

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

(define (shift p #:x [dx 0] #:y [dy 0] #:z [dz 0])
  (define (go p)
    (point (+ dx (point-x p))
           (+ dy (point-y p))
           (+ dz (point-z p))))
  (if (list? p)
      (map go p)
      (go p)))

{module+ main
  (define (put-cliff! stage path #:max-drop max-drop #:min-drop min-drop)
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

  (define (put-plateau! stage ring #:y y #:layer-height layer-height #:min-y [min-y 1])
    (let* ([shell (ring-shell ring)]
           [min-y (max 0 min-y)]
           [y (min 95 y)])
      (for ([xz shell])
        (for ([y (in-range min-y (+ 1 y))])
          (put-block! stage (point (car xz) y (cdr xz)) (block 'Clodstone))))
      (when (> y (+ layer-height min-y))
        (put-plateau! stage (expand-ring ring)
                      #:y (- y layer-height)
                      #:layer-height layer-height
                      #:min-y min-y))))

  (define floor-y 40)
  (define hilltop-y (+ floor-y 12))

  (define path (steps->path (point 100 40 200) '(E E E E N E E E N
                                                   E E E E E S E N
                                                   E N E E N E N E
                                                   E E E N E E)))

  (define ring
    (let ([steps '(E E SE SE SE W W SW W W NW W W NE NE)])
      (points->ring (steps->path (point 100 1 200) steps))))

  ; Be careful to not accidentally write to B01
  (define hh-mountain-ring
    (let ([B01 (open-stgdat 'IoA (string->path "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/B01/STGDAT01.BIN"))])
      #;(print-block-stats B01 #:min-y 34 #:max-y 34)
      (IoA-find-ring B01 #;(block 'Vault-Wall)
                     #xA7A ; vault wall that I placed?
                     #:min-y 34 #:max-y 34)))
  (println hh-mountain-ring)

  #;{begin
      (define stage (open-stgdat 'IoA (string->path "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/B00/STGDAT01.BIN")))
      (clear-map! stage) ; #:above-y 45 #:keep-items? #t)
      (create-floor! stage #:y 33 #:y-start 1 #:block (block 'Ice))
      #;(for ([point (IoA-get-special-locs 'blue-tablet)])
          (put-block! stage point (block 'Ice)))
      #;(put-plateau! stage ring #:y 70 #:layer-height 5)
      ; y=31 is the min y that exceeds sea level on IoA
      #;(create-floor! stage #:y 31 #:block (block 'Snow) #:y-start 1)
      #;(put-cliff! stage path #:min-drop 1 #:max-drop 5)
      #;(put-cliff! stage (shift path #:x 40) #:min-drop 2 #:max-drop 7)
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
