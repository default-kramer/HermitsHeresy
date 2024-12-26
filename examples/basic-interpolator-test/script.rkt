#lang racket

(require hermits-heresy
         (submod hermits-heresy undocumented)
         (only-in hermits-heresy/interpolator make-interpolator interpolate)
         (only-in hermits-heresy/basics make-rect xz))

{begin
  (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

  (copy-all-save-files! #:from 'B02 #:to 'B00)

  (define stage (load-stage 'BT1 'B00))

  ; Grid size for interpolator:
  (define scale 40)

  ; Keep (floor-y + height) below 96 or some peaks might be missing
  (define floor-y 50)
  (define height 20)

  (define interpolator
    (let ([rect (make-rect (xz 0 0)
                           ; this is bigger than we actually need, but not a problem:
                           (xz 1000 1000))])
      (make-interpolator rect scale)))

  (define trav
    (traversal
     (let* ([fl (interpolate interpolator (xz XXX ZZZ))]
            [peak (+ floor-y (floor (* fl height)))])
       (when (<= YYY peak)
         (set-block! 'Grassy-Earth)))))

  (time (traverse stage trav))

  ;(save-stage! stage)
  }
