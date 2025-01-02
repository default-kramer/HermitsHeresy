#lang racket

(require hermits-heresy
         (submod hermits-heresy undocumented)
         (only-in hermits-heresy/interpolator make-interpolator interpolate)
         (only-in hermits-heresy/basics make-rect xz))

; Works nicely on a Blossom Bay that has been flattened down to sea level,
; or a little bit higher.

{begin
  (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

  (copy-all-save-files! #:from 'B02 #:to 'B00)

  (define stage (load-stage 'BT1 'B00))

  ; Bigger than the stage
  (define big-rect (make-rect (xz 0 0)
                              (xz 1000 1000)))

  (define land-shape
    (let* (; 12,11 looks great at elev 8
           ; and 27,23 elev 8
           ; and even 33,29 elev 15
           [i1 (make-interpolator big-rect 23)]
           [i2 (make-interpolator big-rect 19)])
      (lambda (xz)
        (* (interpolate i1 xz)
           (interpolate i2 xz)))))

  (define mtn-shape
    (let* ([i1 (make-interpolator big-rect 47)]
           [i2 (make-interpolator big-rect 41)])
      (lambda (xz)
        (let* ([v1 (interpolate i1 xz)]
               [v2 (interpolate i2 xz)]
               [v (* v1 v2)])
          v))))

  (define leafy-shape
    (let* ([i1 (make-interpolator big-rect 12)]
           [i2 (make-interpolator big-rect 11)])
      (lambda (xz)
        (* (interpolate i1 xz)
           (interpolate i2 xz)))))

  (define trav
    (traversal
     ; For better speed, the interpolator could be called once per XZ
     ; instead of per XZY... but I'm not gonna do that now.
     (let* ([mtn-val (mtn-shape (xz XXX ZZZ))]
            [land-val (land-shape (xz XXX ZZZ))]
            [land-factor (* 8 land-val)])
       (cond
         [(and (> mtn-val 0.3)
               (< YYY (+ 34 land-factor (* 25 mtn-val))))
          (set-block! 'Umber)]
         [(< YYY (+ 37 land-factor))
          (cond
            [(> (leafy-shape (xz XXX ZZZ)) 0.13)
             (set-block! 'Leafy-Spoiled-Soil)]
            [#t (set-block! 'Earth)])]
         ; Here is the "update the minimap" trick, just go touch this water
         [(= YYY 60)
          (set-block! 120)]))))

  (time (traverse stage trav))

  ;(save-stage! stage)
  }
