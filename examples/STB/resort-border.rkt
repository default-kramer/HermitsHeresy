#lang racket

; Adds the North and West borders to the new, cozier resort area.
; Use this script to review and tweak the area size before committing
; to rebuilding the (reflected!) resort.

(require hermits-heresy
         (submod hermits-heresy undocumented))

{begin
  (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

  ; Remember: you can set the PRNG state for a tagged build
  (pseudo-random-generator->vector (current-pseudo-random-generator))

  (copy-all-save-files! #:from 'B02 #:to 'B00)

  (define dst (load-stage 'IoA 'B00))

  ; Proof-of-concept on how to slope the seafloor down towards the edge
  ; of the buildable area.
  ; Right now I just eyeballed seafloor-protect.
  ; This should be defined precisely as the edge of the manual build area.
  ; Then draw the seafloor-slope at RGB=132 along this edge.
  ; Draw a wider area seafloor-min at RGB=152 that fills to the edge of the map.
  ; Then use layers seafloor-slope+seafloor-min to generate the bitmap.
  ; * Paint the background RGB=172
  ; * Gaussian blur, radius=16
  ; * Crystallize, cell size=3
  ; * Magic wand delete all background (RGB=172), tolerance=0%
  (define seafloor-area (bitmap->area "seafloor-slope.bmp"))
  (define seafloor-hill (bitmap->hill "seafloor-slope.bmp" #:adjust-y 1))
  (define seafloor-protect (bitmap->area "seafloor-protect.bmp"))

  (define ph-resort-border-west-lo
    (make-platform-hills (generate-platform-layout 40 105)
                         #:x 560 #:z 290
                         #:peak-y 44))
  (define ph-resort-border-west-hi
    (make-platform-hills (generate-platform-layout 30 90)
                         #:x 560 #:z 290
                         #:peak-y 54))
  ; To avoid draw distance problems when looking from the NE corner to the SW
  ; corner, lower the hill as it goes south.
  (define ph-resort-border-west-extra-lo
    (make-platform-hills (generate-platform-layout 40 65)
                         #:x 560 #:z 385
                         #:peak-y 39))

  (define ph-resort-border-north-lo
    (make-platform-hills (generate-platform-layout 320 40)
                         #:x 400 #:z 262
                         #:peak-y 44))
  (define ph-resort-border-north-hi
    (make-platform-hills (generate-platform-layout 305 30)
                         #:x 400 #:z 262
                         #:peak-y 54))

  (define trav!
    (traversal
     (cond
       [(in-area? seafloor-area)
        (cond
          [(in-area? seafloor-protect) #f]
          [(in-hill? seafloor-hill)
           (set-block! 'Stony-Sand)]
          [(< YYY 31)
           (set-block! 'Sea-water-full-block)]
          [(= YYY 31)
           (set-block! 'Sea-water-shallow-block)]
          [#t
           (set-block! 0)])]
       [(in-platform-hills?! ph-resort-border-north-hi) #t]
       [(in-platform-hills?! ph-resort-border-west-hi) #t]
       [(in-platform-hills?! ph-resort-border-north-lo) #t]
       [(in-platform-hills?! ph-resort-border-west-lo) #t]
       [(in-platform-hills?! ph-resort-border-west-extra-lo) #t]
       )))

  (time (traverse dst trav! #:force-unsound-optimization? #t))
  (save-stage! dst)
  }
