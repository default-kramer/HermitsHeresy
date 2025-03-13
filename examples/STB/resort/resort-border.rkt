#lang racket

; Adds the North and West borders to the new, cozier resort area.
; Also rebuilds some of the sea and beach.

(require hermits-heresy
         (submod hermits-heresy undocumented))

(define release-mode
  ; A "one-way" release, not suitable for ongoing building.
  #;'publish

  ; A recurring release uses obvious placeholder blocks to indicate
  ; "this part of the build comes from an HH script."
  ; This should allow me to do things like place trees on islands
  ; and re-run the script and as long as those islands haven't changed
  ; everything should still line up.
  'recurring
  )

{begin
  (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

  ; Remember: you can set the PRNG state for a tagged build
  (pseudo-random-generator->vector (current-pseudo-random-generator))

  (copy-all-save-files! #:from 'B02 #:to 'B00)

  (define dst (load-stage 'IoA 'B00))

  ; The beach protect bitmap was created by doing something like
  #;(stage->pict stage 'Light-Dolomite)
  ; and then manually drawing the rest of the protected area.
  ; The light dolomite naturally defines the border of the resort cliffs.
  ; I also placed a few dolomite guideposts manually.
  (define beach-protect (bitmap->area "beach-protect.bmp"))

  ; These bitmaps were all drawn manually using the protect bitmap as a guide.
  (define beach-area (bitmap->area "resort-beach.bmp"))

  ; The dropoff bitmap is used to lower the sea floor when it nears
  ; the edge of the buildable area. To regenerate this bitmap:
  ; * use RGB=7 for the non-bedrock (off-limits) area
  ;   - the value 7 controls how much to drop by
  ;   - this value is saved in the .pdn file
  ; * paint the rest black (RGB=0)
  ; * Gaussian blur, radius = 9
  ; * Crystallize, cell size = 3
  ; * Magic Wand to delete all pixels that remain black (RGB=0)
  ; This gives you a bitmap where very black indicates less dropoff
  ; and less black indicates more dropoff. Thus you want
  #;([darkest -1] [step -1])
  ; as the amount to adjust the elevation by.
  (define sea-hill
    (make-hill
     (bitmap-sampler "sea.bmp"
                     #:rgb 'g
                     #:normalize '[0 1 ... N-1]
                     #:project '([lightest 31] [step -1]))
     (bitmap-hill-adjuster "sea-dropoff.bmp"
                           #:rgb 'max
                           #:project '([darkest -1] [step -1]))))

  ; TODO don't re-read the bitmap, get this area from the hill
  (define sea-area (bitmap->area "sea.bmp"))

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

  ; Confirmed that a virgin IoA sea uses 341 (full) and 349 (shallow).
  (define-values (sandy-sandstone
                  stony-sand
                  sea-full
                  sea-shallow)
    (case release-mode
      [(publish) (values (block 'Sandy-Sandstone)
                         (block 'Stony-Sand)
                         (block 'Sea-water-full-block)
                         (block 'Sea-water-shallow-block))]
      [(recurring) (values 469 ; yellow hardwood tile
                           (block 'Strange-Sand)
                           (block 'Sea-water-full-block)
                           (block 'Sea-water-shallow-block))]))

  (define do-hills? (member release-mode '(publish)))

  (define trav!
    (traversal
     (cond
       [(and do-hills?
             (or (in-platform-hills?! ph-resort-border-north-hi)
                 (in-platform-hills?! ph-resort-border-west-hi)
                 (in-platform-hills?! ph-resort-border-north-lo)
                 (in-platform-hills?! ph-resort-border-west-lo)
                 (in-platform-hills?! ph-resort-border-west-extra-lo)))
        #t]
       [(in-area? beach-protect) #f]
       [(in-area? beach-area)
        (when (< YYY 32)
          (set-block! sandy-sandstone)
          ; preserve my manual chisel work
          #;(set-chisel! 'none))]
       [(in-hill? sea-hill)
        (set-block! stony-sand)
        (set-chisel! 'none)]
       [(in-area? sea-area)
        (and (cond
               [(< YYY 31)
                (set-block! sea-full)]
               [(= YYY 31)
                (set-block! sea-shallow)]
               [#t #f])
             (set-chisel! 'none))]
       )))

  (time (traverse dst trav! #:force-unsound-optimization? #t))
  (save-stage! dst)
  }
