#lang racket

; Don't forget
; * above-hill? seems like it would be useful
; * you could allow a traversal to write to a megablueprint by
;   providing alternate implementations of set-block! et al...
;   You could do both at the same time (write to stage and blueprint/log)
; * you could export platform-hills to a bitmap to allow the user to specify elevation
;  - as separate bitmaps: lo, hi, and borders
; * you could reimplement translation,rotation,reflection as sampler combiners
;  - although would need a new type for a 3D block sampler


; Adds the North and West borders to the new, cozier resort area.
; Also rebuilds some of the sea and beach.

(require hermits-heresy
         (submod hermits-heresy undocumented)
         (prefix-in ph- "../platform-hills.rkt"))

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

  ; This was drawn using the platform hills as a guide:
  (define enclave-area (bitmap->area "enclave.bmp"))

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
  #;([darkest 0] [step -1])
  ; as the amount to adjust the elevation by.
  ; (Pure black was deleted, so anchor it to 0 instead of -1)
  (define sea-hill
    (make-hill
     (bitmap-sampler "sea.bmp"
                     #:rgb 'g
                     #:normalize '[0 .. N-1]
                     ; Should end at 31, just below the shallow sea level
                     #:project '[23 24 25 26 27 28 29 30 31])
     (function + (bitmap-sampler "sea-dropoff.bmp"
                                 #:rgb 'max
                                 #:project '([darkest 0] [step -1])))
     ; just testing, doesn't look great:
     #;(let ([floor (bitmap->area "sea-floor-only.bmp")])
         (function - (make-interpolated-sampler floor
                                                12 ; scale
                                                '[0 0 1 1 2])))))

  ; TODO don't re-read the bitmap, get this area from the hill
  (define sea-area (bitmap->area "sea.bmp"))

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
      [(recurring) (values (block 'Strange-Sand) ; 469 ; yellow hardwood tile
                           (block 'Strange-Sand)
                           (block 'Sea-water-full-block)
                           (block 'Sea-water-shallow-block))]))

  (define do-hills? (member release-mode '(publish)))

  (define trav!
    (traversal
     ; update the minimap trick:
     #;(when (and (in-area? beach-area)
                  (= YYY 60)
                  (not (= 0 (remainder XXX 96)))
                  (not (= 0 (remainder ZZZ 96))))
         (set-block! 120))
     #;(let ([belt-y 40]
             ; The only flaw with Sooty Softwood is that I'd prefer not to stack
             ; brick/stone on top of wood if I can avoid it...
             [belt (block 'Sooty-Softwood)]
             ; Granite Floor is almost as good as Sooty Softwood.
             ; But it does kind of insist upon itself.
             ; And there is one pretty gnarly reflection from the pool light.
             #;[belt (block 'Granite-Floor-Block)]
             ; The Red Brick Floor really pops, but the texture is too
             ; similar to the Masonry Wall below it.
             #;[belt (block 'Red-Brick-Floor)]

             ; Mottled Wall (black) pretty much requires a black roof,
             ; but it's worth it. Interiors and exteriors both pop!
             ; This + Sooty Softwood + black roofs looks unbeatable.
             ; Should I try the non-Fancy black roof?
             ; Nah, I'd rather keep that fresh for my tiny houses.
             ; (Also: Changed the pool cover and harp cover to the non-Fancy roof (blue)
             ;  and it matches perfectly, and even ties the blue bunting together!)
             ; UPON FURTHER REVIEW: Masonry Wall might actually be better?
             [wall 810] ; Mottled Wall (black)
             ; Herringbone Floorboard (white) is very good.
             ; It works with the blue or black roof.
             ; But I think the texture is just a little too much,
             ; giving the edge to the Mottled Wall (black).
             #;[wall 756] ; Herringbone Floorboard (white)
             ; Don't forget, Masonry Wall is still very good.
             ; Works with blue or black roof, but I think black is better.
             #;[wall (block 'Masonry-Wall)]
             ; Wooden Wall (blue) does make interiors look great.
             ; Needs roof change, black is good.
             #;[wall 308] ; Wooden Wall (blue)
             ; Woodless Timbered Wall is too white,
             ; although I never tried it with the black roof.
             #;[wall (block 'Woodless-Timbered-Wall)]
             ; Always worth trying Adobe, but not this time.
             #;[wall (block 'Adobe-Wall-Midsection)]
             ; Other notes: Slimy Wall is actually pretty good, and where
             ; would I use it other than a resort?
             ; Even the Slimy Block is better than I would expect.
             )
         (cond
           [(and (= YYY belt-y)
                 (block-matches? 'Modern-Masonry-Block 'Granite-Floor-Block))
            (set-block! belt)]
           [(and (> YYY belt-y)
                 (block-matches? 'Masonry-Wall))
            (set-block! wall)]))
     (cond
       [(and do-hills?
             (or (in-platform-hills?! ph-resort-border-north-hi)
                 (in-platform-hills?! ph-resort-border-west-hi)
                 (in-platform-hills?! ph-dock-border-north)
                 (in-platform-hills?! ph-resort-border-north-lo)
                 (in-platform-hills?! ph-resort-border-west-lo)
                 (in-platform-hills?! ph-dock-enclave-west)
                 (in-platform-hills?! ph-dock-enclave-south)
                 (in-platform-hills?! ph-resort-border-west-extra-lo)))
        #t]
       [(in-area? enclave-area)
        ; A fishing hole here would probably be good. It will probably
        ; stop Gillian and Finn from fishing in the aquarium...
        (when (< YYY 32)
          (set-block! 'Stony-Sand))
        (when (= YYY 37)
          (set-block! 120))]
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
