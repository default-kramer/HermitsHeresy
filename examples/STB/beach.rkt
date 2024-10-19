#lang racket

(require hermits-heresy
         (submod hermits-heresy undocumented)
         (only-in hermits-heresy/hill make-shell make-shell2))

{begin
  (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

  (define sand-hill (bitmap->hill "resort-sand.bmp" #:adjust-y 32))
  (define sand-hill-base (bitmap->hill "resort-sand.bmp" #:adjust-y 45))

  (define grass-hill (bitmap->hill "resort-grass.bmp" #:adjust-y 33))
  ;(define base-hill (bitmap->hill "base-placeholder.bmp" #:adjust-y 34))
  (define base-area (bitmap->area "base-placeholder.bmp"))

  (define sea-floor (bitmap->hill "resort-water.bmp" #:adjust-y 30))
  (define sea-full (bitmap->hill "resort-water.bmp" #:adjust-y 31))
  (define sea-top (bitmap->hill "resort-water.bmp" #:adjust-y 32))

  ; Drawn with width=28, hardness=12%, crystallize with cell size=3
  (define beach-border-hill (bitmap->hill "beach-border.bmp"
                                          ;"test.bmp"
                                          #:adjust-y 44 ;61
                                          ;#:cutoff-y 43
                                          ))

  (define NOMERGE (time (make-shell2 beach-border-hill #:y 38)))
  (define NOMERGE2 (make-shell2 NOMERGE #:y 35))

  (copy-all-save-files! #:from 'B02 #:to 'B00)

  (define dst (load-stage 'IoA 'B00))
  (protect-area! dst base-area)

  #;(define sandy-mottler
      (build-mottler '[Sandy-Sandstone 60]
                     ; suggest palm tree placement:
                     '[Shifting-Sand 1]))

  #;(define grassy-mottler
      (build-mottler '[Grassy-Earth 25]
                     ; suggest bush+fiddleheads placement:
                     '[Limegrassy-Earth 1]
                     '[Lemongrassy-Earth 1]
                     ; add the grass item atop this sand for a little flavor:
                     '[Sand 1]))

  ;(define ff (load-stage 'Furrowfield 'B00))
  ;(define ff-copy-area (bitmap->area "ff-copy-area.bmp"))
  ;(define ff-selection (selection ff ff-copy-area '((translate-to 460 0))))

  ;(define bt3 (load-stage 'BT3 'B00))
  ;(define ss-copy-area (bitmap->area "ss-copy-area.bmp"))
  ;(define ss-selection (selection bt3 ss-copy-area '((translate-to 471 -7))))

  (define trav
    (traversal
     #;(when (in-hill? beach-border-hill)
         (set-block! 'Dark-Dolomite))
     (cond
       #;[(block-matches? 'Old-Skool-Wall-Block)
          (set-block! 128 #;'Clear-water-full-block)]
       [(in-hill? NOMERGE2)
        (set-block! 'Dark-Dolomite)]
       [(in-hill? NOMERGE)
        (set-block! 'Chert)]
       [(in-hill? beach-border-hill)
        #;(set-block! 'Dark-Dolomite)
        #;(set-block! 'Seeded-Spoiled-Soil)
        (cond
          ;[(< YYY 35) (set-block! 'Dark-Dolomite)]
          ;[(< YYY 39) (set-block! 'Chert)]
          [(< YYY 42) (set-block! 'Seeded-Spoiled-Soil)]
          [#t (case (random 3)
                [(0 1) (set-block! 'Mossy-Spoiled-Soil)]
                [(2) (set-block! 'Seeded-Spoiled-Soil)])])
        ]
       #;[(in-hill? sand-hill-base)
          (set-block! 'Sandstone)]
       #;[(in-hill? sand-hill)
          (set-block! (sandy-mottler))]
       #;[(in-hill? grass-hill)
          (set-block! (grassy-mottler))]
       #;[(in-hill? sea-floor)
          (set-block! 'Stony-Sand)]
       #;[(in-hill? sea-full)
          (set-block! 341 #;(block 'Sea-water-full-block))]
       #;[(in-hill? sea-top)
          (set-block! 349 #;(block 'Sea-water-shallow-block))]
       )))

  (time (traverse dst trav #:force-unsound-optimization? #t))
  }
