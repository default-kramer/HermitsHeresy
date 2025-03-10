#lang racket

(require hermits-heresy
         #;(only-in (submod hermits-heresy undocumented) YYY))

{begin
  (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

  ;(define sand-hill (bitmap->hill "resort-sand.bmp" #:adjust-y 32))
  ;(define sand-hill-base (bitmap->hill "resort-sand.bmp" #:adjust-y 31))
  ;(define grass-hill (bitmap->hill "resort-grass.bmp" #:adjust-y 33))
  ;(define base-hill (bitmap->hill "base-placeholder.bmp" #:adjust-y 34))
  (define base-area (bitmap->area "base-placeholder.bmp"))

  ;(define sea-floor (bitmap->hill "resort-water.bmp" #:adjust-y 30))
  ;(define sea-full (bitmap->hill "resort-water.bmp" #:adjust-y 31))
  ;(define sea-top (bitmap->hill "resort-water.bmp" #:adjust-y 32))

  ; Drawn with width=28, hardness=12%, crystallize with cell size=3
  ;(define beach-border-hill (bitmap->hill "beach-border.bmp" #:adjust-y 61))

  (copy-all-save-files! #:from 'B02 #:to 'B00)

  (define dst (load-stage 'IoA 'B00))
  (protect-area! dst base-area)

  (define ph-port-border-north-lo
    (make-platform-hills (generate-platform-layout 170 40)
                         #:x 380 #:z 380
                         #:peak-y 44))
  (define ph-port-border-north-hi
    (make-platform-hills (generate-platform-layout 155 30)
                         #:x 380 #:z 380
                         #:peak-y 54))

  (define ph-happy-accident-lo
    (make-platform-hills (generate-platform-layout 40 100)
                         #:x 495 #:z 390
                         #:peak-y 44))
  (define ph-happy-accident-hi
    (make-platform-hills (generate-platform-layout 30 100)
                         #:x 495 #:z 390
                         #:peak-y 54))

  (define ph-resort-border-west-lo
    (make-platform-hills (generate-platform-layout 40 100)
                         #:x 480 #:z 290
                         #:peak-y 44))
  (define ph-resort-border-west-hi
    (make-platform-hills (generate-platform-layout 30 100)
                         #:x 480 #:z 290
                         #:peak-y 54))

  (define ph-resort-border-north-lo
    (make-platform-hills (generate-platform-layout 320 40)
                         #:x 500 #:z 270
                         #:peak-y 44))
  (define ph-resort-border-north-hi
    (make-platform-hills (generate-platform-layout 305 30)
                         #:x 500 #:z 270
                         #:peak-y 54))


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
     (cond
       [(in-platform-hills?! ph-port-border-north-hi) #t]
       [(in-platform-hills?! ph-happy-accident-hi) #t]
       [(in-platform-hills?! ph-resort-border-north-hi) #t]
       [(in-platform-hills?! ph-resort-border-west-hi) #t]
       [(in-platform-hills?! ph-port-border-north-lo) #t]
       [(in-platform-hills?! ph-happy-accident-lo) #t]
       [(in-platform-hills?! ph-resort-border-north-lo) #t]
       [(in-platform-hills?! ph-resort-border-west-lo) #t]
       )
     #;(when (in-hill? beach-border-hill)
         (set-block! 'Dark-Dolomite))
     #;(cond
         #;[(block-matches? 'Old-Skool-Wall-Block)
            (set-block! 128 #;'Clear-water-full-block)]
         #;[(in-hill? beach-border-hill)
            (set-block! 'Dark-Dolomite)]
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
         ; Here is the "update the minimap" trick, just go touch this water
         #;[(= YYY 60)
            (set-block! 120)]
         )))

  (time (traverse dst trav #:force-unsound-optimization? #t))
  }
