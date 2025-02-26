#lang racket

(require hermits-heresy
         (submod hermits-heresy undocumented)
         pict
         ;rackunit
         )

(println "starting script")

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

(define-values (cs-plateau mountain evil)
  (time (values (bitmap->area "cs-plateau.bmp")
                (bitmap->area "mountain.bmp")
                (bitmap->area "evil.bmp"))))

; bumps and bumps2 are basically the same thing, but with different randomness
; courtesy of the Crystallize effect. Use one with Chunky Chert and the other
; with Chert to get a nice blend. (Well, almost. Walls will be entirely
; one block or the other, which doesn't look great with steep walls but would
; probably look fine with more gradual slopes.)
; I'm really enjoying this workflow. Here's how to do the Paint.NET part:
; * Hide all layers except bumps. Increase bumps opacity to fully opaque.
; * Save as bitmap file.
; * Paint background white (to avoid opacity during blur)
; * Gaussian Blur, radius=12
; * Crystallize, cell size=3
; * Use magic wand (tolerance about 40% seems good) to delete white background
(define bumps (bitmap->hill "bumps.bmp"))
(define bumps2 (bitmap->hill "bumps2.bmp"))

(define (update-manual-build-pict stage filename)
  (define the-pict
    (stage->pict stage #hash((2766 . #xFF0000FF) ; seaweed (normal and troweled)
                             (718 . #xFF0000FF))))
  (send (pict->bitmap the-pict) save-file filename 'bmp)
  (println (format "updated: ~a" filename))
  (void))

; Pattern matching idea? (Is this easier in SQLite?)
; With this idea, it's easy to recommend "start your patterns with the rarest block for best speed."
; So in this example using #:upwards is better than #:downwards.
#;(from stage
        [(pattern (column #:upwards
                          ; each id will always be bound to a list on a match?
                          [start (one 'Seaweed-Styled-Block)]
                          [fill (greedy (any 'Seaside-Scene-Block 'vacant))]
                          [end (one 'Seaside-Scene-block)]))
         (println (list "blah" fill))
         (for ([point fill])
           (stage-write! stage point (block 'Seaside-Scene-Block)))]
        [(pattern blah ...)
         (void "do something")])

(define-syntax-rule (with-absolute-seed seed body ...)
  (parameterize ([current-pseudo-random-generator
                  (vector->pseudo-random-generator
                   (vector 42 42 seed 42 42 42))])
    body ...))

; Sweet, a traversal can handle stage->pict also!
; Damn, this feels like it's going to be awesome.
; Given a small DSL, you can analyze it and automatically do
; * chunk skipping, e.g. (when (is-block? a b c) ...)
; * y skipping, e.g. (when (= y 0) ...)
; * area filtering/skipping
; I think using expand-syntax-once is the key; it will allow us to stop when
; we have reached the level of expansion that we want.
;
; Also, we'd like some way to avoid calling on (block 'Lava) every node
; and instead precompile it... Not totally sure how.
; We would need to "lift" or "gather up" the constant expressions and scope
; them outside of the (lambda (callback-args) body ...) form.
; Aha - simply by using a special form like (constant expr) and walking
; the syntax tree, gathering them, and lifting them!
; For example,
#;(define-syntax-rule (replace-blocks! a ... b)
    (let ([a2 (constant (resolve-block a))]
          ...)
      (when (or (is-block? a2)
                ...)
        (set-block! b))))
; Then after reaching desired expansion, we can walk the tree and lift
; all the `(constant x)` expressions up outside of the lambda!
#;(traversal
   #:bindings [#:y y] ; should probably use a macro instead...
   (when (is-block? 'Bedrock)
     (set-pixel! pict "red"))

   ; replace-blocks! can be built using is-block? and set-block!
   (replace-blocks! 'Seaside-Scene-Block
                    'Seaweed-Styled-Block
                    (mottle '[Lava 3 Chert 1 Bloodstone 1]))
   (replace-liquids! 'Any 'Liquid-Lava)
   (when (in-hill? my-hill)
     (set-block! (cond
                   [(> y 60) 'Snow]
                   [(> y 55) (mottle 'Snow 'Chert)]
                   [else 'Chert])))
   )


{begin ;module+ main
  ;(copy-all-save-files! #:from 'B02 #:to 'B00)
  (define B00 (load-stage 'IoA 'B00))
  (println "loaded stage")
  ;(update-manual-build-pict B00 "TEMP.bmp")
  (define manual-build (bitmap->area "manual-build.bmp"))

  ; Takes about 15 seconds on the command line or 26 seconds in DrRacket.
  ; Full script takes about 37 seconds in DrRacket.
  #;(time (let ([lava (block 'Lava)])
            (traverse-lambda B00 (lambda (args)
                                   (case (callback-args-block args)
                                     [(718 2766) (set-callback-args-block! args lava)]
                                     [else #f])))))

  ; Takes about 12 seconds on the command line or 25 seconds in DrRacket.
  ; Full script takes about 36 seconds in DrRacket.
  ; Naive xzy bundling slows down to 15 seconds on the command line.
  ; Passing xzy as separate arguments speeds up, slightly below 12 seconds!
  ; Now try unsafe-provide...
  #;(begin
      (define trav
        (let ([lava (block 'Lava)])
          (traversal
           (when (block-matches? 718 2766)
             (set-block! lava)))))
      (time (traverse B00 trav)))

  ;(clear-area! B00 'all #:keep-items? #f)
  ;(repair-sea! B00 'all)

  ; It seems better *not* to allow the user to customize the (-> ARGB y-elevation) function...
  ; Instead the implementation uses this convention:
  #;(let ([y (- 95 (quotient (max r g b) 2))]) (blah ...))
  ; Doing it this way pushes the work to the image editor, which is the best place to handle it.
  ; So if you want to raise some plateau by N blocks, you just decrease (darken) the color by N*2.
  (time
   (protect-area! B00 manual-build)
   (put-hill! B00 (area->hill2 evil bumps) (block 'Poisonous-Peat) #:adjust-y -4)
   (put-hill! B00 (area->hill2 cs-plateau bumps) (block 'Snow))
   (put-hill! B00 (area->hill2 mountain bumps2) (block 'Chunky-Chert))
   (put-hill! B00 (area->hill2 mountain bumps) (block 'Chert))
   (with-absolute-seed 223344
     (decorate-peaks! B00 mountain
                      (lambda (xz below)
                        (if (not (simple? below))
                            0
                            (case (random 7)
                              [(0 1) 0] ; vacant
                              [(2 3) (chisel (block 'Snow) 'flat-lo)]
                              [(4 5) 18] ; snow cover
                              [(6) (block 'Snow)])))))
   )

  ;(save-stage! B00)
  }
