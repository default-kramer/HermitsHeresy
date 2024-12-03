#lang racket

(require hermits-heresy
         (submod hermits-heresy undocumented)
         "helper.rkt"
         )

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

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


#;{begin ;module+ main
    ;(copy-all-save-files! #:from 'B02 #:to 'B00)
    (define B00 (load-stage 'IoA 'B00))

    ; Of course, the following timing depends on what is actually in my B00 save slot.
    ; What's important is that the macro traversal outperforms the lambda traversal,
    ; or at least equals it.
    ; For timing inside DrRacket, close it, run `raco setup --pkgs hermits-heresy`,
    ; and restart DrRacket with only this file open.

    ; Takes about 14 seconds on the command line or 15 seconds in DrRacket.
    ; Full script takes about 21 seconds in DrRacket.
    #;(time (let ([lava (block 'Lava)])
              (traverse-lambda B00 (lambda (args)
                                     (case (argbox-block args)
                                       [(718 2766) (set-argbox-block! args lava)]
                                       [else #f])))))

    ; Takes about 11 seconds on the command line or 13 seconds in DrRacket.
    ; Full script takes about 18 seconds in DrRacket.
    (begin
      (define trav
        (let ([lava (block 'Lava)])
          (traversal
           (when (block-matches? 718 2766)
             (set-block! lava)))))
      (time (traverse B00 trav)))

    ;(save-stage! B00)
    }

(define COUNT 0)

(define-syntax-rule (build-mottle-proc sym ...)
  (let* ([vec (vector-immutable (block sym)
                                ...)]
         [prng (make-pseudo-random-generator)]
         [len (vector-length vec)])
    (lambda ()
      (set! COUNT (+ 1 COUNT))
      (vector-ref vec (random len prng)))))

{begin ;module+ main
  (define mottle (build-mottle-proc 'Grassy-Earth
                                    'Grassy-Earth
                                    'Lemongrassy-Earth
                                    'Limegrassy-Earth
                                    'Earth
                                    'Mossy-Earth
                                    'Stony-Soil))
  (define trav
    #;(traversal
       (cond
         [(> YYY 70) ; 83 -> 2064384, 70 -> 4300800
          (set-block! (mottle))]))
    (traversal
     (cond
       [(liquid?)
        (void "keep liquids")]
       #;[(block-matches? 'Seaside-Sand 'Stony-Sand 'Bubbling-Seaside-Sand)
          (set-block! 'Ice)]
       #;[(block-matches? 'Marble)
          (set-block! 'Zenithium-Vein)]
       #;[(block-matches? 'Snow)
          (void "keep snow")]
       #;[(block-matches? 'Grassy-Earth 'Limegrassy-Earth)
          (set-block! (mottle))]
       [(not (block-matches? 0))
        (set-block! 'Ice)]
       #;[(not (block-matches? 0))
          (set-block! (mottle))]
       ))
    #;(traversal
       (cond
         [(block-matches? 'Snow)
          (set-block! 'Mossy-Earth)]
         [(block-matches? 'Chalk)
          (set-block! 'Stony-Soil)]
         [(block-matches? 'Marble)
          (set-block! 'Copper-Vein)]))
    #;(traversal (when (not (block-matches? 0))
                   (set-block! 'Ice)))
    #;(traversal
       (cond
         [(= YYY 60)
          (case (remainder (quotient XXX 20) 2)
            [(0) (set-block! 'Zenithium-Vein)]
            [(1) (set-block! 'Magnetite-Vein)])]))
    )

  (begin
    (define stage (load-stage 'BT1 'B00))
    (time (traverse stage trav))
    ;(save-stage! stage)
    )
  }
