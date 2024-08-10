#lang racket

(provide get-template-image save-template-image)

(require (prefix-in layout: "layouts.rkt")
         racket/runtime-path
         pict)

(module+ test
  (require rackunit))

(define-runtime-path images-dir "images")

(define (load-image id)
  (bitmap (build-path images-dir (format "~a.bmp" id))))

(define-syntax-rule (load-images id ...)
  (make-hash (list (cons 'id  (load-image 'id))
                   ...)))

(define images (load-images IoA-background
                            IoA-bedrock-mask
                            Furrowfield-background
                            Furrowfield-bedrock-mask
                            Khrumbul-Dun-background
                            Khrumbul-Dun-bedrock-mask
                            Moonbrooke-background
                            Moonbrooke-bedrock-mask
                            Malhalla-background
                            Anglers-Isle-background
                            Anglers-Isle-bedrock-mask
                            Skelkatraz-background
                            Skelkatraz-bedrock-mask
                            ))

(module+ test ; make sure all image dimensions are some multiple of 32
  (for ([img (hash-values images)])
    (check-equal? (remainder (pict-width img) 32) 0)
    (check-equal? (remainder (pict-height img) 32) 0)))

(define/contract (save-template-image id)
  (-> symbol? any/c)
  (define pic (get-template-image id))
  (define bmp (pict->bitmap pic 'unsmoothed))
  (define filename (format "~a.bmp" id))
  (send bmp save-file filename 'bmp)
  (format "wrote to ~a" (build-path (current-directory) filename)))

(define/contract (get-template-image id)
  (-> symbol? pict?)
  (define (legacy-translate id)
    (case id
      [(IoA-mask) 'IoA-chunk-mask]
      [else id]))
  (case (legacy-translate id)
    [(IoA-chunk-mask) (chunk-layout->mask layout:IoA)]
    [(Furrowfield-chunk-mask) (chunk-layout->mask layout:Furrowfield)]
    [(Khrumbul-Dun-chunk-mask) (chunk-layout->mask layout:Khrumbul-Dun)]
    [(Moonbrooke-chunk-mask) (chunk-layout->mask layout:Moonbrooke)]
    [(Malhalla-chunk-mask) (chunk-layout->mask layout:Malhalla "gray")]
    [(Anglers-Isle-chunk-mask) (chunk-layout->mask layout:Anglers-Isle)]
    [(Skelkatraz-chunk-mask) (chunk-layout->mask layout:Skelkatraz)]
    [else
     (hash-ref images id
               (lambda () (error "Unknown image ID:" id)))]))

(define empty-chunk (blank 32 32))

(define (chunk-layout->mask layout [color "black"])
  ; (define-type Chunk-Layout (Vectorof (Vectorof (U #f Integer))))
  (define masked-chunk (filled-rectangle 32 32 #:draw-border? #f #:color color))
  (define (row->pict vec)
    (apply hc-append (for/list ([cell vec])
                       (if cell empty-chunk masked-chunk))))
  (apply vc-append (for/list ([row layout])
                     (row->pict row))))


; Temp code, should no longer be needed... but still here for posterity.
; Converts an image like this: https://github.com/Sapphire645/DQB2ChunkEditorPlus/blob/main/src/Data/Masks/Angler_Mask.png
; (slightly modified: delete the background) to the list that `parse-map` wants
#;(define (read-sapphire-mask)
    (define bmp (bitmap "readmask.bmp"))
    (define pixels (pict->argb-pixels bmp))
    (define cell-size 64) ; 64x64 pixels per chunk
    (define pixels-per-row (pict-width bmp))
    (define W (quotient (+ 10 (pict-width bmp)) cell-size)) ; chunks wide
    (define H (quotient (+ 10 (pict-height bmp)) cell-size)) ; chunks tall
    (for/list ([z (in-range H)])
      (for/list ([x (in-range W)])
        (define pixel-x (+ 7 (* x cell-size))) ; +7 to move a bit more towards the center
        (define pixel-z (+ 7 (* z cell-size)))
        (define index (* 4 (+ pixel-x
                              (* pixel-z pixels-per-row))))
        (define alpha (bytes-ref pixels index))
        ;(println (list x z index pixel-x pixel-z alpha))
        (if (= 0 alpha) '_ 'X))))

; use from REPL for a quick spot check
(define (spot-check)
  (list (cc-superimpose (get-template-image 'IoA-background)
                        (get-template-image 'IoA-chunk-mask))
        (cc-superimpose (get-template-image 'IoA-background)
                        (get-template-image 'IoA-bedrock-mask))
        (cc-superimpose (get-template-image 'Furrowfield-background)
                        (get-template-image 'Furrowfield-chunk-mask))
        (cc-superimpose (get-template-image 'Furrowfield-background)
                        (get-template-image 'Furrowfield-bedrock-mask))
        (cc-superimpose (get-template-image 'Khrumbul-Dun-background)
                        (get-template-image 'Khrumbul-Dun-chunk-mask))
        (cc-superimpose (get-template-image 'Khrumbul-Dun-background)
                        (get-template-image 'Khrumbul-Dun-bedrock-mask))
        (cc-superimpose (get-template-image 'Moonbrooke-background)
                        (get-template-image 'Moonbrooke-chunk-mask))
        (cc-superimpose (get-template-image 'Moonbrooke-background)
                        (get-template-image 'Moonbrooke-bedrock-mask))
        (cc-superimpose (get-template-image 'Malhalla-background)
                        (get-template-image 'Malhalla-chunk-mask))
        (cc-superimpose (get-template-image 'Anglers-Isle-background)
                        (get-template-image 'Anglers-Isle-chunk-mask))
        (cc-superimpose (get-template-image 'Anglers-Isle-background)
                        (get-template-image 'Anglers-Isle-bedrock-mask))
        (cc-superimpose (get-template-image 'Skelkatraz-background)
                        (get-template-image 'Skelkatraz-chunk-mask))
        (cc-superimpose (get-template-image 'Skelkatraz-background)
                        (get-template-image 'Skelkatraz-bedrock-mask))))

(module+ test
  (void (spot-check)))
