#lang racket

(require pict
         "stage.rkt")

(define empty-chunk (blank 32 32))

(define (chunk-layout->mask layout [color "black"])
  ; (define-type Chunk-Layout (Vectorof (Vectorof (U #f Integer))))
  (define masked-chunk (filled-rectangle 32 32 #:draw-border? #f #:color color))
  (define (row->pict vec)
    (apply hc-append (for/list ([cell vec])
                       (if cell empty-chunk masked-chunk))))
  (apply vc-append (for/list ([row layout])
                     (row->pict row))))

(define/contract (get-mask-image stage mask-kind)
  (-> stage? (or/c 'chunk 'bedrock) any/c)
  (case mask-kind
    [(chunk)
     (let* ([layout (stage-chunk-layout stage)]
            [pict (chunk-layout->mask layout)])
       pict)]
    [(bedrock)
     (let* ([layout (stage-chunk-layout stage)]
            [height (* 32 (vector-length layout))]
            [width (* 32 (vector-length (vector-ref layout 0)))])
       (define bytes-per-pixel 4)
       (define pict-bytes (make-bytes (* bytes-per-pixel width height)))
       (error "TODO"))]))

(define/contract (save-mask-image! stage mask-kind filename)
  (-> stage? (or/c 'chunk 'bedrock) path-string? any/c)
  (case mask-kind
    [(chunk)
     (let* ([layout (stage-chunk-layout stage)]
            [pict (chunk-layout->mask layout)])
       pict)]))

{module+ main
  (require hermits-heresy
           (submod hermits-heresy undocumented))
  (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")
  (define stage (load-stage 'BT1 'B00))

  (define (save-image pic filename)
    (define bmp (pict->bitmap pic 'unsmoothed))
    (send bmp save-file filename 'bmp)
    (format "wrote to ~a" (build-path (current-directory) filename)))

  (define chunkmask (get-mask-image stage 'chunk))
  (save-image chunkmask "chunk-mask.bmp")

  (define bedrockmask (stage->pict stage (hash 1 #xFF000000)))
  (save-image bedrockmask "bedrock-mask.bmp")
  }
