#lang racket

(require hermits-heresy)

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")
(define source-slot 'B02) ; slot 3
(define dest-slot 'B00) ; slot 1, my ephemeral slot
(define stage-id 'BT1) ; Buildertopia (the first one, not Beta or Gamma)

; Use Buildertopia code 3c0mmifdh3 if you want to follow along using
; the exact same island.
  
{when #f ; phase 1
  (define gold-mottler
    (build-mottler '[Dark-Dolomite 5]
                   '[Gold-Vein 3]))

  (define trav
    (traversal
     (cond
       [(block-matches? 'Chalk)
        (set-block! 'Light-Dolomite)]
       [(block-matches? 'Chunky-Chalk)
        (set-block! (gold-mottler))])))

  (copy-all-save-files! #:from source-slot #:to dest-slot)
  (define stage (load-stage stage-id dest-slot))
  (traverse stage trav)
  (save-stage! stage)
  }

{when #f ; phase 2
  (define path-mottler
    (build-mottler '[Grassy-Earth 1]
                   '[Limegrassy-Earth 1]
                   '[Mossy-Earth 1]
                   '[Earth 1]
                   '[Stony-Soil 2]
                   '[Siltstone 2]))

  (define trav
    (traversal
     (cond
       [(block-matches? 'Seaside-Scene-Block)
        (set-block! (path-mottler))])))

  (copy-all-save-files! #:from source-slot #:to dest-slot)
  (define stage (load-stage stage-id dest-slot))
  (traverse stage trav)
  (save-stage! stage)
  }
