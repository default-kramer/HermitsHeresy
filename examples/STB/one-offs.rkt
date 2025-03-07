#lang racket

; Templates for one-off scripts

(require hermits-heresy)

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

#;{begin ; one-off template: stage->pict
    (require (submod hermits-heresy undocumented)
             pict)

    (define stage (load-stage 'IoA 'B02))
    (define the-pict
      (let* ([block1 (block 'Carved-Castle-Tile)]
             [block2 (+ #x800 block1)]
             [color #xFF0000FF])
        (stage->pict stage (hash block1 color block2 color))))
    (let ([filename "resort-aquarium.bmp"])
      (send (pict->bitmap the-pict) save-file filename 'bmp)
      (println (format "updated: ~a" filename)))
    }

#;{begin ; one-off template: filling an area
    (require (submod hermits-heresy undocumented))

    (copy-all-save-files! #:from 'B02 #:to 'B00)
    (define stage (load-stage 'IoA 'B00))

    (define area (bitmap->area "TEMPAREA2.bmp"))
    (define trav (traversal
                  (cond
                    [(and (in-area? area)
                          (< YYY 35))
                     (set-block! 'Grassy-Earth)
                     (set-chisel! 'none)])))
    (traverse stage trav #:force-unsound-optimization? #t)
    (save-stage! stage)
    }
