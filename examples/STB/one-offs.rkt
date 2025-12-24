#lang racket

; Templates for one-off scripts

(require hermits-heresy)

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

#;{begin ; one-off template: stage->pict
    (require (submod hermits-heresy undocumented)
             pict)

    (define stage (load-stage 'IoA 'B02))
    (define the-pict
      (let* ([block1 (block 'Light-Dolomite)]
             [block2 (+ #x800 block1)]
             [color #xFF0000FF])
        (stage->pict stage (hash block1 color block2 color))))
    (let ([filename "resort-dolomite.bmp"])
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

#;{begin ; one-off template: repair the sea
    (require (submod hermits-heresy undocumented))

    (copy-all-save-files! #:from 'B02 #:to 'B00)
    (define stage (load-stage 'IoA 'B00))

    (define area (bitmap->area "repair-sea-area.bmp"))
    (define trav (traversal
                  (cond
                    [(and (in-area? area)
                          (block-matches? 0
                                          'Sea-water-full-block
                                          'Sea-water-surface-block
                                          'Sea-water-shallow-block))
                     (cond
                       [(< YYY 31)
                        #;(set-block! 'Plasma-full-block)
                        (set-block! 'Sea-water-full-block)]
                       [(= YYY 31)
                        #;(set-block! 'Plasma-shallow-block)
                        (set-block! 'Sea-water-shallow-block)])])))
    (traverse stage trav #:force-unsound-optimization? #t)
    (save-stage! stage)
    }

#;{begin ; one-off template: minimap update trick
    (require (submod hermits-heresy undocumented))

    ;(copy-all-save-files! #:from 'B02 #:to 'B00)
    (define stage (load-stage 'IoA 'B00))

    (define area (bitmap->area "minimap.bmp"))
    (define trav (traversal
                  (when (and (in-area? area)
                             (= YYY 56))
                    (set-block! 120))))
    (traverse stage trav #:force-unsound-optimization? #t)
    (save-stage! stage)
    }

#;{begin ; flatten an Iridescent BT
    (require (submod hermits-heresy undocumented))
    (copy-all-save-files! #:from 'B02 #:to 'B00)
    (define stage (load-stage 'BT3 'B00))

    (define trav (traversal
                  (cond
                    [(> YYY 20)
                     (set-block! 0)]
                    [(block-matches? 'Lava-surface-block
                                     'Lava-shallow-block
                                     'Lava)
                     (set-block! 'Grassy-Earth)])))
    (traverse stage trav #:force-unsound-optimization? #f)
    (save-stage! stage)
    }
