#lang racket

; TODO
; TODO test if the chisel bits do anything for liquids!
; TODO


; Repairs the liquid in the aquarium.
; Assumes that the bitmap file is up-to-date.
; If you move the aquarium, use one-offs.rkt to update the bitmap.

(require hermits-heresy
         (submod hermits-heresy undocumented))

{begin
  (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

  (define aquarium-area (bitmap->area "resort-aquarium.bmp"))

  ;(copy-all-save-files! #:from 'B02 #:to 'B00)

  (define dst (load-stage 'IoA 'B00))

  (define trav
    (traversal
     (when (in-area? aquarium-area)
       (cond
         [(< YYY 35) #f]
         [(block-matches? 'Gravel-Block 'Masonry-Wall 'Modern-Masonry-Block
                          'Iron-Block 'Vault-Tile 'Sooty-Softwood)
          #f]
         [(< YYY 40)
          ;(set-block! 'Muddy-Water-full-block)
          ;(set-block! 'Old-Skool-Wall-Block)
          (set-block! 'Clear-water-full-block)
          (set-chisel! 'none)]
         [(= YYY 40)
          ;(set-block! 'Muddy-Water-surface-block)
          ;(set-block! 'Old-Skool-Wall-Block)
          (set-block! 'Clear-water-surface-block)
          (set-chisel! 'none)]))))

  (time (traverse dst trav #:force-unsound-optimization? #t))
  (save-stage! dst)
  }
