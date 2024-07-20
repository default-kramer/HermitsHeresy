#lang racket

(require hermits-heresy)

; Step 1 - Create template images
#;(begin
    (save-template-image 'IoA-background)
    (save-template-image 'IoA-mask))

; Step 2 - Import template images into paint.net.
; Use paint.net to draw a hill
; NOTES:
; * "Aliased rendering" is almost always what you want
; * (- 95 (quotient (max red green blue) 2))
; Result - we have now output hill.bmp

; Step 3 - Use bitmap->hill and put-hill!
(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

(copy-all-save-files! #:from 'B02 #:to 'B00)

(define B00 (load-stage 'IoA 'B00))

(define hill (bitmap->hill "hill.bmp"))
(put-hill! B00 hill (block 'Chert))

(save-stage! B00)

; Step 4 - Load it in DQB2 and look around!
; If it looks good, maybe you want to copy the result
; to a permanent (non-ephemeral) save slot.
; Or if it doesn't look how you want, adjust your paint.net work,
; re-export hill.bmp, and run the script again.
; In other words, repeat steps 2-3 until you are satisifed with the result.
