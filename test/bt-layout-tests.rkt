#lang racket

; I used to have somewhat-tricky logic to infer the chunk layout by looking at
; the jaggedness of the bedrock. Then Sapphire figured out how to read the save file:
; https://github.com/default-kramer/HermitsHeresy/discussions/3#discussioncomment-10959611
;
; But these tests are still useful even though the logic is much simpler now.

(require rackunit
         (only-in (submod "../hermits-heresy/hermits-heresy/stage.rkt" for-testing)
                  file->chunk-layout))

(define-syntax-rule (check-layout filename expect)
  (let* ([path (build-path "fresh-topias" filename)]
         [layout (file->chunk-layout path)])
    (check-equal? layout expect)))

; Filename is the code you can give to Brownbeard to recreate the island.


; Large Coral Cay
(check-layout "4pvf1r91tm1.BIN" '((X X X X X X X X X X X)
                                  (X X X X X X X X X X X)
                                  (X X X X X X X X X X X)
                                  (X X X X X X X X X X X)
                                  (X X X X X X X X X X X)
                                  (X X X X X X X X X X X)
                                  (X X X X X X X X X X X)
                                  (X X X X X X X X X X X)
                                  (X X X X X X X X X X X)
                                  (X X X X X X X X X X X)
                                  (X X X X X X X X X X X)
                                  (_ _ _ _ _ _ _ _ _ _ _)
                                  (X X _ _ _ _ _ _ _ _ _)
                                  (X X _ _ _ _ _ _ _ _ _)))

; Medium Coral Cay
(check-layout "9y1ckuju01.BIN" '((X X X X X _ X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X _)
                                 (X X X X X X X X X _)
                                 (X X X X X X X _ _ _)
                                 (_ X X _ _ _ _ _ _ _)
                                 (_ X X _ _ _ _ _ _ _)))

; Small Coral Cay
(check-layout "a1i60xvhuw.BIN" '((_ _ _ _ X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (_ X X _ _ _ _)
                                 (_ X X _ _ _ _)))

; Large Defiled Isle
(check-layout "864haxtx44.BIN" '((X X X X X X X X X X X X _)
                                 (X X X X X X X X X X X X _)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (_ _ _ X X X X X X X X X X)
                                 (_ X X _ _ _ _ _ _ _ _ _ _)
                                 (_ X X _ _ _ _ _ _ _ _ _ _)))

; Medium Defiled Isle
(check-layout "8re998d0re.BIN" '((X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X _)
                                 (X X X X X X X X X _)
                                 (X X X X X X X X X _)
                                 (X X X X X X X X X _)
                                 (X X X X X X X X _ _)
                                 (_ X X _ _ _ _ _ _ _)
                                 (_ X X _ _ _ _ _ _ _)))

; Small Defiled Isle
(check-layout "263i3tma1w.BIN"   '((X X X X X X X)
                                   (X X X X X X X)
                                   (X X X X X X X)
                                   (X X X X X X X)
                                   (X X X X X X X)
                                   (X X X X X X X)
                                   (X X X X X X X)
                                   (_ X X _ _ _ _)
                                   (_ X X _ _ _ _)))

; Large Unholy Helm
(check-layout "93241ad3d7.BIN" '((X X X X X X X X X X X)
                                 (X X X X X X X X X X X)
                                 (X X X X X X X X X X X)
                                 (X X X X X X X X X X X)
                                 (X X X X X X X X X X X)
                                 (X X X X X X X X X X X)
                                 (X X X X X X X X X X X)
                                 (X X X X X X X X X X X)
                                 (X X X X X X X X X X X)
                                 (X X X X X X X X X X X)
                                 (X X X X X X X X X X X)
                                 (_ _ _ _ _ _ _ _ _ _ _)
                                 (X X _ _ _ _ _ _ _ _ _)
                                 (X X _ _ _ _ _ _ _ _ _)))

; Medium Unholy Helm
(check-layout "uceruxa3ad.BIN" '((X X X X X X X X _ _)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X _)
                                 (X X X X X X X X X _)
                                 (X X X X X X X X X _)
                                 (X X X X X X X X X _)
                                 (X X X X X X X X X _)
                                 (_ _ _ X X X _ _ _ _)
                                 (_ X X _ _ _ _ _ _ _)
                                 (_ X X _ _ _ _ _ _ _)))

; Small Unholy Helm
(check-layout "9wchcpc4rw.BIN" '((X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (_ X X _ _ _ _)
                                 (_ X X _ _ _ _)))

; Large Laguna
(check-layout "8ncxxdv8dt.BIN" '((_ _ X X X X X X X X X X _)
                                 (_ X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X X X X X X X X X X X)
                                 (X X X _ X X X X X X X X X)
                                 (_ X X _ _ _ _ _ _ _ _ _ _)
                                 (_ X X _ _ _ _ _ _ _ _ _ _)))

; Medium Laguna
(check-layout "max1mrpapc.BIN" '((X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (X X X X X X X X X X)
                                 (_ X X _ _ _ _ _ _ _)
                                 (_ X X _ _ _ _ _ _ _)))

; Small Laguna
(check-layout "ky0jk670d8.BIN" '((X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (X X X X X X X)
                                 (_ X X _ _ _ _)
                                 (_ X X _ _ _ _)))

; Medium Iridescent
(check-layout "13iapcevmr.BIN"   '((X X X X X X X X X X)
                                   (X X X X X X X X X X)
                                   (X X X X X X X X X X)
                                   (X X X X X X X X X X)
                                   (_ X X X X X X X X X)
                                   (X X X X X X X X X X)
                                   (X X X X X X X X X X)
                                   (X X X X X X X X X X)
                                   (X X X X X X X X X X)
                                   (_ _ _ X X X _ _ _ _)
                                   (_ X X _ _ _ _ _ _ _)
                                   (_ X X _ _ _ _ _ _ _)))
