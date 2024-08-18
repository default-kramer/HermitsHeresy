#lang racket

(require rackunit
         (only-in (submod "../hermits-heresy/hermits-heresy/infer-topia-layout.rkt" for-testing)
                  get-layout print-runs)
         (only-in (submod "../hermits-heresy/hermits-heresy/NEW-API.rkt" for-testing)
                  get-bedrock-chunks))

(define-syntax-rule (check-layout filename expect)
  (let* ([path (build-path "fresh-topias" filename)]
         [chunks (get-bedrock-chunks path)]
         [layout (get-layout chunks)])
    (check-equal? expect layout)))

(define (show-runs filename)
  (let* ([path (build-path "fresh-topias" filename)]
         [chunks (get-bedrock-chunks path)])
    (print-runs chunks)))

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
                                  (_ X X _ _ _ _ _ _ _ _)
                                  (_ X X _ _ _ _ _ _ _ _)))

; Medium Coral Cay
; Okay, already we have a test case that needs us to put the first two runs,
; which have length 5 and 4, into a single row like [X X X X X _ X X X X]
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
; Useful test case because the first chunk has a "jaggedness" of 4 relative
; to the chunk below it. (I guessed that jaggedness > 2 was impossible.)
; AHA, look closer! There is probably a way we can improve these calcluations.
; Here are the top and bottom profiles, lined up:
#;(#f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f X X X X X X X X X X X X X X X)
#;(#f #f #f #f #f #f #f #f #f #f #f #f X  X  X  #f X  X X X X X X X X X X X X X X X)
; The 3 lone Xs in the middle aren't actually that relevant to jaggedness here.
; Maybe we should redefine profile to drop "islands" in the middle?
; I think I need to see more test cases before I make that change.
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
                                 (_ X X _ _ _ _ _ _ _ _)
                                 (_ X X _ _ _ _ _ _ _ _)))

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
