#lang racket

; Adds the North and West borders to the new, cozier resort area.
; Use this script to review and tweak the area size before committing
; to rebuilding the (reflected!) resort.

(require hermits-heresy
         #;(only-in (submod hermits-heresy undocumented) YYY))

{begin
  (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

  (copy-all-save-files! #:from 'B02 #:to 'B00)

  (define dst (load-stage 'IoA 'B00))

  (define ph-resort-border-west-lo
    (make-platform-hills (generate-platform-layout 40 150)
                         #:x 560 #:z 290
                         #:peak-y 44))
  (define ph-resort-border-west-hi
    (make-platform-hills (generate-platform-layout 30 150)
                         #:x 560 #:z 290
                         #:peak-y 54))

  (define ph-resort-border-north-lo
    (make-platform-hills (generate-platform-layout 320 40)
                         #:x 400 #:z 262
                         #:peak-y 44))
  (define ph-resort-border-north-hi
    (make-platform-hills (generate-platform-layout 305 30)
                         #:x 400 #:z 262
                         #:peak-y 54))

  (define trav
    (traversal
     (cond
       [(in-platform-hills?! ph-resort-border-north-hi) #t]
       [(in-platform-hills?! ph-resort-border-west-hi) #t]
       [(in-platform-hills?! ph-resort-border-north-lo) #t]
       [(in-platform-hills?! ph-resort-border-west-lo) #t]
       )))

  (time (traverse dst trav #:force-unsound-optimization? #t))
  (save-stage! dst)
  }
