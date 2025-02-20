#lang racket

(require hermits-heresy)

(define phills (make-platform-hills
                (generate-platform-layout 300 35)
                #:x 90 #:z 80
                #:peak-y 35
                #:tall-y -2
                #:short-y -2
                #:wall-block 'Umber
                #:wall-chisel 'flat-lo))

(define umber-mottler (build-mottler '[Umber 7]
                                     '[Lumpy-Umber 1]))

{begin
  (save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")
  (define source-slot 'B02)
  (define dest-slot 'B00)

  (copy-all-save-files! #:from source-slot #:to dest-slot)

  (define stage (load-stage 'BT1 dest-slot))

  (define trav!
    (traversal
     (cond
       [(in-platform-hills?! phills)
        ; Replace solid Umber with mottled Umber/Lumpy mixture
        (when (block-matches? 'Umber)
          (set-block! (umber-mottler)))]
       )))

  (traverse stage trav! #:force-unsound-optimization? #t)

  (save-stage! stage)
  }
