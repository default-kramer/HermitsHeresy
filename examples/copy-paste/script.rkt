#lang racket

(require hermits-heresy)

(println "starting script")

(save-dir "C:/Users/kramer/Documents/My Games/DRAGON QUEST BUILDERS II/Steam/76561198073553084/SD/")

(define copy-area (bitmap->area "copy-area.bmp"))
(define sw-rock-area (bitmap->area "sw-rock-to-move.bmp"))

{begin
  (define src (load-stage 'IoA "./FirstPossibleSave/STGDAT01.BIN"))
  (define copy-sel (selection src copy-area (list)))
  (define sw-rock-sel (selection src sw-rock-area '((translate -12 50))))
  (define count 0)

  (define dst (load-stage 'IoA 'B00))
  (define trav
    (traversal
     (with-selection [srcblock copy-sel]
       (set! count (+ 1 count))
       (when (block-matches? 0)
         (set-block! srcblock)))
     (with-selection [srcblock sw-rock-sel]
       (set! count (+ 1 count))
       (set-block! srcblock))))
  (time (traverse dst trav #:force-unsound-optimization? #t))
  (println (list "count:" count))
  }
