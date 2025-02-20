#lang typed/racket

(provide Chisel chisel->mask)

(define-type Chisel (U 'none 'flat-lo 'flat-hi))

(: chisel->mask (-> Chisel Fixnum))
(define (chisel->mask chisel)
  (case chisel
    [(none) 0]
    ; * 1/3/5/7 - diagonal chisel N/E/S/W, matches (blueprint.chisel_status << 4)
    ; * 2/4/6/8 - diagonal chisel SW/SE/NW/NE
    ; * 9/a/b/c - concave chisel NW/SW/SE/NE
    [(flat-lo) #xe000]
    [(flat-hi) #xf000]))
