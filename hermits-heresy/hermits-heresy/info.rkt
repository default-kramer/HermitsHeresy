#lang info
(define collection "hermits-heresy")
(define deps '("base"))
(define implies '())
(define build-deps '())
;(define scribblings '(("scribblings/plisqin.scrbl" (multi-page))))
(define pkg-desc "Power Tools for DQB2")
(define version "0.1")
(define pkg-authors '(|Ryan Kramer|))

; I just copied turtle-insect's license because maybe I would copy some code too.
; I think GPL-3.0-or-later would be fine but I don't know.
(define license 'GPL-3.0-only)
