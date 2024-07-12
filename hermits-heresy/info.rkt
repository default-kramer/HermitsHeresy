#lang info

; This directory is the root of the Multi-Collection Package.
; This directory is where `raco pkg install` should be run.
; Each subdirectory is a collection in this package.
; See https://blog.racket-lang.org/2017/10/tutorial-creating-a-package.html
; and https://docs.racket-lang.org/pkg/index.html

(define collection 'multi)
(define deps '("base"))
(define build-deps '("scribble-lib" "doc-coverage"))

; I just copied turtle-insect's license because maybe I would copy some code too.
; I think GPL-3.0-or-later would be fine but I don't know.
(define license 'GPL-3.0-only)
