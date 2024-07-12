#lang info

; This directory is the root of the Multi-Collection Package.
; This directory is where `raco pkg install` should be run.
; Each subdirectory is a collection in this package.
; See https://blog.racket-lang.org/2017/10/tutorial-creating-a-package.html
; and https://docs.racket-lang.org/pkg/index.html

(define collection 'multi)
(define deps '("base"))
(define build-deps '("scribble-lib"))
(define license 'GPL-3.0)
