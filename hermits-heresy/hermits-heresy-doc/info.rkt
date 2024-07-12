#lang info

; This command will update the entire package, including the documentation:
;   raco setup --pkgs hermits-heresy
; This command will update the documentation only:
;   raco setup -l hermits-heresy-doc

(define collection "hermits-heresy-doc")
(define deps '("base"))
(define build-deps '("hermits-heresy" "scribble-lib" "racket-doc" "rackunit-lib" "sandbox-lib"))
(define scribblings '(("scribblings/main.scrbl" (multi-page))))
(define pkg-desc "documentation for Hermit's Heresy")
(define pkg-authors '(|Ryan Kramer|))
(define license 'GPL-3.0)
