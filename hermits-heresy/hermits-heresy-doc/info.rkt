#lang info

; This command will update the entire package, including the documentation:
;   raco setup --pkgs hermits-heresy
; This command will update the documentation only:
;   raco setup -l hermits-heresy-doc

(define collection "hermits-heresy-doc")
(define deps '("base"))
(define build-deps '("hermits-heresy" "scribble-lib" "racket-doc" "rackunit-lib" "sandbox-lib" "doc-coverage"))
(define scribblings '(("scribblings/main.scrbl" ())))
(define pkg-desc "documentation for Hermit's Heresy")
(define pkg-authors '(|Ryan Kramer|))

; I just copied turtle-insect's license because maybe I would copy some code too.
; I think GPL-3.0-or-later would be fine but I don't know.
(define license 'GPL-3.0-only)
