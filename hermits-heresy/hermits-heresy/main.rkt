#lang racket

(provide save-dir
         bitmap->hill
         put-hill!
         load-stage
         copy-all-save-files!
         save-stage!
         block
         find-block-name
         get-template-image
         save-template-image
         bitmap->area
         protect-area!
         traverse
         build-mottler
         (rename-out [make-selection selection])
         make-platform-hills
         generate-platform-layout

         ; Traversal lang
         (rename-out [compile-traversal traversal])
         block-matches? set-block!
         with-selection
         in-hill?
         in-platform-hills?!
         )

(module+ undocumented
  (provide area->hill2
           chisel
           decorate-peaks!
           simple?
           stage->pict

           ; traversal
           HHEXPR YYY XXX ZZZ
           in-area? set-chisel!

           ; For alternate traversal
           traverse-lambda
           argbox-block
           set-argbox-block!
           ))

(require "NEW-API.rkt"
         "selection.rkt"
         "build-mottler.rkt"
         (only-in "platform-layout.rkt" generate-platform-layout)
         (only-in "platform-hills.rkt" make-platform-hills)
         (only-in "traversal/untyped-traversal.rkt"
                  compile-traversal block-matches? set-block! in-hill?
                  set-chisel! in-area?
                  HHEXPR YYY XXX ZZZ)
         (only-in "traversal/traversal.rkt"
                  argbox-block set-argbox-block!)
         "traversal/trav-macros.rkt"
         "images.rkt")
