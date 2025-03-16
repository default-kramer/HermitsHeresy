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
         make-hill
         (rename-out [make-bitmap-sampler bitmap-sampler])
         combine-samplers function intersection union

         ; Provided for not much reason other than documentation convenience:
         sampler?
         grayscale-spec?
         normalize-spec?
         project-spec?

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
           make-interpolated-sampler

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
         (only-in "basics.rkt" sampler? make-rect xz)
         (only-in "bitmap-sampler.rkt" make-bitmap-sampler
                  grayscale-spec? normalize-spec? project-spec?)
         (only-in "combine-samplers.rkt"
                  combine-samplers function intersection union)
         (only-in "hill.rkt" make-hill)
         (only-in "interpolated-sampler.rkt" make-interpolated-sampler)
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
