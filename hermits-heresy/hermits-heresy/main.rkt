#lang racket

(provide save-dir
         stage->pict
         bitmap->hill
         bitmap->area
         area->hill2
         put-hill!
         load-stage
         protected-areas
         block
         copy-all-save-files!
         decorate-peaks!
         simple?
         chisel
         save-stage!
         get-template-image
         save-template-image
         )

(require "NEW-API.rkt"
         "images.rkt")
