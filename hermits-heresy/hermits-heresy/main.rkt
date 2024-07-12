#lang racket

(provide save-dir
         stage->pict
         bitmap->hill
         bitmap->area
         area->hill2
         put-hill!
         ; rethink
         mark-writable
         load-stage
         protected-areas
         block
         copy-everything!
         decorate-peaks!
         simple?
         chisel
         save-stage!
         )

(require "NEW-API.rkt")
