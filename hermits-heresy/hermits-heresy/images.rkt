#lang racket

(provide get-template-image save-template-image)

(require racket/runtime-path
         pict)

(define-runtime-path images-dir "images")

(define (load-image id)
  (bitmap (build-path images-dir (format "~a.bmp" id))))

(define-syntax-rule (load-images id ...)
  (make-hash (list (cons 'id  (load-image 'id))
                   ...)))

(define images (load-images IoA-background
                            IoA-mask))

(define/contract (save-template-image id)
  (-> symbol? any/c)
  (define pic (get-template-image id))
  (define bmp (pict->bitmap pic 'unsmoothed))
  (define filename (format "~a.bmp" id))
  (send bmp save-file filename 'bmp)
  (format "wrote to ~a" (build-path (current-directory) filename)))

(define/contract (get-template-image id)
  (-> symbol? pict?)
  (hash-ref images id
            (lambda () (error "Unknown image ID:" id))))
