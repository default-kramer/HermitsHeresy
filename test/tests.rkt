#lang racket

{module+ test
  (require "../NEW-API.rkt"
           (submod "../NEW-API.rkt" for-testing)
           rackunit)

  (define-syntax-rule (load kind path)
    (load-stage kind (string->path path)))

  ; 2024-05-23 looks correct to my eyes
  (let ([stage (load 'IoA "STGDAT01.001.bin")])
    (check-equal? (blocks-hash stage) '(8305176 9691690))
    (clear-area! stage 'all #:keep-items? #f)
    (repair-sea! stage 'all)
    (check-equal? (blocks-hash stage) '(5119984 1574160)))
  }