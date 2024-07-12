#lang racket

(module+ test
  (require doc-coverage
           hermits-heresy)
  (check-all-documented 'hermits-heresy)
  )
