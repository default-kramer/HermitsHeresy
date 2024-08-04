#lang racket

{module+ test
  (require hermits-heresy
           (only-in "../hermits-heresy/hermits-heresy/NEW-API.rkt" bitmap->area area-contains?)
           (submod "../hermits-heresy/hermits-heresy/NEW-API.rkt" for-testing)
           rackunit)

  (let ([hill (bitmap->hill "images/hill1.bmp")]
        [expect #hash(
                 ; first row - left and right corners are fully transparent
                 ((0 . 0) . #f)
                 ((1 . 0) . 95) ; black
                 ((2 . 0) . 45) ; 100/2
                 ((3 . 0) . #f)
                 ; second row - always have a 255, so easily 0
                 ((0 . 1) . 0)
                 ((1 . 1) . 0)
                 ((2 . 1) . 0)
                 ((3 . 1) . 0)
                 ; third row - all black, opacity is irrelevant
                 ((0 . 2) . 95)
                 ((1 . 2) . 95)
                 ((2 . 2) . 95)
                 ((3 . 2) . 95)
                 ; fourth row - crosses the y=0 threshold
                 ((0 . 3) . 2)
                 ((1 . 3) . 1)
                 ((2 . 3) . 0)
                 ((3 . 3) . 0)
                 )])
    (for ([kvp (hash->list expect)])
      (let ([loc (car kvp)]
            [elevation (cdr kvp)])
        (check-equal? (hill-ref hill loc) elevation))))

  (let ([area (bitmap->area "images/STB-manual-build.bmp")])
    (check-equal? (area-bounds area)
                  (rect (xz 329 30) (xz 419 127)))
    (check-true (area-contains? area (xz 329 30)))
    (check-true (area-contains? area (xz 387 30)))
    (check-false (area-contains? area (xz 388 30)))
    (check-true (area-contains? area (xz 329 33)))
    (check-true (area-contains? area (xz 387 33)))
    (check-false (area-contains? area (xz 388 33)))
    (check-false (area-contains? area (xz 388 37)))
    (check-true (area-contains? area (xz 388 38)))
    (check-true (area-contains? area (xz 405 93)))
    (check-true (area-contains? area (xz 405 94)))
    (check-true (area-contains? area (xz 406 93)))
    (check-false (area-contains? area (xz 406 94))))
  }