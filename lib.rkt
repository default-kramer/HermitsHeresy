#lang racket

(provide draw-topography
         expand-topography
         basic-hill-expander
         topography->pict
         ; rethink:
         candidates-points
         steps->path todo
         point point-x point-y point-z
         )

(require pict
         (only-in racket/draw make-color))

{module typed typed/racket
  (provide (all-defined-out))

  ; X - if x1 < x2 then x1 is east of x2
  ; Y - if y1 < y2 then y1 has lower elevation
  ; Z - if z1 < z2 then z1 is north of z2
  (define-type Axis (U 'X 'Y 'Z))

  ; Let's delay validating coordinate ranges until you export the final build.
  ; So any integer is fine.
  (struct point ([x : Integer]
                 [y : Integer]
                 [z : Integer])
    #:transparent #:type-name Point)

  (define-type Block Positive-Byte)

  (struct scene ([blocks : (Immutable-HashTable Point Block)])
    #:transparent #:type-name Scene)

  (define-type XZ (Pairof Integer Integer))

  (define (neighbors [xz : XZ])
    (let ([x (car xz)]
          [z (cdr xz)])
      (list (cons (add1 x) z)
            (cons (sub1 x) z)
            (cons x (add1 z))
            (cons x (sub1 z)))))

  ; Topography defines the contour/elevation of a section of land,
  ; by storing the maximum Y coordinate (the "peak") of each XZ pair.
  ; So abbreviating to "top" can mean "just the top layer"
  (struct topography ([peaks : (Immutable-HashTable XZ Integer)])
    #:transparent #:type-name Topography)

  (define (top-contains? [top : Topography] [point : XZ])
    (hash-ref (topography-peaks top) point #f))

  (struct candidates ([points : (Immutable-HashTable XZ Integer)] ; value is distance from original topography
                      ;[min-distance : Integer] ; the min value in the hash
                      ;[max-distance : Integer] ; the max value in the hash
                      )
    #:transparent #:type-name Candidates)

  ; Returns the initial candidate set for the given topography,
  ; which will include all points having distance=1.
  (define (build-candidates [top : Topography])
    (: try-add (-> (Immutable-HashTable XZ Integer) (Listof XZ) (Immutable-HashTable XZ Integer)))
    (define (try-add points xzs)
      (if (empty? xzs)
          points
          (let* ([xz (car xzs)]
                 [points
                  (if (top-contains? top xz)
                      points ; already part of topography, do not add
                      (hash-set points xz 1))]); all distances are 1
            (try-add points (cdr xzs)))))
    (let loop : Candidates ([keys
                             : (Listof XZ)
                             (hash-keys (topography-peaks top))]
                            [points
                             : (Immutable-HashTable XZ Integer)
                             (hash)])
      (if (empty? keys)
          (candidates points)
          (loop (cdr keys)
                (try-add points (neighbors (car keys)))))))

  (define (expand-topography [orig-top : Topography]
                             ; choose func returns: (List XZ Height)
                             [choose-candidate : (-> Candidates (U #f (List XZ Integer)))])

    (: update-cands (-> Topography (Immutable-HashTable XZ Integer) (Listof XZ) Integer (Immutable-HashTable XZ Integer)))
    ; Add/update each xz to have distance, unless it already has an equal/lesser distance.
    ; Cascade shorter distances to neighbors as needed.
    ; Any xz already in the given topography will be skipped.
    (define (update-cands top points xzs distance)
      (if (empty? xzs)
          points
          (let* ([xz (car xzs)]
                 [existing (hash-ref points xz #f)]
                 [points (cond
                           [(top-contains? top xz)
                            points]
                           [(not existing)
                            (hash-set points xz distance)]
                           [(distance . < . existing)
                            ; We need to cascade to all neighbors
                            (update-cands top
                                          (hash-set points xz distance)
                                          (neighbors xz)
                                          (add1 distance))]
                           [else ; existing distance is shorter, no update needed
                            points])])
            (update-cands top points (cdr xzs) distance))))

    (: step (-> Topography Candidates Topography))
    (define (step [top : Topography] [cands : Candidates])
      (let ([choice (choose-candidate cands)])
        (if (not choice)
            top
            (let* ([height (second choice)]
                   [xz (first choice)]
                   [x (car xz)]
                   [z (cdr xz)]
                   ; remove new item from candidates...
                   [points (candidates-points cands)]
                   [distance (hash-ref points xz)] ; ... but grab the distance first!
                   [points (hash-remove points xz)]
                   ; add new item to topography
                   [top (topography (hash-set (topography-peaks top) xz height))]
                   ; now add/update candidates
                   [points (update-cands top points (neighbors xz) (add1 distance))])
              (step top (candidates points))))))

    (step orig-top (build-candidates orig-top)))

  (define (get-bounds [xzs : (Listof XZ)])
    (for/fold ([min-x : Integer 999999] ; TODO should improve this
               [min-z : Integer 999999]
               [max-x : Integer -999999]
               [max-z : Integer -999999])
              ([xz xzs])
      (let ([x (car xz)]
            [z (cdr xz)])
        (values (min min-x x)
                (min min-z z)
                (max max-x x)
                (max max-z z)))))

  (define (basic-hill-expander #:steps [steps : Integer]
                               #:slope [slope : Integer])
    (lambda ([candidates : Candidates])
      (let* ([points (candidates-points candidates)]
             ; key is XZ, value is distance from original topography
             [items (hash->list points)]
             ; sort such that shorter distances are closer to the front
             [item (argmin (lambda ([x : (Pairof XZ Integer)]) (cdr x)) items)]
             [xz (car item)]
             [distance-from-orig (cdr item)])
        (if (distance-from-orig . > . steps)
            #f ; stop expanding
            (list xz (* slope (- distance-from-orig)))))))

  ; Should return next ring... and that's all we need!?!
  ; As long as we store each ring, that fully defines the shape, right?
  (define (todo [ring : (Listof Point)]
                #:max-drop [max-drop : Nonnegative-Integer]
                #:min-drop [min-drop : Nonnegative-Integer])
    (define last-y (+ -2 (point-y (first ring))))
    (: recurse (-> (Listof Point) (Listof Point)))
    (define (recurse [ring : (Listof Point)])
      (if (empty? ring)
          (list)
          (let* ([w-choices '(1 2 2 3 3 4 4 5)]
                 [w (list-ref w-choices (random (length w-choices)))]
                 [w (min w (length ring))]
                 [delta-y (case (random 2)
                            [(0) -1]
                            [(1) 1]
                            [else (error "assert fail")])]
                 [_ (set! last-y (+ last-y delta-y))]
                 [blah (for/list : (Listof Point)
                         ([p (take ring w)])
                         (let* ([above-y (point-y p)]
                                [y (max last-y (- above-y max-drop))]
                                [y (min y (- above-y min-drop))])
                           (set! last-y y) ; Hmm...
                           (point (point-x p)
                                  y
                                  ; TODO how do we know which direction to grow?
                                  ; Hard-coded +1 z here isn't good:
                                  (+ 1 (point-z p)))))])
            (append blah (recurse (drop ring w))))))
    (recurse ring))

  (: steps->path (-> Point (Listof (U 'N 'S 'E 'W)) (Listof Point)))
  (define (steps->path start dirs)
    (if (empty? dirs)
        (list start)
        (let-values ([(dx dz)
                      (case (car dirs)
                        [(N) (values 0 -1)]
                        [(S) (values 0 1)]
                        [(E) (values 1 0)]
                        [(W) (values -1 0)])])
          (let ([next (point (+ dx (point-x start))
                             (point-y start)
                             (+ dz (point-z start)))])
            (cons start (steps->path next (cdr dirs)))))))
  } ; end of module

(require (submod 'typed))

(define (topography->pict top)
  (define peaks (topography-peaks top))
  (define keys (hash-keys peaks))
  (define-values (min-x min-z max-x max-z)
    (get-bounds keys))
  (define-values (min-y max-y)
    (let ([heights (hash-values peaks)])
      (values (apply min heights)
              (apply max heights))))
  (define (make-row z)
    (apply hc-append
           (for/list ([x (in-range min-x (add1 max-x))])
             (let ([item (hash-ref peaks (cons x z) #f)])
               (if item
                   (let* ([a 0]
                          [b (+ 1 (- max-y min-y))]
                          [c (+ 1 (- item min-y))]
                          [ratio (/ c b)]
                          [ratio (+ 0.4 (* 0.6 ratio))])
                     (filled-rectangle 1 1 #:draw-border? #f
                                       #:color (make-color 0 0 0 ratio)))
                   (blank 1 1))))))
  (apply vc-append (for/list ([z (in-range min-z (add1 max-z))])
                     (make-row z))))

(define-syntax-rule (draw-topography id ...)
  (let ()
    (define peaks (list))
    (define lines '#(id ...))
    (for ([z (in-range (vector-length lines))])
      (let ([chars (list->vector (string->list (format "~a" (vector-ref lines z))))])
        (for ([x (in-range (vector-length chars))])
          (let ([ch (vector-ref chars x)])
            (when (not (equal? #\- ch))
              (set! peaks (cons (cons (cons x z) 0) peaks)))))))
    (topography (make-immutable-hash peaks))))
