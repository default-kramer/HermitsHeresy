#lang typed/racket

(provide expand-topography
         basic-hill-expander
         ; rethink:
         candidates-points
         steps->path todo
         point point-x point-y point-z
         )

(module+ test
  (require typed/rackunit))

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

(define (point->xz [p : Point])
  (ann (cons (point-x p) (point-z p)) XZ))

(define (neighbors [xz : XZ])
  (let ([x (car xz)]
        [z (cdr xz)])
    (list (cons (add1 x) z)
          (cons (sub1 x) z)
          (cons x (add1 z))
          (cons x (sub1 z)))))

(define (neighbors-diagonal [xz : XZ])
  (let ([x (car xz)]
        [z (cdr xz)])
    (list (cons x (add1 z))
          (cons (add1 x) (add1 z))
          (cons (add1 x) z)
          (cons (add1 x) (sub1 z))
          (cons x (sub1 z))
          (cons (sub1 x) (sub1 z))
          (cons (sub1 x) z)
          (cons (sub1 x) (add1 z)))))

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

(define (get-bounds [xzs : (Listof XZ)] #:padding [padding : Integer 0])
  (match xzs
    [(list xz more ...)
     (let-values ([(min-x min-z max-x max-z)
                   (for/fold ([min-x : Integer (car xz)]
                              [min-z : Integer (cdr xz)]
                              [max-x : Integer (car xz)]
                              [max-z : Integer (cdr xz)])
                             ([xz more])
                     (let ([x (car xz)]
                           [z (cdr xz)])
                       (values (min min-x x)
                               (min min-z z)
                               (max max-x x)
                               (max max-z z))))])
       (values (- min-x padding)
               (- min-z padding)
               (+ max-x padding)
               (+ max-z padding)))]
    [else
     (error "Cannot get-bounds of empty list")]))

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

; Ouch! There is a failing test explaining why the `shell`
; is not totally well-defined yet...
; Also, what is the list of points really for?
(struct ring ([points : (Listof Point)] ; the points that defined the ring
              [shell : (Listof XZ)] ; will be sorted
              [interior : (Setof XZ)])
  #:transparent #:type-name Ring)

(define (ring-inside? [ring : Ring] [p : Point])
  (let ([x (point-x p)]
        [z (point-z p)]
        [interior (ring-interior ring)])
    (set-member? interior (cons x z))))

(: points->ring (-> (Listof Point) (U #f Ring)))
(define (points->ring points)
  ; We are going to create an XZ bounding box that surrounds all the given
  ; points. Then we will determine the Group ID of each XZ inside that box.
  ; The given list of points (XZ only) are assigned a Group ID of 'wall.
  ; All other XZs within the box will be assigned an integer Group ID
  ; using a flood fill algorithm such that XZs not separated by a 'wall
  ; will all have the same Group ID.
  ; A group that reaches any edge of the bounding box is considered
  ; "outside" the ring, and a group that does not is considered "inside".
  (define-type GroupId (U 'unset 'wall Integer))
  (if (empty? points)
      #f
      (let*-values ([(min-x min-z max-x max-z)
                     ; Pad the bounding box by one in all directions.
                     ; This means that when we are done, if this is a proper ring
                     ; it must have only one outside group and only one inside group.
                     ; (Without this, walls could separate corners into distinct groups.)
                     (get-bounds (map point->xz points) #:padding 1)]
                    ; Mutable hash for computing Group IDs.
                    [(groups) (ann (make-hash) (Mutable-HashTable XZ GroupId))])
        (define (groups-get [x : Integer] [z : Integer])
          (hash-ref groups (cons x z) (lambda () 'unset)))
        (define (groups-set! [x : Integer] [z : Integer] [id : GroupId])
          (hash-set! groups (cons x z) id))

        (for ([point points])
          (groups-set! (point-x point) (point-z point) 'wall))

        (: flood! (-> Integer Integer Integer Void))
        (define (flood! x z group-id)
          (when (and (>= x min-x)
                     (<= x max-x)
                     (>= z min-z)
                     (<= z max-z))
            (let ([existing (groups-get x z)])
              (define recurse?
                (case existing
                  [(unset) (begin (groups-set! x z group-id) #t)]
                  [else #f]))
              (when recurse?
                (flood! (+ x -1) z group-id)
                (flood! (+ x 1) z group-id)
                (flood! x (+ z -1) group-id)
                (flood! x (+ z 1) group-id))))
          (void))
        (let ([group-id 1])
          (for ([x (in-range min-x (+ 1 max-x))])
            (for ([z (in-range min-z (+ 1 max-z))])
              (set! group-id (+ 1 group-id))
              (flood! x z group-id))))

        (define outside-group-ids : (Setof Integer) (set))
        (define (mark-outside! [x : Integer] [z : Integer])
          (let ([sts (groups-get x z)])
            (case sts
              [(unset) (error "Assert fail!")]
              [(wall) (void)]
              [else (set! outside-group-ids (set-add outside-group-ids sts))])))
        (for ([x (list min-x max-x)])
          (for ([z (in-range min-z (+ 1 max-z))])
            (mark-outside! x z)))
        (for ([z (list min-z max-z)])
          (for ([x (in-range min-x (+ 1 max-x))])
            (mark-outside! x z)))
        (define inside-group-ids
          (let* ([ids (filter integer? (hash-values groups) #;(vector->list vec))]
                 [ids (filter (lambda (i)
                                (not (set-member? outside-group-ids i)))
                              ids)])
            (list->set ids)))
        (and (= 1 (set-count outside-group-ids))
             (= 1 (set-count inside-group-ids))
             ; Now we know this is actually a ring
             (let ([interior (ann (set) (Setof XZ))])
               (for ([x (in-range min-x (+ 1 max-x))])
                 (for ([z (in-range min-z (+ 1 max-z))])
                   (let ([group-id (groups-get x z)])
                     (define inside? : Boolean
                       (case group-id
                         [(unset) (error "Assert fail!")]
                         [(wall) #t]
                         [else (set-member? inside-group-ids group-id)]))
                     (when inside?
                       (set! interior (set-add interior (cons x z)))))))
               (define (interior? [xz : XZ])
                 (set-member? interior xz))
               (define shell
                 (let ()
                   (define (shell? [a : (U Point XZ)])
                     (let ([xz (if (point? a)
                                   (point->xz a)
                                   a)])
                       (and (not (interior? xz))
                            (ormap interior? (neighbors xz)))))
                   (define start : XZ
                     ; Remember that we padded the bounding box by 1.
                     ; So we know there will be at least one shell? point
                     ; having z=min-z
                     (let loop ([x min-x])
                       (let ([xz (cons x min-z)])
                         (cond
                           [(> x max-x) (error "assert fail")]
                           [(shell? xz) xz]
                           [else (loop (+ 1 x))]))))
                   (: get-shell (-> XZ XZ (Listof XZ)))
                   (define (get-shell [curr : XZ] [prev : XZ])
                     (let* ([candidates (neighbors-diagonal curr)]
                            [candidates (filter shell? candidates)]
                            [next
                             (match candidates
                               [(list a b)
                                (cond
                                  [(equal? a prev) b]
                                  [(equal? b prev) a]
                                  [else a])]
                               [else (error "assert fail")])])
                       (cond
                         [(equal? start next)
                          (list curr)]
                         [else
                          (cons curr (get-shell next curr))])))
                   (get-shell start start)))
               (ring points
                     shell
                     interior))))))

(define (expand-ring [ring : Ring])
  (let* ([shell (ring-shell ring)]
         [next-points (map (lambda ([xz : XZ])
                             (point (car xz) 1 (cdr xz)))
                           shell)])
    (points->ring next-points)))

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

(: steps->path (-> Point (Listof (U 'N 'NE 'E 'SE 'S 'SW 'W 'NW)) (Listof Point)))
(define (steps->path start dirs)
  (if (empty? dirs)
      (list start)
      (let-values ([(dx dz)
                    (case (car dirs)
                      [(N) (values 0 -1)]
                      [(NE) (values 1 -1)]
                      [(E) (values 1 0)]
                      [(SE) (values 1 1)]
                      [(S) (values 0 1)]
                      [(SW) (values -1 1)]
                      [(W) (values -1 0)]
                      [(NW) (values -1 -1)])])
        (let ([next (point (+ dx (point-x start))
                           (point-y start)
                           (+ dz (point-z start)))])
          (cons start (steps->path next (cdr dirs)))))))

(define (handle-drawn-topography [lines : (Vectorof Any)])
  ; Handles both macros (typed and untyped)
  (let ()
    (define peaks : (Listof (Pairof XZ Integer)) (list))
    (for ([z (in-range (vector-length lines))])
      (let ([chars (list->vector (string->list (format "~a" (vector-ref lines z))))])
        (for ([x (in-range (vector-length chars))])
          (let ([ch (vector-ref chars x)])
            (when (not (equal? #\- ch))
              (set! peaks (cons (cons (cons x z) 0) peaks)))))))
    (topography (make-immutable-hash peaks))))

(define-syntax-rule (draw-topography id ...)
  (handle-drawn-topography '#(id ...)))

{module+ test
  (define-syntax-rule (top->ring id ...)
    (let* ([top (draw-topography id ...)]
           [points (topography-peaks top)]
           [points (map (lambda ([xz : XZ]) (point (car xz) 0 (cdr xz)))
                        (hash-keys points))])
      (points->ring points)))
  (define-syntax-rule (in?? ring xzs ...)
    (map (lambda ([xz : XZ])
           (ring-inside? ring (point (car xz) 0 (cdr xz))))
         '(xzs ...)))
  (let ([ring (top->ring --XX---
                         X-----X)])
    (check-false ring))
  (let ([ring (top->ring -X-
                         X-X
                         -X-)])
    (check-true (ring? ring))
    (when ring
      (check-equal? (in?? ring
                          (0 . 0) (1 . 0) (2 . 0)
                          (0 . 1) (1 . 1) (2 . 1)
                          (0 . 2) (1 . 2) (2 . 2))
                    (list #f #t #f
                          #t #t #t
                          #f #t #f))
      (check-equal? (ring-shell ring)
                    (map point->xz (steps->path (point 1 99 -1)
                                                '(SE SE SW SW NW NW NE))))))
  (let ([ring (top->ring --XX--
                         -X--X-
                         X----X
                         -XXX-X
                         ----X-)])
    (check-true (ring? ring))
    (when ring
      (define steps '(E SE SE SE S SW SW NW W W NW NW NE NE))
      (check-equal? (ring-shell ring)
                    (map point->xz (steps->path (point 2 99 -1) steps)))
      (check-equal? (in?? ring
                          (0 . 0) (1 . 0) (2 . 0) (3 . 0) (4 . 0) (5 . 0)
                          (0 . 1) (1 . 1) (2 . 1) (3 . 1) (4 . 1) (5 . 1)
                          (0 . 2) (1 . 2) (2 . 2) (3 . 2) (4 . 2) (5 . 2)
                          (0 . 3) (1 . 3) (2 . 3) (3 . 3) (4 . 3) (5 . 3)
                          (0 . 4) (1 . 4) (2 . 4) (3 . 4) (4 . 4) (5 . 4))
                    (list #f #f #t #t #f #f
                          #f #t #t #t #t #f
                          #t #t #t #t #t #t
                          #f #t #t #t #t #t
                          #f #f #f #f #t #f))))
  (let ([ring (top->ring --XX--
                         -X--X-
                         X-----
                         -XXX-X
                         ----X-)])
    (check-false ring))
  ; Here's a tricky one... It's not clear what I would want to even do here.
  ; The motivation for rings was to support cliff/plateau creation, and I don't know
  ; how I would create a cliff for the following topography algorithmically.
  ; So maybe the following should not be a ring.
  ; (Also remember: using shell points rather than border points means
  ;  we could now relax the "one interior group" restriction because we
  ;  no longer care if the topography is completely "filled in".
  ;  e.g. A solid 5x5 square of points has a perfectly usable shell.)
  (fail "The shell concept is not fully baked. Commented-out code that follows would crash:")
  #;(let ([ring (top->ring --XXXXX
                           ----X-X
                           --XX--X
                           ---XXXX)])
      (check-true (ring? ring)))
  } ; end test submodule
