#lang typed/racket

(provide combine-samplers function intersection union
         combine-samplers:list
         Sampler-Combiner)

(require "basics.rkt"
         "ufx.rkt")

(define-type Operation (U '+ '-))

(define operation? (make-predicate Operation))

(: get-op (-> Operation (-> Fixnum Fixnum Fixnum)))
(define (get-op op)
  (case op
    [(+) ufx+]
    [(-) ufx-]))

(: get-fallback (-> Operation Fixnum))
(define (get-fallback op)
  (case op
    [(+ -) 0]))

(struct sampler-function ([operation : Operation]
                          [sampler : Fixnum-Sampler]
                          [fallback : (U #f Fixnum)])
  #:transparent
  #:type-name Sampler-Function)

(struct sampler-intersection ([operation : Operation]
                              [sampler : Fixnum-Sampler])
  #:transparent
  #:type-name Sampler-Intersection)

(struct sampler-union ([operation : Operation]
                       [sampler : Fixnum-Sampler]
                       ; not sure what these should be named:
                       [fallback-a : (U #f Fixnum)]
                       [fallback-b : (U #f Fixnum)])
  #:transparent
  #:type-name Sampler-Union)

(define-type Sampler-Combiner
  (U Sampler-Function Sampler-Intersection Sampler-Union))

(: combine (-> Fixnum-Sampler Sampler-Combiner Fixnum-Sampler))
(define (combine sampler arg)
  (define s1 (fixnum-sampler-func sampler))
  (define rect1 (fixnum-sampler-bounding-rect sampler))

  (match arg
    [(sampler-function op other fallback)
     (let ([f (get-op op)]
           [fallback (or fallback (get-fallback op))]
           [s2 (fixnum-sampler-func other)])
       (define (sample [xz : XZ])
         (let ([val (s1 xz)])
           (and val
                (f val (or (s2 xz) fallback)))))
       (fixnum-sampler sample rect1))]
    [(sampler-intersection op other)
     (let ([f (get-op op)]
           [s2 (fixnum-sampler-func other)]
           [rect2 (fixnum-sampler-bounding-rect other)])
       (define (sample [xz : XZ])
         (let ([val (s1 xz)])
           (and val
                (let ([val2 (s2 xz)])
                  (and val2 (f val val2))))))
       (fixnum-sampler sample (rect-intersect rect1 rect2)))]
    [(sampler-union op other fallback-a fallback-b)
     (let ([f (get-op op)]
           [fallback-a (or fallback-a (get-fallback op))]
           [fallback-b (or fallback-b (get-fallback op))]
           [s2 (fixnum-sampler-func other)]
           [rect2 (fixnum-sampler-bounding-rect other)])
       (define (sample [xz : XZ])
         (let ([v1 (s1 xz)]
               [v2 (s2 xz)])
           (and (or v1 v2)
                (f (or v1 fallback-a)
                   (or v2 fallback-b)))))
       (fixnum-sampler sample (rect-union rect1 rect2)))]
    [else
     (error "assert fail" arg)]))

(: combine-samplers:list (-> Fixnum-Sampler (Listof Sampler-Combiner) Fixnum-Sampler))
(define (combine-samplers:list sampler combiners)
  (let loop ([sampler sampler]
             [combiners combiners])
    (match combiners
      [(list) sampler]
      [(list x more ...)
       (loop (combine sampler x) more)])))

(: combine-samplers (-> Fixnum-Sampler Sampler-Combiner * Fixnum-Sampler))
(define (combine-samplers sampler . combiners)
  (combine-samplers:list sampler combiners))

(: validate-op (-> Any Operation))
(define (validate-op x)
  (cond
    [(operation? x) x]
    [(procedure? x) (validate-op (object-name x))]
    [else (error "invalid sampler operator:" x)]))

(define (function [op : (U Operation Procedure)]
                  [sampler : Fixnum-Sampler]
                  #:fallback [fallback : (U #f Fixnum) #f])
  (sampler-function (validate-op op) sampler fallback))

(define (intersection [op : (U Operation Procedure)]
                      [sampler : Fixnum-Sampler])
  (sampler-intersection (validate-op op) sampler))

(define (union [op : (U Operation Procedure)]
               [sampler : Fixnum-Sampler])
  ; since I don't know what to name these, let's not expose them to the public yet
  (let ([fallback-a #f]
        [fallback-b #f])
    (sampler-union (validate-op op) sampler fallback-a fallback-b)))
