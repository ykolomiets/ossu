#lang racket

(provide (all-defined-out)) 

(define (sequence low high stride)
  (if (> low high)
      (list)
      (cons low (sequence (+ low stride) high stride))))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (let ([i (remainder n (length xs))])
              ;(car (list-tail xs i)))]))
              (list-ref xs i))]))

(define (stream-for-n-steps s n)
  (if (<= n 0)
      (list)
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

(define (funny-number-stream)
  (letrec ([f (lambda (x)
                (let ([elem (if (= 0 (remainder x 5)) (- x) x)])
                  (cons elem (lambda () (f (+ x 1))))))])
    (f 1)))

(define (dan-then-dog)
  (letrec ([dan (lambda () (cons "dan.jpg" dog))]
           [dog (lambda () (cons "dog.jpg" dan))])
    (dan)))

(define (stream-add-zero s)
  (letrec ([f (lambda (s)
             (let ([pr (s)])
               (cons (cons 0 (car pr)) (lambda () (f (cdr pr))))))])
    (lambda () (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (let ([pr (cons (list-nth-mod xs n) (list-nth-mod ys n))])
                  (cons pr (lambda () (f (+ n 1))))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([vlen (vector-length vec)]
           [f (lambda (n)
                (if (= n vlen)
                    #f
                    (let ([elem (vector-ref vec n)])
                      (if (and (pair? elem) (equal? v (car elem)))
                          elem
                          (f (+ n 1))))))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([cache (make-vector n)]
        [next-pos 0]
        [set-in-cache (lambda (pr)
                        (begin
                          (vector-set! cache next-pos pr)
                          (set! next-pos (remainder (+ next-pos 1) n))
                        )
                       )])
    (lambda (v)
      (let ([cached (vector-assoc v cache)])
        (if cached
            cached
            (let ([pr (assoc v xs)])
              (if pr
                  (begin (set-in-cache pr) pr)
                  #f)))))))

(define-syntax while-less
  (syntax-rules (do)
    [(while-less e1 do e2)
     (letrec ([l e1]
              [f (lambda ()
                   (if (< e2 l)
                       (f)
                       #t))])
       (f))]))