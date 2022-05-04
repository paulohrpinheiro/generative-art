#lang racket
(require plot)


(define size 800)


(define (plotThis? x y)
  (let ((v (modulo (+ (expt x 2) (expt y 3)) 50)))
    (= v 0)))


(define (generate xs i j)
  (cond ((= i 0) xs)
        (else
         (cond ((= j 0)
                (generate xs (- i 1) size))
               (else (cond ((plotThis? i j)
                            (generate (cons (cons i j) xs) i (- j 1)))
                           (else (generate xs i (- j 1)))))))))


(define (normalize p)
  (list (car p) (cdr p)))
 

(define pointsToPlot (generate '() size size))

(plot-width size)
(plot-height size)
(plot-background 'black)
(plot-foreground 'white)
(plot (points
       #:sym 'pixel #:color 'mediumturquoise
       (map (lambda(p) (normalize p)) pointsToPlot)))