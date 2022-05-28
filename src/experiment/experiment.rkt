#lang racket
(require plot)


(define totalPoints 1e6)
(define initial (cons 0 0))
(define size 3000)


(define (realRandom)
  (/ (random 4294967087) 4294967086.0))


(define (transform p xt yt yd)
  (let ((px (car p))
        (py (cdr p))
        (xtx (car xt))
        (xty (cdr xt))
        (ytx (car yt))
        (yty (cdr yt)))
    (cons (+ (* px xtx) (* py xty)) (+ yd (* px ytx) (* py yty)))))


(define (leaf p)
  (let ((r (realRandom)))
    (cond ((< r 0.11) (transform p (cons  0.00  0.00) (cons  0.00  0.16) 0.1))
          ((< r 0.35) (transform p (cons  0.85  0.04) (cons -0.04 -0.85) 0.2))
          ((< r 0.50) (transform p (cons  0.20 -0.26) (cons  0.32  0.99) 0.3))
          ((< r 0.75) (transform p (cons -0.20  0.26) (cons -0.23 -0.22) 0.5))
          ((< r 0.90) (transform p (cons  0.10  0.56) (cons  0.03  0.52) 0.7))
          (else       (transform p (cons -0.15  0.28) (cons  0.26 -0.24) 0.11)))))


(define (genPoints pointsList i)
  (cond ((= i 0) pointsList)
        (else (let* ((lastPoint (car pointsList))
                     (x (car lastPoint))
                     (y (cdr lastPoint))
                     (newPoint (leaf (cons x y))))
                (genPoints (cons newPoint pointsList) (sub1 i))))))


(define pointsToPlot (genPoints (list initial) totalPoints))

(plot-width size)
(plot-height size)
(plot-background 'black)
(plot-foreground 'white)
(plot (points #:sym 'dot #:color 'indigo
              (map (lambda(p) (list (car p) (cdr p))) pointsToPlot)))