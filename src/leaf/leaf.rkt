#lang racket
(require plot)


(define totalPoints 1e5)
(define initial (cons 0 0))
(define size 800)


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
    (cond ((< r 0.01) (transform p (cons  0.00  0.00) (cons  0.00 0.16) 0.00))
          ((< r 0.86) (transform p (cons  0.85  0.04) (cons -0.04 0.85) 1.60))
          ((< r 0.93) (transform p (cons  0.20 -0.26) (cons  0.23 0.22) 1.60))
          (else       (transform p (cons -0.15  0.28) (cons  0.26 0.24) 0.44)))))


(define (genPoints pointsList i)
  (cond ((= i 0) pointsList)
	(else (let* ((lastPoint (car pointsList))
		     (x (car lastPoint))
		     (y (cdr lastPoint))
		     (newPoint (leaf (cons x y)))
		     (i-minus-one (- i 1)))
		(genPoints (cons newPoint pointsList) i-minus-one)))))


(define pointsToPlot (genPoints (list initial) totalPoints))

(plot-width size)
(plot-height size)
(plot-background 'black)
(plot-foreground 'white)
(plot (points (map (lambda(p) (list (car p) (cdr p))) pointsToPlot) #:sym 'dot #:color 'darkolivegreen))