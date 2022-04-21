#lang racket
(require plot)

(define totalPoints 1e5)
(define initial (cons 0 0))
(define size 800)

(define (realRandom)
  (/ (random 4294967087) 4294967086.0))


(define (transform p xt yt xd yd)
  (let ((px (car p))
        (py (cdr p))
        (xtx (car xt))
        (xty (cdr xt))
        (ytx (car yt))
        (yty (cdr yt)))
       (cons (+ xd (* px xtx) (* py xty)) (+ yd (* px ytx) (* py yty)))))


(define (leaf p)
  (let ((r (realRandom)))
    (cond ((< r 0.05) (transform p (cons  (+ r r) r) (cons  0.00 (- r 0.08)) 0.10 0.10))
          ((< r 0.10) (transform p (cons  (+ 1 r) r) (cons -0.04 (- r 0.50)) 0.20 0.20))
          ((< r 0.50) (transform p (cons  (* r r) r) (cons  0.23 (- r 0.11)) 0.30 0.30))
          ((< r 0.70) (transform p (cons  (- 1 r) r) (cons  0.52 (- r 0.24)) 0.40 0.40))
          (else       (transform p (cons  (- r 1) r) (cons  0.70 0.80) 0.50 0.24)))))


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
(plot (points #:sym 'pixel #:color 'darkcyan
       (map (lambda(p) (list (car p) (cdr p))) pointsToPlot)))