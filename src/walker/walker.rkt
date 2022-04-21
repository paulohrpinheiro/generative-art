#lang racket
(require plot)


(define totalPoints 1e5)
(define initial (cons 0 0))
(define size 800)


(define (normalize value)
  (modulo value size))


(define (walker point)
  (let (
	(r (random size))
	(x (car point))
	(y (cdr point)))
    (cons
      (normalize (+ y (quotient r 10)))
      (normalize (+ x (* r 10))))))


(define (genPoints pointsList i)
  (cond ((= i 0) pointsList)
	(else (let* ((lastPoint (car pointsList))
		     (x (car lastPoint))
		     (y (cdr lastPoint))
		     (newPoint (walker (cons x y)))
		     (i-minus-one (- i 1)))
		(genPoints (cons newPoint pointsList) i-minus-one)))))


(define pointsToPlot (genPoints (list initial) totalPoints))

(plot-width size)
(plot-height size)
(plot-background 'black)
(plot-foreground 'white)
(plot (points #:sym 'dot #:color 'OrangeRed
              (map (lambda(p) (list (car p) (cdr p))) pointsToPlot)))