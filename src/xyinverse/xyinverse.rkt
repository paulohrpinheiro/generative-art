#lang racket
(require plot)


(define totalPoints 1e5)
(define initial (cons 1 1))
(define size 800)


(define (realRandom)
  (/ (random 4294967087) 4294967086.0))


(define (leaf p)
  (let ((x (car p))
        (y (cdr p)))
    (cons (+ x y) (- y x))))


(define (genPoints pointsList i)
  (cond ((= i 0) pointsList)
	(else (let* ((lastPoint (car pointsList))
		     (x (car lastPoint))
		     (y (cdr lastPoint))
		     (result (leaf (cons x y)))
                     (newPoint (cons (- i x) (- y i)))
		     (i-minus-one (- i 1)))
		(genPoints (cons newPoint pointsList) i-minus-one)))))


; To get to this shape, I've been through these before:
;
; (list (+ r2 r1 (car p)) (* (+ 1 r2) (cdr p)))))
; (list (+ r1 (log (abs (car p)))) (* (+ 0.2 r1) (log (abs (cdr p)))))))
; (list (* r1 (log (abs (car p)))) (* r2 (log (abs (cdr p)))))))
(define (normalize p)
  (let ((r1 (realRandom)))
     (list (* r1 (car p)) (+ 1 (cdr p)))))
 

(define pointsToPlot (genPoints (list initial) totalPoints))

(plot-width size)
(plot-height size)
(plot-background 'black)
(plot-foreground 'white)
(plot (points
        #:sym 'dot #:color 'red
       (map (lambda(p) (normalize p)) pointsToPlot)))