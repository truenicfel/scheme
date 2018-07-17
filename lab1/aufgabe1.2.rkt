;;; Aufgabe 2 ;;;
(define aufgabe2 (lambda (x y z)
 ; content of aufgabe2
 (define maxXY (if (> x y) x y))
 (define minXY (if (< x y) x y))
 (+ (expt maxXY 2) (expt (if (> minXY z) minXY z) 2)) 
))

(aufgabe2 1 2 3)
(aufgabe2 1 3 2)
(aufgabe2 2 1 3)
(aufgabe2 2 3 1)
(aufgabe2 3 1 2)
(aufgabe2 3 2 1)
(aufgabe2 2 1 2)
(aufgabe2 2 2 2)
(aufgabe2 3 1 3)