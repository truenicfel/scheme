;;; Aufgabe 13 ;;;
(define (compose f g)
  ; return a function, that executes g first and then f
  (lambda (n) (f (g n)))
  )

; square function (for testing)
(define (square n) (* n n))

; double function (for testing)
(define (double n) (+ n n))

; tests
(double 1)
((compose square double) 1)
(double 2)
((compose square double) 2)
(double 3)
((compose square double) 3)