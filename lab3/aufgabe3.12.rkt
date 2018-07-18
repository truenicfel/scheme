;;; Aufgabe 12 ;;;
(define (double f)
  ; return a function, that executes f twice
  (lambda (n) (f (f n)))
  )

; square function (for testing)
(define (square n) (* n n))

; tests
(square 1)
((double square) 1)
(square 2)
((double square) 2)
(square 3)
((double square) 3)