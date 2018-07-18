;;; Aufgabe 14 ;;;
(define (polynom-function p)
  ; else a + x ( <recursion> )
  (lambda (x)
    (if (null?  p)
        ; then return value of p
        0
        (+
         (car p)
         (*
          x
          ((polynom-function (cdr p)) x)
          )
         )
        )
    )
  )

; test
(define p1 (polynom-function '(1 2 3)))
(p1 2)

;(lambda (x) (+ (* 2 (expt x 2)) 1)) ; 2x^2 + 1