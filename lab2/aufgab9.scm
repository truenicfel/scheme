;;; Aufgabe 9 ;;;
(define (flatten inputList)
  (if (list? (car inputList))
      ; recursion
      (flatten (car inputList))
      (car inputList)
      )
  )

(flatten ’((a b) ((c) d (e f g))))