;;; Aufgabe 7 ;;;
(define (union-set set1 set2)
  (if (null? set1)
      set2
      (if (member (car set1) set2)
          (union-set (cdr set1) set2)
          (cons (car set1) (union-set (cdr set1) set2))
          )
      )
  )

(union-set (list 3 4) (list 1 3 2))