;;; Aufgabe 8 ;;;
(define (union-set set1 set2)
  (cond
    ((null? set1) set2)
    ((null? set2) set1)
    (else
     (cond
       ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
       ((> (car set1) (car set2)) (cons (car set2) (union-set set1 (cdr set2))))
       (else (union-set set1 (cdr set2)))
       )
     )
    )
  )

(union-set (list 1 2 3 4 5) (list 1 2 3 12))