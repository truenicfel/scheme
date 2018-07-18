; Aufgabe 7
; Mengen von Zahlen als Listen

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((member (car set1) set2) (union-set (cdr set1) set2))
        (else (cons (car set1) (union-set (cdr set1) set2)))))