; Aufgabe 9

(define (flatten l)
  (cond
    ((null? l) l)
    ((pair? l) (append (flatten (car l)) (flatten (cdr l))))
    (else (list l))))

; oder mit "Akkumulator"

; mit Hilfsfunktion collect
(define (collect all tree) ; collect all elements of tree in all
  (cond ((null? tree) all)
        ((pair? tree) (collect (collect all (cdr tree)) (car tree)))
        (else (cons tree all))))

(define (flatten l) (collect '() l))

; das gleiche mit lokalem letrec  
(define (flatten l)
  (letrec ((collect
            (lambda (all tree) ; collect all elements of tree in all
              (cond ((null? tree) all)
                    ((pair? tree) (collect (collect all (cdr tree)) (car tree)))
                    (else (cons tree all))))))
    (collect '() l)))


(flatten '())
(flatten 5)
(flatten '(1 2 3))
(flatten '( 1 . 2))
(flatten '((2 3 ) () (() 4)))