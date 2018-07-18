; Aufgabe 10
(define (poly+ p1 p2)
  (cond ((null? p1) p2)
        ((null? p2) p1)
        (else (cons (+ (car p1) (car p2)) (poly+ (cdr p1) (cdr p2))))))

(define (poly-scale c p)
  (map (lambda (x) (* c x)) p))

(define (poly* p1 p2)
  (cond ((null? p1) '())
        ((null? p2) '())
        (else (poly+ (poly-scale (car p1) p2) (cons 0 (poly* (cdr p1) p2))))))

(define (poly/ p1 p2)
  (let loop ((r1 (reverse p1))
        (r2 (reverse p2))
        (l1 (length p1))
        (l2 (length p2))
        (d '()) ; we have p2 * (reverse d) + (reverse r1) = p1 as invariant
        )
        (cond ((< l1 l2) (list (reverse d) (reverse r1)))
              ((= 0 (car r2)) (loop r1 (cdr r1) l1 (- l2 1) d))
              (else
               (let ((f (/ (car r1) (car r2))))
                 (loop (cdr (poly+ r1 (poly-scale (- f) r2))) r2 (- l1 1) l2 (cons f d))))))) 
               
      