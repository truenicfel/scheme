; Aufgabe 2
; Summe der quadrate der beiden groessten zahlen von dreien

(define (quadratsumme a b c)
  (define (qs a b c)
    (if (< b c) (qqs a c b) (qqs a b c)))
  (define (qqs a b c)
    (+ (* a a) (* b b)))
  (if (< a b) (qs b a c) (qs a b c)))


; oder
(define (quadratsumme a b c)
  (let ((x (max a b))
        (y (max (min a b) c)))
    (+ (* x x) (* y y))))

; oder das gleiche ohne let aber mit Funktionen
(define (square x) (* x x))

(define (quadratsumme a b c)
    (+ (square (max a b)) (square (max (min a b) c))))

; oder
(define (quadratsumme a b c)
  (+ (square a) (square b) (square c) (- (square (min a b c)))))
 
