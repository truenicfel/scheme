; Aufgabe 4

(define (f n)
  (cond ((= n 1) 1)
        ((= n 1) 1)
        ((= n 2) 1)
        (else (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

; oder
(define (f n)
  (if (< n 3) 
      1
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))


; iterativ
(define (f-iterative n)
  (if (< n 3) 1
   (do ((i 3 (+ i 1))
        (fi-1 1 fi)
        (fi-2 1 fi-1)      
        (fi 6 (+ fi (* 2 fi-1) (* 3 fi-2))))
     ((= i n) fi)
     )))

; oder

(define (f-iterative n)
  (define (f-aux i fi fi-1 fi-2)
    (if (< i n)
	(f-aux (+ i 1) (+ fi (* 2 fi-1) (* 3 fi-2)) fi fi-1)
        fi))
  (f-aux 2 1 1 1)
)

; oder

(define (f-iterative n)
  (let loop ((i 2)
	     (fi 1)
	     (fi-1 1)
	     (fi-2 1))
    (if (< i n)
	(loop (+ i 1) (+ fi (* 2 fi-1) (* 3 fi-2)) fi fi-1)
	fi)))
