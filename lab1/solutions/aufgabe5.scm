; Aufgabe 5

; rekursiv
(define (binom n k)
  (if (= k 0)
      1
      (if (= k n)
	  1
	  (+
	    (binom (- n 1) (- k 1))
	    (binom (- n 1) k)))))


;iterativ: siehe Pascalsches Dreieck
                
(define (binom n k)
  ; first two local functions
  (define (next-list nk)
    (cons 1
          (let loop ((prev nk))
            (if (and (pair? prev) (pair? (cdr prev)))
                (cons (+ (car prev) (cadr prev))
                      (loop (cdr prev)))
                '(1)))))
  (define (binom-list n) ; liste n über 0 bis n über n)
    (let loop ((i 0)
               (ik '(1)))
      (display ik) (newline) ; debugging output
      (if (= i n)
          ik
          (loop (+ i 1) (next-list ik))))) 
  ; now we start:
  (let loop ((i 0)
             (nk (binom-list n)))
    (if (= i k) 
        (car nk)
        (loop (+ i 1) (cdr nk)))))
 
; example
(binom 8 3)

	  

