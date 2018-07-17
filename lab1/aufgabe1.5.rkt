;;; Aufgabe 5 ;;;
(define (recursive n k)
  (cond ((= k 1) n)
        ((= n k) 1)
        (else (+ (recursive (- n 1) (- k 1)) (recursive (- n 1) k))))
)

(recursive 5 3)
(recursive 1 1)
(recursive 5 1)
(recursive 5 5)
(recursive 3 2)


(define (iterative n)
  (if (< n 3) 1 
      ; make array[n+1], start at i = 3; i++
      (do ((array (make-vector (+ n 1) 1))
           (i 3 (+ i 1)))
        ; stop at i = n+1; return array[n]
        ((= i (+ n 1)) (vector-ref array n))
        ; body
        (vector-set! array i (+ (vector-ref array (- i 1)) (* 2 (vector-ref array (- i 2))) (* 3 (vector-ref array (- i 3)))))
        )
  )
)

(iterative 0)
(iterative 1)
(iterative 2)
(iterative 3)
(iterative 4)