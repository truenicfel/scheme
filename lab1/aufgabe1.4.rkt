;;; Aufgabe 4 ;;;
(define (recursive n)
  (if (>= n 3)
       ; then
       (+ (recursive (- n 1)) (* 2 (recursive (- n 2))) (* 3 (recursive (- n 3))))
       ; else
       1)

)

(recursive 0)
(recursive 1)
(recursive 2)
(recursive 3)
(recursive 4)

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