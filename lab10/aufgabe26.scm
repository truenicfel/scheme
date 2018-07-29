(define-syntax swap
  (syntax-rules ()
    ((swap a b)
     ; store b in tmp
     (let ((tmp b))
       ; values swapped
       (set! b a)
       (set! a tmp)
       )
     )
    )
  )

(define-syntax printValue
  (syntax-rules ()
    ((printValue name value)
     (begin
       (display name)
       (display ": ")
       (display value)
       (newline)
     )
     )
    )
  )

(define-syntax sort
  (syntax-rules ()
    ((sort a b)
     ; in case a > b then i swap them
     (if (> a b) (swap a b))
     )
    )
  )


(define a 20)
(define b 10)

(printValue "a" a)

(printValue "b" b)

(sort a b)

(printValue "a" a)

(printValue "b" b)