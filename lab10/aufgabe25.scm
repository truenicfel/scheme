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