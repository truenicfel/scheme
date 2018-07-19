; Get value of given node
(define (value node)
  (car node)
  )

; Get left child of given node
(define (left node)
  (cadr node)
  )

; Get right child of given node
(define (right node)
  (cddr node)
  )

; Set value of given node
(define (value! node value)
  (set-car! node value)
  )

; Set left child of given node
(define (left! node childNode)
  (set-car! (cdr node) childNode)
  )

; Set right child of given node
(define (right! node childNode)
  (set-cdr! (cdr node) childNode)
  )

; Check if given node is empty
(define (empty? node)
  (null? node)
  )

; n will be inserted into the given tree
; (n is a number)
(define (insert tree n)
  (if (empty? tree)
      ; then construct a new child 
      (cons
       n
       (cons '() '()))
      ; else check if n is <, > or =
      (cond
        ; left
        ((< n (value tree))
         (left! tree (insert (left tree) n)) tree
         )
        ; right
        ((> n (value tree))
         (right! tree (insert (right tree) n)) tree
         )
        ; equal -> we stop
        ((= n (value tree))
         tree
         )
        )
      )
  )
; print value with given indentation
(define (print_value value indent)
  (do ((i 0 (+ i 1)))
       ((= i indent))
       (display " ")
       )
  (display value)
  (newline)
  )
                                   

(define (print_tree_indent tree indent)
  (if (not (empty? tree))
      (begin 
        (print_tree_indent (right tree) (+ indent 2))
        (print_value (value tree) indent)
        (print_tree_indent (left tree) (+ indent 2))
        )
  ))

(define (print_tree tree)
  (print_tree_indent tree 0)
  )


(define (smallest tree)
  (if (empty? tree)
      #f
      (let ((smallest_number (smallest (left tree))))
        (if smallest_number
          smallest_number
          (value tree)
          )
        )
      )
  )

(define (remove-smallest tree)
  (if (empty? tree)
      #f
      (let ((new_left_tree (remove-smallest (left tree))))
        (if new_left_tree
          (begin
            (left! tree new_left_tree)
            tree)
          (if (empty? (right tree))
              '()
              (right tree)
              )
          )
        )
      )
)


(define tree (cons
              5
              (cons '() '())
              )
  )

(set! tree (insert tree 2))
(set! tree (insert tree 7))
(set! tree (insert tree 8))
(set! tree (insert tree 12))
(set! tree (insert tree 11))


(print_tree tree)
(newline)
(display (smallest tree))
(newline)
(set! tree (remove-smallest tree))
(print_tree tree)

(newline)
(display (smallest tree))
(newline)
(set! tree (remove-smallest tree))
(print_tree tree)

(newline)
(display (smallest tree))
(newline)
(set! tree (remove-smallest tree))
(print_tree tree)
