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

(define tree (cons
              5
              (cons '() '())
              )
  )

(set! tree (insert tree 2))

(left tree)
(value tree)
(right tree)
tree

;(value test)
;(left test)
;(right test)
;
;(empty? (value test))
;(empty? (left test))
;(empty? (right test))
;
;; set root value to six
;(value! test 6)
;(value test)
;
;; 
;(left! test
;       (cons
;              7
;              (cons '() '())
;              ) 
;       )
;(left test)
;
;(right! test (cons
;              8
;              (cons '() '())
;              )
;        )
;(right test)

