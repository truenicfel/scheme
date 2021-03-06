;
; TODO:
; 

; MEMORY MANAGEMENT
; --------------------------------------------------------------------------------

; this is the memory for our scheme interpreter
; ----------------------------------------
(define MEM_SIZE 300)
(define memory (make-vector MEM_SIZE))
; index of the first free element in memory
(define free 0)

; memory allocation function
; ----------------------------------------
(define (malloc n)
  ; check if we exceed the memory size
  (if (and
        (<= (+ free n 1) MEM_SIZE)
        (> n 0)
        )
      (let ((result (+ free 1)))
        ; store size in first element
        (vector-set! memory free n)
        ; compute new free
        (set! free (+ result n))
        ; return is the index of the first element
        result
        )
      ; error
      (error "Either memory is full or allocated size is 0 or less!")
      )
  )

; access specific element in memory (bounds checked!)
; ----------------------------------------
(define (ref pointer offset)
  (if (and (>= pointer 0) (< pointer MEM_SIZE))
      (let
          ; get size from memory
          ((size (vector-ref memory (- pointer 1))))
        ; offset must be in [0; size[
        (if
         (and (>= offset 0) (< offset size))
         ; this is the desired case: return the requested memory field
         (vector-ref memory (+ pointer offset))
         ; else error
         (error "The specified offset is to big or to small!")
        )
        )
      ; else error
      (error "The specified pointer (address) does not exist in memory!")
      )
  )

; set specific element in memory (bounds checked!)
; ----------------------------------------
(define (ref! pointer offset value)
  (if (and (> pointer 0) (< pointer MEM_SIZE))
      (let
          ; get size from memory
          ((size (vector-ref memory (- pointer 1))))
        ; offset must be in [0; size[
        (if
         (and (>= offset 0) (< offset size))
         ; this is the desired case: return the requested memory field
         (vector-set! memory (+ pointer offset) value)
         ; else error
         (error "The specified offset is to big or to small!")
        )
        )
      ; else error
      (error "The specified pointer (address) does not exist in memory!")
      )
  )

; get tag of given pointer
; ----------------------------------------
(define (tag pointer)
  (ref pointer 0)
  )

; create new number object with initial value
; returns pointer to newly allocated object
; ----------------------------------------
(define (i-number value)
  ; malloc 2 fields and store pointer
  (let((pointer (malloc 2)))
    (ref! pointer 0 'number)
    (ref! pointer 1 value)
    pointer
    )
  )

; access number object at given adress
; not a number object will cause an error
; ----------------------------------------
(define (number->value pointer)
  ; check if it is a number
  (if (eqv? (tag pointer) 'number)
      ; its a number! -> return the first value behind tag
      (ref pointer 1)
      ; its not a number :( -> error
      (error "The specified pointer does not contain a number!")
      )
  )

; create new symbol object with initial value
; returns pointer to newly allocated object
; ----------------------------------------
(define (i-symbol value)
  ; malloc 2 fields and store pointer
  (let((pointer (malloc 2)))
    (ref! pointer 0 'symbol)
    (ref! pointer 1 value)
    pointer
    )
  )

; access symbol object at given adress
; not a symbol object will cause an error
; ----------------------------------------
(define (symbol->value pointer)
  ; check if it is a number
  (if (eqv? (tag pointer) 'symbol)
      ; its a number! -> return the first value behind tag
      (ref pointer 1)
      ; its not a symbol :( -> error
      (error "The specified pointer does not contain a symbol!")
      )
  )

; readable memory dump
; ----------------------------------------
(define (printMemory)
  (display "Memory Contents:")
  (newline)
  (display "----------------------------------------")
  (newline)
  (printRecord 0)
  )


(define (printRecord pointer)
  (if (and (<= pointer MEM_SIZE) (> (vector-ref memory pointer) 0))
      (let ((length (vector-ref memory pointer))) 
        (display "address: ")
        (display pointer)
        (newline)
        (display "   length: ")
        (display length)
        (newline)
        (display "   data: ")
        (do ((i 1 (+ i 1)))
          ((> i length))
          (display (vector-ref memory (+ pointer i)))
          (display ", ")
          )
        (newline)
        (printRecord (+ pointer length 1))   
        )
      )
  )

; construct a new pair
; a pair looks as follows
; ----------------------------------------

(define (i-cons head tail)
  ; malloc 3 fields and store pointer
  (let((pointer (malloc 3)))
    (ref! pointer 0 'pair)
    (ref! pointer 1 head)
    (ref! pointer 2 tail)
    pointer
    )
  )

; access car of pair object at given adress
; not a pair object will cause an error
; ----------------------------------------
(define (i-car pointer)
  ; check if it is a number
  (if (eqv? (tag pointer) 'pair)
      ; its a pair! -> return the car
      (ref pointer 1)
      ; its not a pair :( -> error
      (error "The specified pointer does not contain a pair!")
      )
  )

; access cdr of pair object at given adress
; not a pair object will cause an error
; ----------------------------------------
(define (i-cdr pointer)
  ; check if it is a number
  (if (eqv? (tag pointer) 'pair)
      ; its a pair! -> return the cdr
      (ref pointer 2)
      ; its not a pair :( -> error
      (error "The specified pointer does not contain a pair!")
      )
  )

; prints error message and stops the program
; ----------------------------------------
(define (error message)
  (display message)
  (newline)
  (exit)
  )

; function to define a static record
; ----------------------------------------
(define (staticRecord record)
  (let ((pointer (malloc 1)))
    (ref! pointer 0 record)
    pointer
    )
  )


; static records
; ----------------------------------------
; true record
(define i-true (staticRecord 'true))
; false record
(define i-false (staticRecord 'false))
; null record
(define i-null (staticRecord 'null))
; undefined record
(define i-undefined (staticRecord 'undefined))
; epsilon record
(define i-epsilon (staticRecord 'epsilon))


; INPUT AND OUTPUT
; --------------------------------------------------------------------------------

; Convert expression to i-expression
; (Convert scheme data structure into our data structure)
; ----------------------------------------
(define (expr->i-expr expression)
  (cond
    ; pair
    ; -----
    ((pair? expression)
     ; create internal pair
     (i-cons
      (expr->i-expr (car expression))
      (expr->i-expr (cdr expression))
      )
     )
    ; number
    ; -----
    ((number? expression)
     (i-number expression)
     )
    ; symbol
    ; -----
    ((symbol? expression)
     (i-symbol expression)
     )
    ; boolean
    ; -----
    ((boolean? expression)
     (if expression i-true i-false)
     )
    ; null
    ; -----
    ((null? expression)
     i-null
     )
    (else
     i-undefined
     )
    )
  )

; Calls read from scheme to read an expression
; and convert it to i-expr.
; ----------------------------------------
(define (i-read)
  (expr->i-expr (read))
  )

; Convert a local data structure into a scheme
; data structure.
; ----------------------------------------
(define (i-expr->expr pointer)
  (cond
    ; pair
    ; -----
    ((eqv? (tag pointer) 'pair)
     ; create internal pair
     (cons
      (i-expr->expr (i-car pointer))
      (i-expr->expr (i-cdr pointer))
      )
     )
    ; number
    ; -----
    ((eqv? (tag pointer) 'number)
     (number->value pointer)
     )
    ; symbol
    ; -----
    ((eqv? (tag pointer) 'symbol)
     (symbol->value pointer)
     )
    ; boolean
    ; -----
    ((eqv? (tag pointer) 'true)
     #t
     )
    ((eqv? (tag pointer) 'false)
     #f
     )
    ; null
    ; -----
    ((eqv? (tag pointer) 'null)
     '()
     )
    ; undefined
    ; -----
    ((eqv? (tag pointer) 'undefined)
     (if #f #f)
     )
    )
  )

; Display a given local data structure.
; ----------------------------------------
(define (i-display pointer)
  (display (i-expr->expr pointer))
  (newline)
  )


; BINDINGS
; --------------------------------------------------------------------------------

; Create a new binding in memory.
; Params are pointers to objects in memory.
; ----------------------------------------
(define (i-binding symbol value compose)
  ; malloc 4 fields 
  (let((pointer (malloc 4)))
    ; store the binding tag
    (ref! pointer 0 'bind)
    ; store pointer to symbol (variable name)
    (ref! pointer 1 symbol)
    ; store pointer to value (variable value)
    (ref! pointer 2 value)
    ; next binding
    (ref! pointer 3 compose)
    pointer
    )
  )

; Get value from binding. Recursively searches
; for the variable given by symbol param. Starts
; at give binding (pointer).
; ----------------------------------------
(define (binding->value symbol binding)
  (cond
    ; is a binding?
    ((eqv? (tag binding) 'bind)
     (if
      (eqv?
       (symbol->value symbol)
       (symbol->value (ref binding 1)))
      ; we found the value! load and return it
      (ref binding 2)
      ; we did not find the value! keep on searching...
      (binding->value symbol (ref binding 3))
      )
     )
    ; is epsilon?
    ((eqv? (tag binding) 'epsilon)
     ; return false -> value not found
     #f
     )
    (else
     ; error case
     (error "The given pointer does not point to a binding!")
     )
    )
  )

; Create new environment with given binding
; ----------------------------------------
(define (i-environment binding)
  ; malloc 2 fields 
  (let((pointer (malloc 2)))
    ; store the environment tag
    (ref! pointer 0 'environment)
    ; store pointer to binding
    (ref! pointer 1 binding)
    pointer
    )
  )

; Global environment
; ----------------------------------------
(define global-environment
  ; create empty environment
  (i-environment i-epsilon)
  )

; Get binding from environment
; ----------------------------------------
(define (environment->binding environment)
  ; check if environment is an environment
  (if (eqv? (tag environment) 'environment)
      (ref environment 1)
      (error "This is not an environment!")
      )
  )

; Set binding in environment
; ----------------------------------------
(define (environment-set! environment binding)
  ; check if environment is an environment
  (if (eqv? (tag environment) 'environment)
      (ref! environment 1 binding)
      (error "This is not an environment!")
      )
  )

; Add binding in environment
; ----------------------------------------
(define (add-variable variable value environment)
  ; check if environment is an environment
  (if (eqv? (tag environment) 'environment)
      ; create new binding from variable + value + old-binding
      ; and add it as new binding
      (environment-set!
       environment
       (i-binding variable value (environment->binding environment))
       )
      (error "This is not an environment!")
      )
  )

; Search for the value of the variable
; ----------------------------------------
(define (variable->value variable environment)
  ; check if environment is an environment
  (if (eqv? (tag environment) 'environment)
      ; get value of variable in the environment
      (binding->value variable (environment->binding environment))
      (error "This is not an environment!")
      )
  )

; test area
; ----------------------------------------

(printMemory)

; first binding x -> 10
;(define firstBinding (i-binding (i-symbol 'x) (i-number 10) i-epsilon))
; second binding y -> 20
;(define secondBinding (i-binding (i-symbol 'y) (i-number 20) firstBinding))

; third binding z -> 30
(define thirdBinding (i-binding (i-symbol 'z) (i-number 30) i-epsilon))
; fourth binding a -> 40
(define fourthBinding (i-binding (i-symbol 'a) (i-number 40) thirdBinding))

(define testSymbol (i-symbol 'x))
(define testNumber (i-number 1))

(environment-set! global-environment fourthBinding)
(add-variable testSymbol testNumber global-environment)
(variable->value (i-symbol '123) global-environment)
(printMemory)