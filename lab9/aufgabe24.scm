;
; TODO:
; - i-define lacks error checking (exactly 2 arguments expected!)
; - i-lambda lacks error checking 
; 

; MEMORY MANAGEMENT
; --------------------------------------------------------------------------------

; this is the memory for our scheme interpreter
; ----------------------------------------
(define MEM_SIZE 3000)
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
  ; check if it is a symbol
  (if (eqv? (tag pointer) 'symbol)
      ; its a symbol! -> return the first value behind tag
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
    ; lambda
    ; -----
    ((eqv? (tag pointer) 'lambda)
     (list
      'lambda
      (i-expr->expr (lambda->arglist pointer))
      (i-expr->expr (lambda->body pointer))
      )
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

; PRIMITIVE FUNCTIONS
; --------------------------------------------------------------------------------

; Create new function in memory.
; function: function to store.
; ----------------------------------------
(define (i-primitive function)
  ; malloc 2 fields and store pointer
  (let((pointer (malloc 2)))
    ; store the primitive tag
    (ref! pointer 0 'primitive)
    ; store pointer to function
    (ref! pointer 1 function)
    pointer
    )
  )

; Get executable code from given location.
; pointer: gives the location of the function
; Stops in case pointer doesn't point to a function.
; ----------------------------------------
(define (primitive->function pointer)
  ; check if it is a primitive
  (if (eqv? (tag pointer) 'primitive)
      ; its a primitive! -> return the function value behind tag
      (ref pointer 1)
      ; its not a primitive :( -> error
      (error "The specified pointer does not contain a primitive!")
      )
  )

; Useful function to map a primitive function to
; a name in the global environment. Creates the function
; using i-primitive.
; name: name of the variable in the global environment
; function: function that gets mapped to the name
; ----------------------------------------
(define (add-primitive name function)
  ; create new primitive which stores function
  (let
      ((functionLocation (i-primitive function)))
    ; add variable to global environment 
    (add-variable (i-symbol name) functionLocation global-environment)
    )
  )

; Primitive function: i-plus
; environment: Here the function will look up variable values
; values: list of parameters (these will get added up)
; ----------------------------------------
(define (i-plus environment values)
  (i-number
   (let loop ((sum 0)
              (value values))
     (if (eqv? (tag value) 'pair)
         (loop (+ sum (number->value (i-eval environment (i-car value))))
               (i-cdr value))
         sum)))
  )
; add the function to global environment
(add-primitive '+ i-plus)

; Primitive function: i-minus
; environment: Here the function will look up variable values
; values: list of parameters (these will get added up)
; ----------------------------------------
(define (i-minus environment values)
  ; check if values is a list
  (if
   (eqv? (tag values) 'pair)
   (i-number
    ; evaluate first argument and store it to startValue
    (let loop ((startValue (number->value (i-eval environment (i-car values))))
                           (value (i-cdr values)))
               (if (eqv? (tag value) 'pair)
                   (loop (- startValue (number->value (i-eval environment (i-car value))))
                         (i-cdr value))
                   startValue))) 
    )
   )
; add the function to global environment
(add-primitive '- i-minus)

; Primitive function: i-define
; environment: Here the function will look up variable values
; values: list of parameters (these will get added up)
; ----------------------------------------
(define (i-define environment values)
  (if (eqv? (tag values) 'pair)
      (let ((parameterOne (i-car values))
            (parameterTwo (i-car (i-cdr values))))
        (add-variable
         ; pointer to symbol object
         parameterOne
         ; evaluate parameter two
         (i-eval environment parameterTwo)
         ; provide environment for evaluation of param two
         environment
         )
        i-undefined
        )
      (error "Not a pair!")
      )
  )

; add the function to global environment
(add-primitive 'define i-define) 

; LAMBDA
; --------------------------------------------------------------------------------

; Create a new lambda in memory.
; environment: environment in which the lambda will be executed
; body: body of the lambda expression as list
; argumentList: arguments as a list (local variables in the lambda)
; ----------------------------------------
(define (new-lambda argumentList body environment)
  ; malloc 4 fields and store pointer
  (let((pointer (malloc 4)))
    ; store the lambda tag
    (ref! pointer 0 'lambda)
    ; store pointer to argumentList
    (ref! pointer 1 argumentList)
    ; store pointer to body
    (ref! pointer 2 body)
    ; store pointer to environment
    (ref! pointer 3 environment)
    pointer
    )
  )

; Get the argument list from lambda.
; pointer: points to lambda in local memory 
; ----------------------------------------
(define (lambda->arglist pointer)
  ; check if it is a lambda
  (if (eqv? (tag pointer) 'lambda)
      ; its a lambda! -> return the argumentList behind tag
      (ref pointer 1)
      ; its not a lambda :( -> error
      (error "The specified pointer does not contain a lambda!")
      )
  )


; Get the body from lambda.
; pointer: points to lambda in local memory 
; ----------------------------------------
(define (lambda->body pointer)
  ; check if it is a lambda
  (if (eqv? (tag pointer) 'lambda)
      ; its a lambda! -> return body
      (ref pointer 2)
      ; its not a lambda :( -> error
      (error "The specified pointer does not contain a lambda!")
      )
  )

; Get the environment from lambda.
; pointer: points to lambda in local memory 
; ----------------------------------------
(define (lambda->environment pointer)
  ; check if it is a lambda
  (if (eqv? (tag pointer) 'lambda)
      ; its a lambda! -> return environment
      (ref pointer 3)
      ; its not a lambda :( -> error
      (error "The specified pointer does not contain a lambda!")
      )
  )

; Create new lambda and return pointer to it.
; environment: points to environment
; values: list containing argument list and body of lambda call
; ----------------------------------------
(define (i-lambda environment values)
  ; check if its a pair
  (if (eqv? (tag values) 'pair)
      (new-lambda
       ; get pointer to list of parameters
       (i-car values)
       ; get pointer to body
       (i-car (i-cdr values))
       ; pass environment
       environment
       )
      )
  )
(add-primitive 'lambda i-lambda)

; EVAL AND APPLY
; --------------------------------------------------------------------------------

; evaluate an expression in a given environment
; environment: obvious
; expression: obvious
; ----------------------------------------
(define (i-eval environment expression)
  (cond
    ; expression is a number!
    ((eqv? (tag expression) 'number)
     ; simply return the pointer to the number object
     expression
     )
    ; expression is a pair!
    ((eqv? (tag expression) 'symbol)
     ; perform lookup in environment
     ; store result in value
     (let ((value
            (variable->value expression environment)))
       ; check if result is valid
       (if value
           ; return value
           value
           ; #f was returned
           (error "The variable does not exist!")
           )
       )
     )
    ; expression is a pair!
    ((eqv? (tag expression) 'pair)
     ; first element in list
     (i-apply
      ; the environment
      environment
      ; the function
      (i-eval environment (i-car expression))
      ; argument list
      (i-cdr expression)
      )
     )
    (else
     (error "i-eval can't handle the given type")
     )
    )
  )

; Prepare environment with arguments given in arguments.
; Return a new environment.
; paramer-list: list of params defined with the function
; expression-list: list of expressions provided with the function call
; call-environment: the environment in which the function is called
; environment: the environment where the function was defined
; ----------------------------------------
(define
  (add-parameters parameter-list expression-list call-environment environment)
  (let ( ; load if lists are empty
        (parametersNull (eqv? (tag parameter-list) 'null))
        (expressionsNull (eqv? (tag expression-list) 'null))
        )
    (cond
      ; both lists not empty
      ((and (not parametersNull) (not expressionsNull))
       ; add variable to given environment
       (add-variable
         ; get variable name
         (i-car parameter-list)
         ; evaluate variable value
         (i-eval call-environment (i-car expression-list))
         environment
         )
       ; call myself with one element less in both lists
       (add-parameters
        (i-cdr parameter-list)
        (i-cdr expression-list)
        call-environment
        environment
        )
       )
      ; both lists empty (stop criteria for recursion)
      ((and parametersNull expressionsNull)
       environment
       )
      (else
       (error "Bad syntax in function call (incorrect number of arguments!") 
       )
      )
    )
  )

; Create a new environment for lambda call.
; ----------------------------------------
(define (new-lambda-environment param-list expr-list call-env def-env)
  (add-parameters param-list
                  expr-list
                  call-env
                  (i-environment (environment->binding def-env))
                  )
  )


; Evaluate a given function. Calls the appropriate
; primitive function.
; environment: environment in which the function will be executed
; function: pointer to function object in our local memory
; argumentList: arguments to the function called
; ----------------------------------------
(define (i-apply environment function argumentList)
  ; we expect a function here!
  (cond
    ((eqv? (tag function) 'primitive)
     ; it's a function
     ((primitive->function function) environment argumentList)
     )
    ((eqv? (tag function) 'lambda)
     ; it's a lambda
     ; this is the environment in that the function will be executed
     (let ((executeEnvironment (new-lambda-environment
                         (lambda->arglist function)
                         argumentList
                         environment
                         (lambda->environment function)
                         )
                        )
           )
       ; evaluate the body in the given environment
       (i-eval executeEnvironment (lambda->body function))
       )
     )
    (else
     ; it's not a function
     (error "Expected a procedure...")
     )
    )
  )

; STARTER CODE
; --------------------------------------------------------------------------------

(define (read-eval-print return)
  (define (i-exit env values) (return 0))
  (add-primitive 'exit i-exit)
  (let loop ()
    (newline)
    (display "i-scheme> ")
    (i-display (i-eval global-environment (i-read)))
    (loop))
  )

(define (i-scheme)
  (display "This is i-scheme version 1.0")
  (call-with-current-continuation read-eval-print)
  (display "Bye!")
  (newline)
)

(i-scheme)