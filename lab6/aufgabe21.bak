;
; TODO:
; - (display "error\n") durch geeignete error funktion ersetzen
;   mit vernünftigen fehlermeldungen
; - ref! sollte ein undefined zurückgeben
; 


; this is the memory for our scheme interpreter
; ----------------------------------------
(define MEM_SIZE 30)
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


; test area
; ----------------------------------------

(printMemory)

(define testNumber (i-number 13))

(define testPair (i-cons testNumber i-null))

(number->value testNumber)

(number->value (i-car i-null))

(printMemory)