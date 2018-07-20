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
      ; else error (dummy)
      (display "error\n")
      )
  )

; access specific element in memory (bounds checked!)
; ----------------------------------------
(define (ref pointer offset)
  (if (and (> pointer 0) (< pointer MEM_SIZE))
      (let
          ; get size from memory
          ((size (vector-ref memory (- pointer 1))))
        ; offset must be in [0; size[
        (if
         (and (>= offset 0) (< offset size))
         ; this is the desired case: return the requested memory field
         (vector-ref memory (+ pointer offset))
         ; else error (dummy)
         (display "error\n")
        )
        )
      ; else error (dummy)
      (display "error\n")
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
         ; else error (dummy)
         (display "error\n")
        )
        )
      ; else error (dummy)
      (display "error\n")
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
(define (new-number value)
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
      ; its not a number :( -> error (dummy)
      (display "error\n")
      )
  )

; create new symbol object with initial value
; returns pointer to newly allocated object
; ----------------------------------------
(define (new-symbol value)
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
      ; its not a number :( -> error (dummy)
      (display "error\n")
      )
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
(define testNumber (new-number 25))
(number->value testNumber)
(number->value i-true)
(number->value i-false)
(number->value i-null)
(number->value i-undefined)

(define testSymbol (new-symbol 'blub))
(symbol->value testSymbol)
(symbol->value i-true)
(symbol->value i-false)
(symbol->value i-null)
(symbol->value i-undefined)

;(define adressOne (malloc 5))
;(define adressTwo (malloc 3))


;memory