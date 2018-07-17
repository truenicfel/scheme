; Aufgabe 6


; Faktorisierung durch Teilen (Knuth: The Art ... Seite 380)
; so woertlich wie moeglich uebernommen. Allerdings ist die
; Folge der dk primitiver als noetig. Aber der Algorithmus
; ist sowieso nur fuer kleine N effizient.

; (factor_by_div 47100194747174954)
; ==> (999863 281 53 47 23 19 11 7 2)
; innere Schleife laeuft bis dk=1001 durch alle ungeraden Zahlen.
;
; (factor_by_div (* 999863 999983))
; ==> dauert ewig
; innere Schleife laeuft bis dk = 999863 durch alle ungeraden Zahlen
; also etwa 1000 mal laenger wie das vorige Problem. zusaetzliche
; kleinere Faktoren veraendern die Laufzeit nicht (kaum).
; Laufzeit ~ max (zweitgroesster Primfaktor, Wurzel(groesster Primfaktor))

(define (factor_by_div X) 
  (define (dk+1 dk)            
    (if (= dk 2) 3 (+ dk 2))) 
  (define p '())                
  (define dk 2)                 
  (define n X)                      
  (let A2 ()             
    (if (= n 1) p             
	(let A3 ()                   
	  (define q (quotient n dk))
	  (define r (modulo n dk))  
	  (if (zero? r)    ; A4          
	      (begin       ; A5           
		(set! p (cons dk p)) 
		(set! n q)           
		(A2))                
	      (if (> q dk) ; A6           
		  (begin               
		    (set! dk (dk+1 dk))
		    (A3))              
		  (begin   ; A7            
		    (set! p (cons n p))
		    p)))))))           
