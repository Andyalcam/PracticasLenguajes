#lang plai

;; filtra-lista: (listof any) procedure → (listof any)
(define (filtra-lista lista predicado)
	(if (empty? lista)
		empty
		(let* (
			[elem (first lista)]
			[res (if (predicado elem)
				elem
				null)]
			)
			(append
				(list res)
				(filtra-lista (rest lista) predicado)
			)
		)
	)
)


;; tipos-lista: (listof any) → (listof string)
(define (tipos-lista lista)
	(if (empty? lista)
		empty
		(let* (
			[elem (first lista)]
			[res (cond
				[(boolean? elem) "BOOLEAN"]
				[(number? elem) "NUMBER"]
				[(list? elem) "LIST"]
				[(string? elem) "STRING"]
				[(symbol? elem) "SYMBOL"]
				[(keyword? elem) "KEYWORD"]
				[(pair? elem) "PAIR"]
				[(char? elem) "CHAR"]
				[else "CHAR"])]
			)
			(append
				(list res)
				(tipos-lista (rest
					lista)))
		)
	)
)

;; raro?: number → boolean
(define (raro? a)
	(if (= a 14)
		#t
		#f
	)
)

;; descendente?: number* → boolean
(define descendente? (lambda nums
	(if (< (length nums) 2) 
		#t
		(if (> (first nums) (second nums))
			(apply
				descendente?
				(rest nums)
			)
			#f
		)
	)
))

;; Aux reversa de una lista
(define (reversa lista)
	(if (empty? lista)
		empty
		(append (reversa (cdr lista)) (list(car lista))
		)
	)
)
;; palindromo?: string → boolean
(define (palindromo? cadena)
	(let* (
		[rev (reversa (string->list cadena))]
		[ncad (list->string rev)]
		)
		(if (string=? cadena ncad)
			#t
			#f
		)
	)
)

;;Aux
(define (primoAux? num div cont)
	(let* (
		[resi (remainder num num)]
		[contAux cont]
		[divAux div]
		)
		(printf "resi")
		(cond
			[(= divAux 0) (if (= contAux 2)
				#t 
				#f)]
			[(= resi 0) (primoAux? num (- 1 num) (+ cont 1))]

			[else (primoAux? num (- 1 num) (cont))]
		)
	)
)
;; primo?: number → boolean
;(define (primo? num)
;	(let* (
;		[cont 0]
;		[resi (remainder num num)]
;		)
;		(if (= resi 0)
;			(+ cont 1)
;		)
;		(if (= cont 2)
;			#t
;			#f
;		)
;	)

	;(if (or (not(= (remainder num num) 0)) (not(= (remainder num 1) 0)))
	;	#t
	;	#f
	;)

	;(cond 
	;	[(= (remainder num num) 0) #t]
	;	[(= (remainder num 1) 0) #t]
	;	[else #f])
;)

;; num-comb-monedas: number → number
;;(define (num-comb-monedas cantidad)
;;	(let-values (
;;		[(cuantos_10 falta) (quotient/remainder cantidad 10)]
;;		[(casos_10) (* cuantos_10 10)])
;;		(cond
;;			[(=0 falta) casos_10]
;;			[(=1 falta) (add1 casos_10)]
;;			[(or (=2 falta) (=3 faltan)) (+ casos_10 2)]
;;			[(=4 falta) (+ casos_10 3)]
;;		)
;;	)
;;)

;;(define (calcula-moda hs nums)
;;	(define (frec-num hs ))
;;	(if (empty? nums)
;;		hs
;;		(let
;;			[num (first nums)]
;;		)
;;	)
	
;;)