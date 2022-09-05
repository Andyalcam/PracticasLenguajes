#lang plai

;;1
;; filtra-lista: (listof any) procedure → (listof any)
;; Funcion que filtra una lista dada y regresa otra con los
;; los elementos que coincidadan con el predicado pasado por parametro
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

;;2
;; tipos-lista: (listof any) → (listof string)
;; Funcion que devuelve una lista con los tipos de datos
;; que tiene la lista pasada por parametro
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

;; Aux raroAux 
;; raroAux: number -> number
;; Funcion auxiliar que evalua las operaciones que harian, de ser
;; el caso, al parametro un numero raro
(define (raroAux num)
  (if (= num 0)
  	0
    (+ (expt (modulo num 10) 3)(raroAux (quotient num 10)))
  )
)

;;3
;; raro?: number → boolean
;; Funcion que compara a un numero con su evaluacion para verificar si es raro
(define (raro? num)
  (= (raroAux num) num)
)

;;4
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
;; reversaAux: (listof any) -> (listof any)
;; FUncion que devuelve una lista en reversa
(define (reversaAux lista)
	(if (empty? lista)
		empty
		(append (reversaAux (cdr lista)) (list(car lista))
		)
	)
)

;;5
;; palindromo?: string → boolean
;; Funcion que decide si una cadena es palindromo o no
(define (palindromo? cadena)
	(let* (
		[rev (reversaAux (string->list cadena))]
		[ncad (list->string rev)]
		)
		(if (string=? cadena ncad)
			#t
			#f
		)
	)
)

;;6
;; primo: number -> boolean
;;Numero primo
;;Funcion que decide si un numero es primo o no
(define (primo num contador acumula)
  (if (and(>= num contador) (=(remainder num contador) 0) )
  	(primo num (+ contador 1)(+ acumula 1))
  	(if (>= num contador)
  		(primo num (+ contador 1) acumula)
  		(if (= acumula 2)
  			#t
        #f
      )
    )
  )
)

;;7

;; Aux para promedio
;; sumaAux: (listof number) -> number
;; Funcion que devuelve la suma de todos los elementos
;; numericos de una lista
(define (sumaAux lista)
  (if (empty? lista)
    0
    (+(car lista)(sumaAux (cdr lista)))
  )
)

;;8
;; promedio: (listof number -> number)
;; Funcion que devuelve el promedio de una lista de numeros
(define (promedio lista)
  (if (empty? lista)
  	0
  	(/ (sumaAux lista) (length lista))
  )
)

;;9
;; rotar: (listof any) -> (listof any)
;;Funcion que rota la lista hacia la izquierda
(define (rotar lista)
  (if (null? lista)
      '()
      (append (cdr lista)
              (cons (car lista)
                    '()
                    )
              )   
      )
)
;;10
;;sucesion geometrica