#lang plai

;;1
;; Funcion que filtra una lista dada y regresa otra con los
;; los elementos que coincidadan con el predicado pasado por parametro
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


;;2
;; Funcion que devuelve una lista con los tipos de datos
;; que tiene la lista pasada por parametro
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

;; Aux raroAux 
;; Funcion auxiliar 
(define (raroAux num)
  (if (= num 0)
  	0
    (+ (expt (modulo num 10) 3)(raroAux (quotient num 10)))
  )
)
;;3
;; raro?: number → boolean
(define (raro? num)
  (= (raroAux num) num))


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
(define (reversa lista)
	(if (empty? lista)
		empty
		(append (reversa (cdr lista)) (list(car lista))
		)
	)
)

;;5
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


;;6
;;Numero primo
;;(num 1 0)
(define (primo num contador acumula)
  (if (and(>= num contador) (=(remainder num contador) 0)   )
      (primo num (+ contador 1)(+ acumula 1))

      (if (>= num contador)
          (primo num (+ contador 1) acumula)

          (if(= acumula 2)
             #t
             #f
             )
          )
   )
)



;;7
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

;; Aux para promedio
(define (sumaAux lista)
  (if (empty? lista)
    0
    (+(car lista)(sumaAux (cdr lista)))
  )
)

;;8
(define (promedio lista)
  (if (empty? lista)
  	0
  	(/ (sumaAux lista) (length lista))
  )
)

;;8
;;(define (calcula-moda hs nums)
;;	(define (frec-num hs ))
;;	(if (empty? nums)
;;		hs
;;		(let
;;			[num (first nums)]
;;		)
;;	)
	
;;)

;;(define (calcula-moda hs nums)
;;  (if (empty? nums)
;;      hs
;;      (let (
;;            [num (first nums)]
;;            [val (hash-ref hs num #f)]
;;            (if (not (eq? val #f)))
;;            (hash-set! hs num (add1 val))
;;            (hash-set! hs num 1)
;;            )
;;          )
;;   )
;; )


;;9
;;Rota la lista hacia la izquierda
(define (rotate-left LIST)
  (if (null? LIST)
      '()
      (append (cdr LIST)
              (cons (car LIST)
                    '())
              )
      )
)


;;10
;;sucesion geometrica