#lang plai

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


(define descendente? (lambda nums
	(if (< (length nums) 2) 
		#t
		(if (> (first nums) (second nums))
			(apply descendente? (rest nums))
			#f
		)
	)
))