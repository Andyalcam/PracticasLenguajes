#lang plai

(define-type AST
	[id (i symbol?)]
	[num (n number?)]
	[bool (b boolean?)]
	[op (f procedure?) (args (listof AST?))]
	[with (bindings (listof binding?)) (body AST?)]
	[with* (bindings (listof binding?)) (body AST?)]
	[fun (params (listof symbol?)) (body AST?)]
	[app (fun AST?) (args (listof AST?))]
)

(define-type Binding
	[binding (id symbol?) (value AST?)]

)

;; Ejercicio 1
;; parse: s-expression → AST
(define (parse sexp)
	(define (parse-op opsexp)
		(let (
			[operador (case (first opsexp)
				[(+) +]
				[(-) -]
				[(*) *]
				[(/) /]
				[(modulo) modulo]
				[(exp) exp]
				[(not) not])])
			(op operador (map parse (rest opsexp)))
		)
	)
	(define (parse-with withsexp)
    	(let (
    		[with (case (first withsexp)
    			[(with) with]
                [(with*) with*])])
      		(with (map (lambda (bdg) (binding (first bdg) (parse (second bdg))))
      				   (second withsexp))
      			  (parse (third withsexp)))
      	)
    )
    (define (parse-fun funsexp)
    	(if ((listof symbol?) (second funsexp))
    		(fun (second funsexp) (parse (third funsexp)))
    		(error "La lista de parametros NO es válida")
        )
    )
    (define (parse-app app-sexp)
    	(app (parse (second app-sexp)) (map parse (third app-sexp)))
    )
	(cond
		[(symbol? sexp)
			(case sexp
				[(T) (bool #t)]
				[(F) (bool #f)]
				[else (id sexp)]
			)]
		[(number? sexp) (num sexp)]
		[(list? sexp) (case (first sexp)
			[(+ - * / modulo exp not) (parse-op sexp)]
			[(with with*) (parse-with sexp)]
            [(fun) (parse-fun sexp)]
            [(app) (parse-app sexp)]
            )]
		;;[else (error "Tu expresión NO es válida en el lenguaje FWAE.")]
	)
)

;Ejercicio 2
;; subst: AST, symbol, AST → AST
;fun asociado directamente a app
(define (subst fwae-ast sub-id valor)
	(cond
	;La expresión ES un ID y puede ser el que estamos buscando
	[(id? fwae-ast) (if (= sub-id (id-i fwae-ast))
		valor 
		fwae-ast
		)] 
	;La expresión PUEDE tener IDs y hay que buscar en ellos a sub-id
	[(op? fwae-ast) (op (op-f fwae-ast) 
		(map(lambda (arg) (subst arg sub-id valor)) (op-args fwae-ast)))]
	;La expresión NO puede tener IDs
	[ else fwae-ast]
	)
)

;; Ejercicio 3
;; interp: AST → number U boolean U AST-fun
;;(define (interp fwae-ast)
;;	(cond
;;		[(with? fwae-ast) (let* (
;;			[bdgs (with-bindings fwae-ast)]
;;			[primeros-bdgs (reverse (rest (reverse-bdgs)))]
;;			[ultimo-bdg (last-bdg)])
;;			(foldl (lambda (bdg) 
;;				(subst (with-body fwae-ast) (binding-id bdg) (binding-value bdg)))
;;				(with-bindings fwae-ast)
;;				ultimo-bdg
;;				primeros-bdgs))]
;;	)
;;)




