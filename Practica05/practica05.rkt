;; Alvarado Camacho Andrea		318064343

#lang plai

(define-type AST
	[id (i symbol?)]
	[num (n number?)]
	[bool (b boolean?)]
	[op (f procedure?) (args (listof AST?))]
	[op-bool (f symbol?) (larg AST?) (rarg AST?)]
	[branch (test AST?) (then AST?) (else AST?)]
	[multi−branch (conds (listof branch-cond?)) (else AST?)]
	[with (bindings (listof binding?)) (body AST?)]
	[with* (bindings (listof binding?)) (body AST?)]
	[fun (params (listof symbol?)) (body AST?)]
	[lempty]
	[lcons (l AST?) (r AST?)]
	[lcar (lst AST?)]
	[lcdr (lst AST?)]
	[app (fun AST?) (args (listof AST?))]
)

(define-type Binding
	[binding (id symbol?) (value AST?)]
	[branch-cond (test AST?) (then AST?)]
)

(define-type Environment
	[mtSub]
	[aSub (name symbol?) (value AST?) (bSub Environment?)]
)

(define-type CFWAEL-Value
	[numV (n number?)]
	[boolV (b boolean?)]
	[listV (l (listof CFWAEL-Value?))]
	[closureV (param (listof symbol?)) (body AST?) (env Environment?)])

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
        (app (if (fun? (parse (second app-sexp)))
                (parse (second app-sexp))
                (error "ERROR" (second app-sexp) " no es una funcion")
              ) (map parse (third app-sexp)))
    )
    (define (parse-branch branch-expr)
    	(branch (parse (second branch-expr)) (parse (third branch-expr)) (parse (fourth branch-expr)))
    )
    (define (parse-mulbrach mulbrach-expr)
    	(multi−branch (map  (lambda (conds) (branch-cond (parse (first conds)) (parse (second conds))))
    						(second mulbrach-expr))
    				(parse (third mulbrach-expr)))
    )
    (cond
        [(symbol? sexp)
            (case sexp
                [(T) (bool #t)]
                [(F) (bool #f)]
                [else (id sexp)]
            )]
        [(number? sexp) (num sexp)]
        [(empty? sexp) (lempty)]
        [(list? sexp) (case (first sexp)
            [(+ - * / modulo exp not) (parse-op sexp)]
            [(or and) (op-bool (first sexp)
                (parse (second sexp)) (parse (third sexp)))]
            [(cons) (lcons (parse (second sexp)) (parse (third sexp)))]
            [(car) (lcar (parse(second sexp)))]
            [(cdr) (lcdr (parse (second sexp)))]
            [(with with*) (parse-with sexp)]
            [(fun) (parse-fun sexp)]
            [(app) (parse-app sexp)]
            [(if) (parse-branch sexp)]
            [(cond) (parse-mulbrach sexp)]
            )]
        [else (error sexp " no es una expresión FWAEL válida")]
    )
)
(define (length lst)
        (match lst
            ['() 0] ; caso base
            [(cons x xs ) (+ 1 ( length xs ))]
        )
    )

(define (desugar s-fwael-expr)
    ;; Función que obtiene la longitud de una lista
    ;; longitud : list - > number
    (define (length lst)
        (match lst
            ['() 0] ; caso base
            [(cons x xs ) (+ 1 ( length xs ))]
        )
    )
    (define (desugar-op op-expr)
        (let (
            [ope (op-f op-expr)]
            [arg (op-args op-expr)])
        	(if (= 1 (length arg))
        		(op ope (list (desugar (first arg))))
        		(op ope 
	                (list (desugar (first arg))
	                    (if (= 1 (length (rest arg)))
	                    	(first (rest arg))
	                        (desugar-op (op ope (rest arg)))
	                    )
	                )
	            )
        	)
        )
    )
    (define (desugar-with wexpr)
        ; Los WITH deben ser transformados en APP
        ; NOTA - APP también posee azúcar sintáctica
        (let (
            [bindings (with-bindings wexpr)])
            (app
                (fun (map (lambda (bdg) (binding-id bdg)) bindings)
                    (with-body wexpr))
                (map (lambda (bdg) (binding-value bdg)) bindings)
            )
        )
    )
    (define (desugar-with* wexpr)
        ; Los WITH* deben trasnformarse en WITH sin asterisco
        (let (
            [bindings (with-bindings wexpr)]
            [body (with*-body wexpr)])
            (with (list (first bindings))
                (if (empty? (rest bindings))
                    body
                    (desugar-with* (with* (rest bindings) body))
                )
            )
        )
    )
    (define (desugar-fun fun-expr)
        (let (
            [param (fun-params fun-expr)]
            [body (fun-body fun-expr)])
            (fun (list (first param))
                (if (empty? (rest param))
                    body
                    (desugar-fun (fun (rest param) body))
                )
            )
        )
    )
    (define (desugar-app app-expr)
        (let (
           [realp (app-args app-expr)]
           [formalp (fun-params (app-fun app-expr))]
           [body (fun-body (app-fun app-expr))])
                (app (fun (list (first formalp)) 
                    (if (empty? (rest formalp))
                        body
                        (desugar-app (app (fun (rest formalp) body) (rest realp)))
                    )
                )
                (map (lambda (param) (desugar param)) (list (first realp)))
            )
        )
    )
    (define (desugar-mulbranch mulbrach-expr)
    	(let (
    		[condis (multi−branch-conds mulbrach-expr)]
    		[el (multi−branch-else mulbrach-expr)])
    		(branch (desugar (branch-cond-test (first condis)))
    				(desugar (branch-cond-then (first condis)))
    				(if (empty? (rest condis))
    					(desugar el)
    					(desugar-mulbranch (multi−branch (rest condis) el))
    				)
    		)
    	)
    )
    (cond
        ; constructores FWAEL que PUEDEN tener azúcar en sub-expresiones
        ; constructores FWAEL que TIENEN azúcar sintáctica
        [(with? s-fwael-expr) (desugar (desugar-with s-fwael-expr))]
        [(with*? s-fwael-expr) (desugar (desugar-with* s-fwael-expr))]
        [(fun? s-fwael-expr) (desugar-fun s-fwael-expr)]
        [(app? s-fwael-expr) (desugar-app s-fwael-expr)]
        [(op? s-fwael-expr) (desugar-op s-fwael-expr)]
        [(multi−branch? s-fwael-expr) (desugar-mulbranch s-fwael-expr)]
        ; constructores FWAEL de expresiones atómicas
        [else s-fwael-expr]
    )
)

(define (subst fwael-expr sub-id env)
    (define (length lst)
        (match lst
            ['() 0] ; caso base
            [(cons x xs ) (+ 1 ( length xs ))]
        )
    )
    (define (find-inenv env)
        (if (mtSub? env)
            (error "ERROR" sub-id " es una variable libre")
            (if (eq? (aSub-name env) sub-id)
                (aSub-value env)
                (find-inenv (aSub-bSub env))
            )
        )
    )
    (define (subs-app app-expr)
        (let (
            [function (app-fun app-expr)]
            [args (app-args app-expr)])
            (app (fun (fun-params function) (if (> 0 (length args))
                                                (subst (fun-body function) (first args) env)
                                                (subst (fun-body function) (first args) env))) args)
        )
    )
    (cond
        ; La expresión un ID, se devuelve su valor asociado
        [(id? fwael-expr) (if (eq? sub-id (id-i fwael-expr))
            (find-inenv env)
            fwael-expr)]
        [(op? fwael-expr) (op (op-f fwael-expr) (map (lambda (arg) (subst arg sub-id env)) (op-args fwael-expr)))]
        [(op-bool? fwael-expr) (op-bool (op-bool-f fwael-expr) (subst (op-bool-larg fwael-expr) sub-id env) (subst (op-bool-rarg fwael-expr) sub-id env))]
        [(with? fwael-expr) (with (with-bindings fwael-expr) (subst (with-body fwael-expr) sub-id env))]
        [(with*? fwael-expr) (with* (with*-bindings fwael-expr) (subst (with*-body fwael-expr) sub-id env))]
        [(lcons? fwael-expr) (lcons (subst (lcons-l fwael-expr) sub-id env) (subst (lcons-r fwael-expr) sub-id env))]
        [(lcar? fwael-expr) (lcar (subst(lcar-lst fwael-expr) sub-id env))]
        [(lcdr? fwael-expr) (lcdr (subst(lcdr-lst fwael-expr) sub-id env))]
        ; La expresión PUEDE contener identificadores, los exploramos recursivamente
        ; La expresión NO puede contener identificadores, se devuelve como tal
        [(fun? fwael-expr) (subst (fun-body fwael-expr) sub-id env)]
        [(app? fwael-expr) (let (
            [function (app-fun fwael-expr)])
            (app (fun (fun-params function)
                      (subst (fun-body function) sub-id env))
                 (subst (app-args fwael-expr) sub-id env))
            )]
        [(branch? fwael-expr) (branch (subst (branch-test fwael-expr) sub-id env) (subst (branch-then fwael-expr) sub-id env) (subst (branch-else fwael-expr) sub-id env))]
        [(multi−branch? fwael-expr) (multi−branch (map (lambda (bcond)
        														(branch-cond
        															(subst (branch-cond-test bcond) sub-id env)
        															(subst (branch-cond-then bcond) sub-id env))) (multi−branch-conds fwael-expr))
        										  (subst (multi−branch-else fwael-expr) sub-id env))]
        [else fwael-expr]
    )
)

(define (interp fwael-expr env)
	(define (length lst)
        (match lst
            ['() 0] ; caso base
            [(cons x xs ) (+ 1 ( length xs ))]
        )
    )
    (define (find-inenv env)
        (if (mtSub? env)
            (error "ERROR" (id-i fwael-expr) "es una variable libre")
            (if (eq? (aSub-name env) (id-i fwael-expr))
                (aSub-value env)
                (find-inenv (aSub-bSub env))
            )
        )
    )
    (define (interp-op op-expr)
        (define (fiop-inenv exp env)
            (if (mtSub? env)
                (error "ERROR" (id-i op-expr) "es una variable libre")
                (if (eq? (aSub-name env) (id-i exp))
                    (aSub-value env)
                    (fiop-inenv exp (aSub-bSub env))
                )
            )
        )
        (let* (
            [desOp (desugar op-expr)]
            [ope (op-f desOp)]
            [arg (op-args desOp)])
            (if (= 1 (length arg))
            	(bool (ope 
	                (cond 
	                    [(bool? (first arg)) (bool-b (first arg))]
	                    [(id? (first arg)) (bool-b (fiop-inenv (first arg) env))]
	                    [(op? (first arg)) (interp-op (first arg))]
                        [else (interp (first arg) env)]
	                )
	            ))
	            (ope 
	                (cond 
	                    [(num? (first arg)) (num-n (first arg))]
	                    [(id? (first arg)) (num-n (fiop-inenv (first arg) env))]
	                    [(op? (first arg)) (interp-op (first arg))]
                        [else (interp (first arg) env)]
	                )
	                (cond
	                    [(num? (second arg)) (num-n (second arg))]
	                    [(id? (second arg)) (num-n (fiop-inenv (second arg) env))]
	                    [(op? (second arg)) (interp-op (second arg))]
                        [else (interp (second arg) env)]
	                )
	            )
            )
        )
    )
    (define (interp-bool bool-expr)
        (define (fibool-inenv exp env)
            (if (mtSub? env)
                (error "ERROR" (id-i bool-expr) "es una variable libre")
                (if (eq? (aSub-name env) (id-i exp))
                    (aSub-value env)
                    (fibool-inenv exp (aSub-bSub env))
                )
            )
        )
        (let (
            [ope (op-bool-f bool-expr)]
            [left (op-bool-larg bool-expr)]
            [right (op-bool-rarg bool-expr)])
            (bool (
                (cond
                 [(eq? 'and ope) (and
                                    (cond 
                                        [(bool? left) (bool-b left)]
                                        [(num? left) (num-n left)]
                                        [(id? left) (bool-b (fibool-inenv left env))]
                                        [else (interp left env)]
                                    )
                                    (cond
                                        [(bool? right) (bool-b right)]
                                        [(num? right) (num-n right)]
                                        [(id? right) (bool-b (fibool-inenv right env))]
                                        [else (interp right env)]
                                    ))]
                 [(eq? 'or ope) (or
                                    (cond 
                                        [(bool? left) (bool-b left)]
                                        [(num? left) (num-n left)]
                                        [(id? left) (bool-b (fibool-inenv left env))]
                                        [else (interp left env)]
                                    )
                                    (cond
                                        [(bool? right) (bool-b right)]
                                        [(num? right) (num-n right)]
                                        [(id? right) (bool-b (fibool-inenv right env))]
                                        [else (interp right env)]
                                    ))]
                )
            ))
        )
    )
	(cond
		[(id? fwael-expr) (interp(find-inenv env) env)]
		[(number? fwael-expr) (interp (num fwael-expr) env)]
        [(num? fwael-expr) (numV (num-n fwael-expr))]
        [(bool? fwael-expr) (boolV (bool-b fwael-expr))]
        [(op? fwael-expr) (interp (interp-op fwael-expr) env)]
        [(op-bool? fwael-expr) (interp (interp-bool fwael-expr) env)]
        [(with? fwael-expr) (error "ERROR" (fwael-expr) " no esta soportada")]
        [(with*? fwael-expr) (error "ERROR" (fwael-expr) " no esta soportada")]
        [(fun? fwael-expr)
            (closureV (fun-params fwael-expr) (fun-body fwael-expr) env)]
        [(lempty? fwael-expr) (listV empty)]
        [(lcons? fwael-expr) (let (
            [l-expr (interp (lcons-l fwael-expr) env)]
            [r-expr (interp (lcons-r fwael-expr) env)])
            (listV (list l-expr r-expr)))]
        [(lcar? fwael-expr) 
            (if (lcons? (lcar-lst fwael-expr))
                (interp (lcons-l (lcar-lst fwael-expr)) env)
                (interp (lcar-lst fwael-expr) env))]
        [(lcdr? fwael-expr)
            (if (lcons? (lcdr-lst fwael-expr))
                (interp (lcons-r (lcdr-lst fwael-expr)) env)
                (interp (lcdr-lst fwael-expr) env))]
		[(app? fwael-expr) (let* (
			[apped-fun (app-fun fwael-expr)]
			[args (app-args fwael-expr)]
			[fun-arg (first (fun-params apped-fun))]
			[env (aSub fun-arg (first args) env)])
            (interp (subst apped-fun fun-arg env) env)
		)]
        [(multi−branch? fwael-expr) (error "ERROR" (fwael-expr) " no esta soportada")]
		[(branch? fwael-expr)
            (if (boolV-b (interp (branch-test fwael-expr) env))
                (interp (branch-then fwael-expr) env)
                (interp (branch-else fwael-expr) env))]
		[else ("error de sintaxis")]
	)
)
