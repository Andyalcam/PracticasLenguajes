#lang plai

;; Alvarado Camacho Andrea		318064343
;; Velazquez Rosas Abner Elias	318171373

(define-type Figura
	[triangulo (a number?) (b number?) (c number?)]
	[rectangulo (a number?) (b number?)]
	[rombo (l number?) (D number?) (d number?)]
	[paralelogramo (a number?) (b number?) (h number?)]
	[elipse (a number?) (b number?)]
)

(define (aux a b)
	(/ (+ (sqr a) (sqr b)) 2)
)

;; perimetro: Figura → number
(define (perimetro f)
	(cond
		[(triangulo? f) (+ (triangulo-a f) (triangulo-b f) (triangulo-c f))]
		[(rectangulo? f) (* (+ (rectangulo-a f) (rectangulo-b f)) 2)]
		[(rombo? f) (* (rombo-l f) 4)]
		[(paralelogramo? f) (* (+ (paralelogramo-a f) (paralelogramo-b f)) 2)]
		[(elipse? f) (* (* 2 pi) (sqrt (aux (elipse-a f) (elipse-b f))))]
	)
)

(define (semiP a b c)
	(/ (+ a b c) 2)
)

(define (area f)
	(cond
		[(triangulo? f)
			(sqrt (* (semiP (triangulo-a f) (triangulo-b f) (triangulo-c f)) (- (semiP (triangulo-a f) (triangulo-b f) (triangulo-c f)) (triangulo-a f)) (- (semiP (triangulo-a f) (triangulo-b f) (triangulo-c f)) (triangulo-b f)) (- (semiP (triangulo-a f) (triangulo-b f) (triangulo-c f)) (triangulo-c f)) ) )]
		[(rectangulo? f)
			(* (rectangulo-a f) (rectangulo-b f))]
		[(rombo? f)
			(/ (* (rombo-d f) (rombo-D f)) 2)]
		[(paralelogramo? f)
			(* (paralelogramo-b f) (paralelogramo-h f))]
		[(elipse? f)
			(* pi (elipse-a f) (elipse-b f))]
	)
)

(define-type Vagon
	[locomotora (p positive-integer?)]
	[pasajeros (cap positive-integer?)]
	[restaurante (mesas positive-integer?) (personal positive-integer?)]
	[dormitorio (camas positive-integer?)]
)

(define-type Tren
	[tren-v (vagon Vagon?)]
	[tren (loci locomotora?) (resto Tren?) (locd locomotora?)]
	[treni (loci locomotora?) (resto Tren?)]
	[trend (resto Tren?) (locd locomotora?)]
)

