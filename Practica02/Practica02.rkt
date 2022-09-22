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

;; area: Figura → number
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
	[tren-t (vagon Vagon?) (resto Tren?)]
	[tren-f (resto Tren?) (vagon Vagon?)]
)

;; num-pasajeros: Tren → positive-integer
(define (num-pasajeros tren)
	(cond
		[(tren-v? tren) 
			(let (
				[vagon (tren-v-vagon tren)])
				(if (pasajeros? vagon)
					(pasajeros-cap vagon)
					0
				)
			)]
		[(tren? tren)
			(let (
				[restot (tren-resto)])
				(num-pasajeros restot)
			)]
		[(tren-t? tren)
			(let (
				[vagon (tren-t-vagon tren)]
				[restot (tren-t-resto tren)])
				(+ (num-pasajeros restot)
					(if (pasajeros? vagon)
						(pasajeros-cap vagon)
						0))
			)]
		[(tren-f? tren)
			(let (
				[restot (tren-f-resto tren)]
				[vagon (tren-f-vagon tren)])
				(+ (num-pasajeros restot)
						(if (pasajeros? vagon)
							(pasajeros-cap vagon)
							0))
			)]
	)
)

(define (ac-potLoc tren)
	(cond
		[(tren-v? tren) 
			(let (
				[vagon (tren-v-vagon tren)])
				(if (locomotora? vagon)
					(locomotora-p vagon)
					0
				)
			)]
		[(tren? tren)
			(let (
				[loci (tren-loci tren)]
				[locd (tren-locd tren)]
				[restot (tren-resto)])
				(+ (locomotora-p loci)
					(locomotora-p locd)
					(ac-potLoc restot))
			)]
		[(tren-t? tren)
			(let (
				[vagon (tren-t-vagon tren)]
				[restot (tren-t-resto tren)])
				(+ (ac-potLoc restot)
					(if (locomotora? vagon)
						(locomotora-p vagon)
						0))
			)]
		[(tren-f? tren)
			(let (
				[restot (tren-f-resto tren)]
				[vagon (tren-f-vagon tren)])
				(+ (ac-potLoc restot)
						(if (locomotora? vagon)
							(locomotora-p vagon)
							0))
			)]
	)
)

(define (cuenta-vags-noLoc tren)
	(cond
		[(tren-v? tren) 
			(let (
				[vagon (tren-v-vagon tren)])
				(if (locomotora? vagon)
					0
					1)
			)]
		[(tren? tren) 
			(let (
				[loci (tren-loci tren)]
				[locd (tren-locd tren)]
				[restot (tren-resto tren)])
				(cuenta-vags-noLoc restot)
			)]
		[(tren-t? tren)
			(let (
				[vagon (tren-t-vagon tren)]
				[restot (tren-t-resto tren)])
				(+ (cuenta-vags-noLoc restot)
					(if (locomotora? vagon)
						0
						1))
			)]
		[(tren-f? tren)
			(let (
				[vagon (tren-f-vagon tren)]
				[restot (tren-f-resto tren)])
				(+ (cuenta-vags-noLoc restot)
					(if (locomotora? vagon)
						0
						1))
			)]
	)
)

;; arrastre-usado: Tren → number
(define (arrastre-usado tren)
	(/ (* (cuenta-vags-noLoc tren) 100)
		(ac-potLoc tren))
)

;; Aux para contar camas
(define (cont-camas tren)
	(cond
		[(tren-v? tren) 
			(let (
				[vagon (tren-v-vagon tren)])
				(if (dormitorio? vagon)
					(dormitorio-camas vagon)
					0)
			)]
		[(tren? tren) 
			(let (
				[restot (tren-resto tren)])
				(cont-camas restot)
			)]
		[(tren-t? tren)
			(let (
				[vagon (tren-t-vagon tren)]
				[restot (tren-t-resto tren)])
				(+ (cont-camas restot)
					(if (dormitorio? vagon)
						(dormitorio-camas vagon)
						0))
			)]
		[(tren-f? tren)
			(let (
				[vagon (tren-f-vagon tren)]
				[restot (tren-f-resto tren)])
				(+ (cont-camas restot)
					(if (dormitorio? vagon)
						(dormitorio-camas vagon)
						0))
			)]
	)
)

;; sin-cama: Tren → nonnegative-integer
(define (sin-cama tren)
	(let (
		[numPas (num-pasajeros tren)]
		[numCam (cont-camas tren)])
		(if (> numPas numCam)
			(- numPas numCam)
			0)
	)
)








