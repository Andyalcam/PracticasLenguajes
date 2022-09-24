#lang plai

;; Alvarado Camacho Andrea		318064343
;; Velazquez Rosas Abner Elias	318171373

;;tipo de datos abstracto Figura que sea utilizado para trabajar con figuras geométricas
(define-type Figura
	[triangulo (a number?) (b number?) (c number?)]
	[rectangulo (a number?) (b number?)]
	[rombo (l number?) (D number?) (d number?)]
	[paralelogramo (a number?) (b number?) (h number?)]
	[elipse (a number?) (b number?)]
)

;; aux: number -> number -> number
;; Funcion auxiliar que realiza el calculo necesario dentro de la raiz cuadrada en la
;; formula de perimetro de elipse ' (a^2 + b^2) / 2 '.
;; * Precondiciones: dos numeros.
;; * Postcondiciones: el resultado de calcular ' (a^2 + b^2) / 2 '.
(define (aux a b)
	(/ (+ (sqr a) (sqr b)) 2)
)

;; perimetro: Figura → number
;; Calcula el perímetro de una Figura dada.
;; ∗ Precondiciones: una instancia de Figura.
;; ∗ Postcondiciones: el perímetro de la Figura dada.
(define (perimetro f)
	(cond
		[(triangulo? f) (+ (triangulo-a f) (triangulo-b f) (triangulo-c f))]
		[(rectangulo? f) (* (+ (rectangulo-a f) (rectangulo-b f)) 2)]
		[(rombo? f) (* (rombo-l f) 4)]
		[(paralelogramo? f) (* (+ (paralelogramo-a f) (paralelogramo-b f)) 2)]
		[(elipse? f) (* (* 2 pi) (sqrt (aux (elipse-a f) (elipse-b f))))]
	)
)

;; semiP: number -> number -> number -> number
;; Funcion auxiliar que calcula el semiPerimetro de un triangulo necesario para calcular
;; el perimetro total ' (a + b + c) / 2 '.
;; ∗ Precondiciones: tres numeros que repesentan la magnitud de cada lado de un triangulo.
;; ∗ Postcondiciones: el resultado del semiperimetro de un triangulo.
(define (semiP a b c)
	(/ (+ a b c) 2)
)

;; area: Figura → number
;; Calcula el área de una Figura dada.
;; * Precondiciones: una instancia de Figura.
;; ∗ Postcondiciones: el área de la Figura dada.
(define (area f)
	(cond
		[(triangulo? f)
			(sqrt (* (semiP (triangulo-a f) (triangulo-b f) (triangulo-c f))
					(- (semiP (triangulo-a f) (triangulo-b f) (triangulo-c f))
						(triangulo-a f))
					(- (semiP (triangulo-a f) (triangulo-b f) (triangulo-c f)) 
						(triangulo-b f))
					(- (semiP (triangulo-a f) (triangulo-b f) (triangulo-c f))
						(triangulo-c f))
					))]
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

;; Clase de tren de pasajeros conformado por los vagones de tipo locomotora, pasajeros
;; restaurante y dormitorio.
(define-type Vagon
	[locomotora (p positive-integer?)]
	[pasajeros (cap positive-integer?)]
	[restaurante (mesas positive-integer?) (personal positive-integer?)]
	[dormitorio (camas positive-integer?)]
)

;; El tipo de datos Tren, que modela trenes de tipo Vagon
(define-type Tren
	[tren-v (vagon Vagon?)]
	[tren (loci locomotora?) (resto Tren?) (locd locomotora?)]
	[tren-t (vagon Vagon?) (resto Tren?)]
	[tren-f (resto Tren?) (vagon Vagon?)]
)

;; num-pasajeros: Tren → positive-integer
;; Calcula el número de pasajeros máximo que pueden abordar el tren.
;; ∗ Precondiciones: un tren que satisface las condiciones establecidas en el ejercicio anterior.
;; ∗ Postcondiciones: la suma de las capacidades máximas de sus vagones de pasajeros.
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
				[restot (tren-resto tren)])
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


;; ac-potLoc: Tren → positive-integer
;; Calcula la potencia de todas las locomotoras en un tren.
;; ∗ Precondiciones: un tren que satisface las condiciones establecidas en el ejercicio anterior.
;; ∗ Postcondiciones: la suma de las potencias de las locomotoras del tren.
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
				[restot (tren-resto tren)])
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


;; cuenta-vags-noLoc: Tren → positive-integer
;; Calcula el número de vagones que no son locomotoras en un tren.
;; ∗ Precondiciones: un tren que satisface las condiciones establecidas en el ejercicio anterior.
;; ∗ Postcondiciones: la suma de los vagones que no son locomotoras de un tren.
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
;; calcula el porcentaje de la potencia de arrastre utilizada del tren.
;; ∗ Precondiciones: un tren que satisface las condiciones establecidas en el ejercicio anterior.
;; ∗ Postcondiciones: la proporción del total de la capacidad de arrastre de todas las locomotoras
;; respecto al número de vagones no-locomotoras como porcentaje numérico.
(define (arrastre-usado tren)
	(/ (* (cuenta-vags-noLoc tren) 100)
		(ac-potLoc tren))
)

;; cont-camas: Tren -> positive-integer
;; Aux para contar camas de un tren.
;; * Precondiciones: un tren que satisface las condiciones establecidas en el ejercicio anterior.
;; * Postcondiciones: la suma total de todas las camas por vagon que tiene un tren.
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
;; Calcula el número de pasajeros que quedarían sin cama durante.
;; De acuerdo al total de pasajeros y camas en el tren.
;; ∗ Precondiciones: un tren que satisface las condiciones establecidas en el ejercicio anterior.
;; ∗ Postcondiciones: el número máximo de pasajeros que excede la capacidad del total de los vagones dormitorio.
(define (sin-cama tren)
	(let (
		[numPas (num-pasajeros tren)]
		[numCam (cont-camas tren)])
		(if (> numPas numCam)
			(- numPas numCam)
			0)
	)
)

;; max-comensales: Tren → nonnegative-integer
;; Determina el número máximo de pasajeros que pueden ser
;; atendidos al mismo tiempo en los vagones restaurante del tren.
;; ∗ Precondiciones: un tren que satisface las condiciones establecidas en el ejercicio anterior.
;; ∗ Postcondiciones: el máximo de pasajeros que pueden ser atendidos en un vagón restaurante; que
;; está limitado por su número de mesas o de personal de servicio.
(define (max-comensales tren)
	(cond
		[(tren-v? tren) 
			(let* (
				[vagon (tren-v-vagon tren)]
				[numPersonal 
					(if (restaurante? vagon)
					(restaurante-personal vagon)
					0)])
				(* numPersonal 8)
				
			)]
		[(tren? tren) 
			(let (
				[restot (tren-resto tren)])
				(max-comensales restot)
			)]
		[(tren-t? tren)
			(let* (
				[vagon (tren-t-vagon tren)]
				[restot (tren-t-resto tren)]
				[numPersonal 
					(if (restaurante? vagon)
					(restaurante-personal vagon)
					0)])
				(+ (max-comensales restot)
					(* numPersonal 8))
			)]
		[(tren-f? tren)
			(let* (
				[vagon (tren-f-vagon tren)]
				[restot (tren-f-resto tren)]
				[numPersonal 
					(if (restaurante? vagon)
					(restaurante-personal vagon)
					0)])
				(+ (max-comensales restot)
					(* numPersonal 8))
			)]
	)
)

;; Pruebas unitarias
(define (prueba1-area)
	(test (area (triangulo 3 4 5))
		6)
)

(define (prueba2-area)
	(test (area (rectangulo 3 5))
		15)
)

(define (prueba3-area)
	(test (area (rombo 12 9 6))
		27)
)

(define (prueba4-area)
	(test (area (paralelogramo 3 4 5))
		20)
)

(define (prueba5-area)
	(test (area (elipse 8 5))
		125.66370614359172)
)

(define (prueba1-perimetro)
	(test (perimetro (triangulo 3 4 5))
		12)
)

(define (prueba2-perimetro)
	(test (perimetro (rectangulo 3 5))
		16)
)

(define (prueba3-perimetro)
	(test (perimetro (rombo 8 6 3))
		32)
)

(define (prueba4-perimetro)
	(test (perimetro (paralelogramo 3 4 5))
		14)
)

(define (prueba5-perimetro)
	(test (perimetro (elipse 8 4))
		39.738353063184405)
)

(define (prueba-num-pasajeros)
	(test (num-pasajeros (tren (locomotora 1)(tren-t(pasajeros 18)(tren-v(dormitorio 30)))(locomotora 15)))
		18)
)

(define (prueba-arrastre-usado)
	(test (arrastre-usado (tren-t (locomotora 2) (tren-f (tren-f (tren-v (pasajeros 20)) (restaurante 2 4)) (dormitorio 1))))
		150)
)

(define (prueba-sin-cama)
	(test (sin-cama (tren-f (tren (locomotora 3)(tren-f (tren-f (tren-v(dormitorio 25)) (pasajeros 34))(restaurante 3 1))(locomotora 3))(pasajeros 15)))
		24)
)

(define (prueba-max-comensales)
	(test (max-comensales (tren-t(pasajeros 13)(tren-t(restaurante 9 1)(tren-t(locomotora 6)(tren-v(restaurante 1 10))))))
		88)
)