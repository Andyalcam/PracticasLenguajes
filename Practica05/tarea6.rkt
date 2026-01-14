#lang plai
(let* (
	[Z (lambda(g) ((lambda (x) (g (lambda (v) ((x x) v))))
				   (lambda (x) (g (lambda (v) ((x x) v))))))]
	[fib (Z (lambda (fib) (lambda (n) (if (or (zero? n) (= n 1))
										1 
										(+ (fib (- n 1)) (fib (- n 2)))))))])
	(fib 3)
)