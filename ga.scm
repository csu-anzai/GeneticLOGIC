#lang racket

(define (random-bit) (random 2))
 
(define (make-random-genome num-bits)
  (map (lambda (x) (random-bit)) (iota num-bits 1)))
 
(define (make-probabilistic-bit-flipper num-bits)
  (define (bit-flipper bit)
    (let ((bit-array (append '(1) (make-list (- num-bits 1) 0))))
      (if (= 1 (list-ref bit-array (random num-bits)))
          (if (= bit 0) 1 0)
          bit)))
  bit-flipper)
 
(define (mutate genome)
  (map (make-probabilistic-bit-flipper (length genome)) genome))
 
(define (crossover g1 g2)
  (let ((crossover-point (random (length g1))))
    (cons (append (list-head g1 crossover-point)
                  (list-tail g2 crossover-point))
          (append (list-head g2 crossover-point)
                  (list-tail g1 crossover-point)))))
									
(define (make-random-population population-size make-member-func)
  (map (lambda (x) (make-member-func)) (iota population-size 1)))
 
(define (get-member-fitness population fitness-func)
  (map fitness-func population))
 
(define (get-average-fitness population fitness-func)
  (/ (reduce + 0 (get-member-fitness population fitness-func))
     (exact->inexact (length population))))


(define (random-shuffle lst)
  (sort lst (lambda (x y) (equal? 0 (random 2)))))
 
(define (make-random-bit-mask size num-ones)
  (if (< size num-ones)
      (error "Invalid size -- MAKE-RANDOM-BIT-MASK" size num-ones)
      (random-shuffle (append (make-list num-ones 1)
                              (make-list (- size num-ones) 0)))))
 
(define (select-for-tournament population tournament-size)
  (map car
     (filter (lambda (x) (= (cdr x) 1))
             (map cons
                population
                (make-random-bit-mask (length population)
                                      tournament-size)))))
 
 
(define (select-fittest population fitness-func num-to-select)
  (list-head
   (sort population
         (lambda (x y) (> (fitness-func x) (fitness-func y))))
   num-to-select))
	 
(define (next-generation population fitness-func)
  (define (produce-next-generation-member)
    (if (= 1 (random 2))
        (let ((tournament-members (select-for-tournament population 3)))
          ((if (= 1 (random 2)) car cdr)
           (apply crossover
                  (select-fittest tournament-members fitness-func 2))))
        (mutate (car (select-for-tournament population 1)))))
  (map (lambda (x) (produce-next-generation-member))
     (iota (length population) 1)))

(define (bits->int genome)
  (reduce + 0
   (map (lambda (x y) (* x (expt 2 y)))
      genome
      (reverse (map (lambda (x) (- x 1)) (iota (length genome) 1))))))
 
(define population
  (make-random-population 10 (lambda () (make-random-genome 10))))
 
(next-generation population bits->int)
		 
		 