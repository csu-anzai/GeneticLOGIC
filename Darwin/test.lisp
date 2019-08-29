;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (darwin lisp); Base: 10.-*-
;;; diagnostic to see if the system works

(in-package 'darwin :use '(lisp *lisp))


(defun test-darwin ()
  (use-species 'lemmings)
  (init-population)
  (let ((point-mutations-per-generation 1)
	(transpositions-per-generation 1))
    (loop for i below 8
	  do (print-fitness)
	     (evolve 10))
    (format t "~%~a"
	    (if (> (average-fitness) .98)
		"Darwin was right."
		"Something is wrong."))
    (average-fitness)))


;;; Tracers
;;; These creatures are tagged to trace the ancestry
;;; they compete randomly

(defspecies tracers
  (chromosomes-per-genome 4)
  (bits-per-codon 6)
  (codons-per-chromosome 16)
  (score-function 'tracer-score)
  (max-score 8)
  (init-function 'tracer-init)
  (show-function  'show-sample-of-population)
  (codon-name-function 'tracer-name))

(defun tracer-score ()
  (*set score!! (random!! (!! 8))))

(defun tracer-init ()
  "Each chromosome start will all codon of one character"
  (loop for chromo below chromosomes-per-genome
	do (loop for codon below codons-per-chromosome
		 do (*set (codon!! codon chromo) (+!! (mod!! (self-address!!) (!! 16))
						    (!! (* 32 (logand 1 chromo))))))))

(defun tracer-name (value)
  (cond ((< value 32.) (elt "ABCDEFGHIJKLMNOPQRSTUVWXYZ012345" value))
	((< value 64.) (elt "abcdefghijklmnopqrstuvwxyz<>[]{}" (- value 32.)))
	(t #\*)))
  
;;; lemmings
;;; They try to evolve their first chromosome to all 0 and their second to all 1 

(defspecies lemmings
  (chromosomes-per-genome 4)
  (bits-per-codon 2)
  (codons-per-chromosome 10)
  (score-function 'lemming-score)
  (show-function  'show-sample-of-population)
  (max-score 60))


(defun lemming-score ()
  (*set score!! (!! 0))
  (loop for i below codons-per-chromosome
	do (*set score!! (-!! score!! (codon!! i 0))))
  (loop for i below codons-per-chromosome
	do (*set score!! (-!! score!! (logxor!! (!! 1) (codon!! i 2)))))
  (*set score!! (+!! score!! (!! max-score)))	;to make positve
  )

