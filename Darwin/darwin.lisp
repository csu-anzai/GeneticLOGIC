;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (darwin lisp); Base: 10.-*-
;;; Darwin: an evolutionary system

(in-package 'darwin :use '(lisp *lisp))
;to use (load-file-set 'darwin)
; run (TEST-DARWIN) to verify

;;; ****************** CHANGE LOG ******************
;;; 
;;; 12/06/86 12:53:05 danny:  Created. Works on Tracers and Lemmings
;;; 
;;; 12/13/86 15:52:21 danny:  adult mortality, selective crossover
;;; 
;;; 2/16/87 13:05:14 danny:  eliminate fitness function
;;; 
;;; 6/10/87 14:59:42 danny:  separate genome and phenome
;;; 
;;; *************** END OF CHANGE LOG ***************

(defun evolve (&optional (total-generations 100000))
  (loop for i below total-generations
	do (choose-competion)		;pick partner!!
	   (compete)
	   (choose-mate)		;pick partner!!
	   (reproduce)
	   (mutate)
	   (make-phenome)
	   (incf generation-count)
	   ))

(defun new-evolve (&optional (total-generations 100000))   ;taylor/jefferson 10/27/87
  (loop for i below total-generations
	do (choose-competion)		;pick partner!!
	   (compete)
	   (choose-mate)		;pick partner!!
	   (reproduce)
	   (lockstep-mutation)
           (make-phenome)
           (when (= 0 (mod generation-count frequency)) 
                 (loop for j below (length *phi-specs*)
                    do (setf (aref target j) (mod (+ (aref target j) (* speed (aref delta j))) 64)))
                 (print target))
	   (incf generation-count)
	   ))


(defun show-evolution (&optional (generations-per-graph-point 1)
		       (initialize-population t)
		       (max-generations 10000000.))
  (when initialize-population (init-population)
	(setq generation-count 0))
  (score)
  (loop until (or (read-char-no-hang)
		  (>= generation-count max-generations))
	do
    (show)
    (evolve generations-per-graph-point)
    )
  (score)
  (show)
  (best-score))

(defun new-show-evolution (&optional (generations-per-graph-point 1)     ;same as above, except moving target
		       (initialize-population t)                    ;modified by taylor and jefferson 10/27/87
		       (max-generations 10000000.))
  (when initialize-population (init-population)
	(setq generation-count 0))
  (score)
  (loop until (or (read-char-no-hang)
		  (>= generation-count max-generations))
	do
    (show)
    (new-evolve generations-per-graph-point)
    )
  (score)
  (show)
  (best-score))


;;;competiion
(defun default-compete-function () (compete-by-score))
(defun default-make-phenome-function () nil)
(defun default-score-function () (*set score!! (!! 0)))

(defun compete-by-score ()
  (score)
  (*if (>!! score!! (pref!! score!! partner!!))
       (loop for i below chromosomes-per-genome
	     do (*pset :no-collisions (chromosome!! i) (chromosome!! i) partner!!))))

(defun compete-by-randomized-score ()
  (score)
  (*let ((randomized-score!! (*!! (random!! (!! 256)) score!!)))
    (declare (type (signed-pvar 32) randomized-score!!))
    (*if (>!! randomized-score!! (pref!! randomized-score!! partner!!))
	 (loop for i below chromosomes-per-genome
	       do (*pset :no-collisions (chromosome!! i) (chromosome!! i) partner!!)))))

(defun record-evolution (&optional (generations-per-graph-point 1)
			 (initialize-population t)
			 (max-generations 10000.)
			 (collect-function 'average-score)
			 (print-interval 100))
  (when initialize-population (init-population)
	(setq generation-count 0))
  (score)
  (loop until (or (>= (average-score) max-score)
		  (>= generation-count max-generations)
		  (eql #\. (read-char-no-hang)))
	do (evolve generations-per-graph-point)
	   (if (zerop (mod generation-count print-interval))
	       (print-fitness))
        collect (funcall collect-function)
    ))


