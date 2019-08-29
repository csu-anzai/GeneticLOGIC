;;; -*- Mode: LISP; Syntax: Common-lisp; Package: (darwin lisp); Base: 10.-*-
;;;choosing partners (for mating or competion)
;;; this is the file that knows about the geometery of the population

(in-package 'darwin :use '(lisp *lisp))

(defun default-choose-mate-function ()
  (*choose-partner-cube  generation-count))

(defun default-choose-competition-function ()
  (*choose-partner-cube (1+ generation-count)))

;;;address computation
(defmacro offset!! (address!! phase!! xor!!)
  `(-!! (logxor!! (+!! ,address!! ,phase!!) ,xor!!) ,phase!!))

;;;random

(defun choose-random ()
  (*let ((lot!! (rank!! (random!! (!! 65536)) '<=!!))
	 odd!! even!!)
    (declare (type (unsigned-pvar 24.) lot!! odd!! even!!))
    (*if (evenp!! lot!!)
	 (*pset :no-collisions (self-address!!) even!! lot!!)
	 (*pset :no-collisions (self-address!!) odd!! (1-!! lot!!)))
    (*when (evenp!! (self-address!!))
      (*pset :no-collisions odd!! partner!! even!!)
      (*pset :no-collisions even!! partner!! odd!!))))

;;;cube

(defun choose-cube ()
  (*choose-partner-cube generation-count))

(defun *choose-partner-cube (dim)
  (*set partner!! (logxor!! (self-address!!)
			    (!! (ash 1 (mod dim *log-number-of-processors-limit* ))))))

;;;1-d
(defun choose-1d ()
  (if (evenp generation-count)
      (*set partner!! (load-byte!! (1+!! (self-address!!)) (!! 0) (!! *log-number-of-processors-limit*)))
      (*set partner!! (load-byte!! (1-!! (self-address!!)) (!! 0) (!! *log-number-of-processors-limit*)))))

;;;2-d
;the shape of the space

(defun choose-2d ()
  (*choose-partner-2d-grid generation-count))

(defun choose-2d+1 ()
  (*choose-partner-2d-grid (1+ generation-count)))


(defvar *bits-per-island* 6)

(defmacro north-address!! ()
  `(cube-from-grid-address!! (self-address-grid!! (!! 0))
			    (logxor!! (self-address-grid!! (!! 1))
				      (!! 1))))

(defmacro south-address!! ()
  `(cube-from-grid-address!! (self-address-grid!! (!! 0))
			    (dpb!! (offset!! (self-address-grid!! (!! 1)) (!! 1) (!! 1))
				   (byte!! (!! *bits-per-island*) (!! 0))
				   (self-address-grid!! (!! 1))
				   )))

(defmacro east-address!! ()
  `(cube-from-grid-address!! (logxor!! (self-address-grid!! (!! 0))
				      (!! 1))
			    (self-address-grid!! (!! 1))))

(defmacro west-address!! ()
  `(cube-from-grid-address!! (dpb!! (offset!! (self-address-grid!! (!! 0)) (!! 1) (!! 1))
				   (byte!! (!! *bits-per-island*) (!! 0))
				   (self-address-grid!! (!! 0))     
				   )
			    (self-address-grid!! (!! 1))))


(defun *choose-partner-2d-grid (count)
  (case (mod count 4)
    (0 (*set partner!! (north-address!!)))
    (1 (*set partner!! (west-address!!)))
    (2 (*set partner!! (south-address!!)))
    (3 (*set partner!! (east-address!!)))))
       
