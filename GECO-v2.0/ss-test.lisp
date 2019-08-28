;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GECO-USER -*-

(in-package :GECO-USER)
#|
Genetic Evolution through Combination of Objects (GECO)

Copyright (C) 1992,1993  George P. W. Williams, Jr.

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Library General Public License for more details.

    You should have received a copy of the GNU Library General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
|#

;;;; This probably isn't the best way to solve this problem with a GA.
;;;;Remember, it's an example of how to use GECO.

(defvar *SSE* nil "a simple sequence ecosystem")
(defvar *SSE-STATISTICS-FILE-COUNTER* 0)
(defvar *SSE-STATISTICS-FILE-NAME* "sse-stats")
(defvar *SSE-STATISTICS-STREAM*) ;should be a slot on ecosystem
(defvar *SSE-DEFAULT-POP-SIZE* 40)
(defvar *SSE-DEFAULT-EVAL-LIMIT* 3000)
(defvar *SSE-TOURNAMENT-SIZE* 7)
(defvar *SSE-PROB-MUTATE* 0.025)
(defvar *SSE-PROB-CROSS* 0.8)

(let* ((sequence-length 10)
       (printable-allele-values (make-array sequence-length))
       (class-symbol (intern (format nil "SEQUENCE-CHROMOSOME-~D"
                                     sequence-length))))
  (dotimes (i sequence-length)
    (setf (aref printable-allele-values i)
          (cond ((< i 10) (character (+ i (char-code #\0))))
                ((< i 36) (character (+ (- i 10) (char-code #\a))))
                (T (error "sequence-chromosome-~D printable-allele-values ~
                           initialization error"
                          sequence-length)))))
  (eval `(defclass ,class-symbol (sequence-chromosome)
           ()))
  (eval `(defmethod SIZE ((self ,class-symbol))
           ,sequence-length))
  (eval `(defmethod LOCUS-ARITY ((self ,class-symbol) locus-index)
           (declare (ignore locus-index))
           ,sequence-length))
  (eval `(defmethod PRINTABLE-ALLELE-VALUES ((self ,class-symbol) locus-index)
           (declare (ignore locus-index))
           ,printable-allele-values)))

;; This operator actually seems to works better with a small population than with a large one.
;; With large populations, the crossover rate must be small so that the population can converge some (since it's quite disruptive).
;; It works significantly better than the PMX crossover, which requires a population of ~100 to work well, and then still requires
;; significantly more evaluations than does R3.
(defmethod R3-CROSS-CHROMOSOMES ((parent1 sequence-chromosome-10)
                                 (parent2 sequence-chromosome-10)
                                 (child1 sequence-chromosome-10)
                                 (child2 sequence-chromosome-10)
                                 &KEY
                                 (allele-test #'eql)
                                 &AUX
                                 (n (size parent1))
                                 (mismatched-alleles (make-array n))
                                 (mismatched-loci (make-array n))
                                 (mismatched-count 0)
                                 (better-parent (if (>= (score (organism parent1))
                                                        (score (organism parent2)))
                                                    parent1
                                                  parent2)))
  "Example of a heuristic version of R3."
  (if (dbg-p :r3)
      (dbgo "~&R3: p1,p2= ~S  Score=~F~%~11T~S  Score=~F"
            parent1 (score (organism parent1))
            parent2 (score (organism parent2))))
  ;; Assume all chromosomes are the same size.
  (dotimes (i n)
    (if (funcall allele-test (locus parent1 i) (locus parent2 i))
            (progn ;; copy the common alleles
                              (setf (locus child1 i) (locus parent1 i))
          (setf (locus child2 i) (locus parent2 i)))
      (progn ;; else, save mismatched alleles for randomizing
        (setf (aref mismatched-alleles mismatched-count)
              (locus better-parent i))
        (setf (aref mismatched-loci mismatched-count) i)
        (incf mismatched-count))))
  ;; randomly reinsert the mismatched alleles
  (dotimes (i mismatched-count)
    (let ((locus# (aref mismatched-loci i))
          ;; randomly prefer the order from the better parent
          (mismatched-allele-index (if (> (geco-random-integer 2) 0)
                                       (min i (1- mismatched-count))
                                     (geco-random-integer mismatched-count))))
      (setf (locus child1 locus#)
            (aref mismatched-alleles mismatched-allele-index))
      (setf (locus child2 locus#)
            (aref mismatched-alleles mismatched-allele-index))
      (let ((m-1 (1- mismatched-count)))
        (do ((mi mismatched-allele-index (1+ mi)))
            ((= mi m-1))
          (setf (aref mismatched-alleles mi) (aref mismatched-alleles (1+ mi))))
        (setq mismatched-count m-1))))
  (if (dbg-p :r3)
      (dbgo "~%~4Tc1,c2= ~S --after~%~11T~S"
            child1 child2)))

(defclass SIMPLE-SEQUENCE-ORGANISM-10 (organism)
  ())

(defmethod CHROMOSOME-CLASSES ((self simple-sequence-organism-10))
  '(sequence-chromosome-10))

(defclass SIMPLE-SEQUENCE-POPULATION-STATISTICS-10 (population-statistics)
  ())

(defmethod PRINT-OBJECT ((self simple-sequence-population-statistics-10) stream &AUX
                         (sse (ecosystem (population self))))
  (print-unreadable-object (self stream :type t :identity t)
    (if (slot-boundp self 'max-score)
        (format stream "Max=~F, Avg=~,2F, #Gens=~D, #Evals=~D"
                (max-score self)
                (avg-score self)
                (generation-number sse)
                (evaluation-number sse))
      (princ "#unbound#" stream))))

(defclass SIMPLE-SEQUENCE-POPULATION-10 (generational-population  maximizing-score-mixin)
  ())

(defmethod ORGANISM-CLASS ((self simple-sequence-population-10))
  'simple-sequence-organism-10)

(defmethod POPULATION-STATISTICS-CLASS ((self simple-sequence-population-10))
  'simple-sequence-population-statistics-10)

(defmethod CONVERGENCE-FRACTION ((self simple-sequence-population-10))
  0.9)

(defmethod CONVERGENCE-THRESHOLD-MARGIN ((self simple-sequence-population-10))
  0.95)

(defclass SIMPLE-SEQUENCE-PLAN (genetic-plan)
  ())

(defmethod PROB-MUTATE ((self simple-sequence-plan))
  *sse-prob-mutate*)

(defmethod PROB-CROSS ((self simple-sequence-plan))
  *sse-prob-cross*)

(defmethod EVALUATE ((self simple-sequence-organism-10) (plan simple-sequence-plan) &AUX
                     (chromosome (first (genotype self)))
                     (chromosome-size (size chromosome)))
  (declare (ignore plan))
  (setf (score self)
        (do* ((locus# 0 (1+ locus#))
              (result 0))
             ((>= locus# chromosome-size)
              result)
          (incf result (- 10 (abs (- locus# (locus chromosome locus#))))))))

(defmethod REGENERATE ((plan simple-sequence-plan)
                       (old-pop simple-sequence-population-10) &AUX
                       (new-pop (make-population (ecosystem old-pop)
                                                 (class-of old-pop)
                                                 :size (size old-pop))))
  ;; selectively reproduce, crossover, and mutate
  (operate-on-population plan old-pop new-pop)
  ;; record old-pop's statistics
  (print (statistics old-pop) *sse-statistics-stream*)
  new-pop)

(defmethod OPERATE-ON-POPULATION ((plan simple-sequence-plan) old-pop new-pop &AUX
                                  (new-orgs (organisms new-pop))
                                  (p-cross (prob-cross plan))
                                  (p-mutate (+ p-cross (prob-mutate plan)))
                                  (tournament-size *sse-tournament-size*)
                                  (pop-size (size old-pop))
                                  (orphan (make-instance (organism-class old-pop))))
  ;; Note that orphan is not a member of any population.  Since it's always a throw-away, it could be created
  ;; just once rather than for each generation, and retained in the plan.
  (do ((org1 (tournament-select-organism old-pop tournament-size)
             (tournament-select-organism old-pop tournament-size))
       org2
       (random# (geco-random-float 1.0) (geco-random-float 1.0))
       (i 0 (1+ i)))
      ((>= i pop-size))
    (cond
     ((> p-cross random#)
      (if (< (hamming-distance (first (genotype org1))
                               (first (genotype (setf org2 (tournament-select-organism
                                                            old-pop tournament-size))))))
          (r3-cross-organisms
           org1 org2
           (setf (aref new-orgs i) (make-organism new-pop))
           orphan ;a throw-away, can be GC'd because it's not in any population
           :allele-test #'=)
        ;; hamming distances < 2 will produce eidetic offspring anyway, by bypass crossover
        (setf (aref new-orgs i)
              (copy-organism-with-score org1 :new-population new-pop))))
     ((> p-mutate random#)
      (swap-alleles (setf (aref new-orgs i)
                          (copy-organism org1 :new-population new-pop))))
     (T ;; copying the score bypasses the need for a redundant evaluation
      (setf (aref new-orgs i)
            (copy-organism-with-score org1 :new-population new-pop))))))

(defun RUN-NEW-SSE (plan &KEY
                         (stream t)
                         (pop-size *sse-default-pop-size*)
                         (evaluation-limit *sse-default-eval-limit*))
  (let ((sse (make-instance 'ecosystem
               :pop-class 'simple-sequence-population-10
               :pop-size pop-size
               :plan-class plan
               :evaluation-limit evaluation-limit)))
    (setq *sse* sse)
    (flet ((run-sse-with-output (stream)
             (setq *sse-statistics-stream* stream)
             (evolve sse)
             (format stream "~& -- Max=~F, Avg=~F, #Gens=~D, #Evals=~D"
                     (max-score (statistics (population sse)))
                     (avg-score (statistics (population sse)))
                     (generation-number sse)
                     (evaluation-number sse))))
      (if stream
          (run-sse-with-output stream)
        (with-open-file (stream (format nil "~A~D"
                                        *sse-statistics-file-name*
                                        (incf *sse-statistics-file-counter*))
                                :direction :output
                                :if-exists :supersede)
          (run-sse-with-output stream))))))

(defun TEST-PLAN (times plan &KEY
                        (stream t)
                        (pop-size *sse-default-pop-size*)
                        (evaluation-limit *sse-default-eval-limit*)
                        &AUX
                        (plan-proto (class-prototype (find-class plan))))
  (let (maxs avgs gens evals)
    (flet ((test (stream)
             (dotimes (i times)
               (run-new-sse plan
                            :stream stream
                            :pop-size pop-size
                            :evaluation-limit evaluation-limit)
               (push (max-score (statistics (population *sse*))) maxs)
               (push (avg-score (statistics (population *sse*))) avgs)
               (push (generation-number *sse*) gens)
               (push (evaluation-number *sse*) evals))))
      (flet ((test-and-final-output (stream)
               (format stream "~&~A PopSize=~D, pCross=~F, pMutate=~F:"
                       plan pop-size (prob-cross plan-proto) (prob-mutate plan-proto))
               (time (test stream))
               (format stream "~% ===> Avg Max=~F, Avg Avg=~F, Avg #Gens=~F, Avg #Evals=~D"
                       (/ (float (reduce #'+ maxs)) times)
                       (/ (float (reduce #'+ avgs)) times)
                       (/ (float (reduce #'+ gens)) times)
                       (/ (float (reduce #'+ evals)) times))))
        (if (eq stream ':file)
            (with-open-file (*sse-statistics-stream* 
                             (format nil "~A~D"
                                     *sse-statistics-file-name*
                                     (incf *sse-statistics-file-counter*))
                             :direction :output
                             :if-exists :supersede)
              (test-and-final-output *sse-statistics-stream*))
          (test-and-final-output stream))))))

#||
(eval-enqueue '(in-package :geco-user))
(evaluate (population *sse*) (plan *sse*))
(inspect *sse*)
(evolve *sse*)
(progn (new-sse)
       (time (evolve *sse*))
       (inspect *sse*))
(progn (test-plan 1 'simple-sequence-plan)
       (inspect *sse*))

(test-plan 10 'simple-sequence-plan)

-- A place to experiment with parameters -- (and these are better than the defaults in the defvars)
(setq *sse-prob-mutate* 0.008)
(setq *sse-prob-cross* 0.85)
(setq *sse-default-pop-size* 60)
(setq *sse-default-eval-limit* 3000)
(setq *sse-tournament-size* 3)
 ===> Avg Max=98.2, Avg Avg=98.08333333333333, Avg #Gens=20.1, Avg #Evals=1090.1
(setq *sse-prob-mutate* 0.2)
(setq *sse-prob-cross* 0.6)
(setq *sse-default-pop-size* 80)
||#
