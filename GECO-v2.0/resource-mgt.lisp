;;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: GECO -*-

(in-package :GECO)
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

;;; Basic resource types

(defun RESOURCE-DEINIT-TYPE-CHECKER (object type-name)
  (unless (subtypep (type-of object) type-name)
    (error "~%#### ~S is not a ~S resource!~%" object type-name)))

(defmacro DEF-GECO-ARRAY-RESOURCE (type-name &REST make-array-keys)
  `(progn
     (defresource (,type-name
                   (size)
                   :matcher (= size (length object))
                   :initializer (dotimes (i size) (setf (svref object i) 'nil))
                   :deinitializer (resource-deinit-type-checker object ',type-name))
       (make-array size ,@make-array-keys))
     (defun ,(intern (format nil "NEW-~A" type-name)) (size)
       (apply #'allocate-resource ',type-name size))
     (defun ,(intern (format nil "OLD-~A" type-name)) (object)
       (deallocate-resource ',type-name object))))

(def-geco-array-resource SIMPLE-VECTOR)
(def-geco-array-resource SIMPLE-BIT-VECTOR :element-type 'bit)

;;; GECO resource types

(defun RESOURCE-DEINIT-CLASS-CHECKER (object class-name)
  (unless (subtypep (class-of object) class-name)
    (error "~%#### ~S is not a ~S resource!~%" object class-name)))

(defun RESOURCE-CLASS-MATCHER (object class-name)
  (eq class-name (class-of object)))

(defmacro DEF-GECO-CLASS-RESOURCE (class-name &KEY make-instance-keys)
  `(progn
     (defresource (,class-name
                   (&REST initargs)
                   :matcher (resource-class-matcher object ',class-name)
                   :initializer (apply #'reinitialize-instance object initargs)
                   :deinitializer (resource-deinit-class-checker object ',class-name))
       (apply #'make-instance ',class-name ,(if make-instance-keys
                                              `(append ',make-instance-keys initargs)
                                              'initargs)))
     (defun ,(intern (format nil "NEW-~A" class-name)) (&REST initargs)
       (apply #'allocate-resource ',class-name initargs))
     (defun ,(intern (format nil "OLD-~A" class-name)) (object)
       (deallocate-resource ',class-name object))))

#| Example:                      (macroexpand ')
(def-geco-class-resource BINARY-CHROMOSOME-10)
(def-geco-class-resource SIMPLE-BINARY10-ORGANISM)
(def-geco-class-resource POPULATION-STATISTICS)
(def-geco-class-resource SIMPLE-BINARY-POPULATION)
(defmethod OLD-SIMPLE-BINARY-POPULATION :AFTER ((object simple-binary-population)
                                                &AUX (organisms (organisms object)))
  (when organisms
    (dotimes (i (size object))
      (old-organism (svref organisms i)))
    (old-simple-vector organisms)))
(def-geco-class-resource SIMPLE-PLAN-1)
(def-geco-class-resource ECOSYSTEM)
|#