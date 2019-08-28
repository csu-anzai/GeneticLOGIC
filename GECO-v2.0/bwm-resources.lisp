;;; -*- Mode: LISP; Syntax: Ansi-common-lisp; Package: CL-LIB; Base: 10 -*-

;;; ****************************************************************
;;; Resources ******************************************************
;;; ****************************************************************
;;;
;;; This is the resources package written February 1992 by 
;;;   Bradford W. Miller
;;;   miller@cs.rochester.edu
;;;   University of Rochester, Department of Computer Science
;;;   610 CS Building, Comp Sci Dept., U. Rochester, Rochester NY 14627-0226
;;;   716-275-1118
;;; I will be glad to respond to bug reports or feature requests.
;;;
;;; This version was obtained from the directory
;;; /afs/cs.cmu.edu/user/mkant/Public/Lisp-Utilities/initializations.lisp
;;; via anonymous ftp from a.gp.cs.cmu.edu.
;;;
;;; Bug reports, improvements, and feature requests should be sent
;;; to miller@cs.rochester.edu. Ports to other lisps are also welcome.
;;; (It would be appreciated if you would also cc mkant@cs.cmu.edu.)
;;;
;;; Copyright (C) 1992 by the University of Rochester.
;;; Right of use & redistribution is granted as per the terms of the 
;;; GNU GENERAL PUBLIC LICENCE version 2 which is incorporated here by
;;; reference. 
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
;;;

;;; ********************************
;;; Motivation *********************
;;; ********************************
;;;
;;; Resources are useful for avoid excessive consing and GCing by explicitly
;;; maintaining pools of objects for reuse, instead of creating them when
;;; needed and relying on the garbage collector to flush them when they are
;;; no longer needed. When an object is no longer needed, it is returned
;;; to the pool of objects of that type, and recycled when a new object
;;; of that type is needed. Using resources might wind up recycling objects
;;; faster than incremental GC, but it isn't clear whether there are
;;; any real time/space savings.
;;;
;;; Since I have been using the resource features of the explorer and
;;; symbolics, the following was inspired to allow a simple port to
;;; non-lispm lisps. It should be compatible with most uses of the lispm's
;;; versions, though it does not support (yet) the wide variety of
;;; options.
;;;
;;; note: efficiency can likely be improved; the basic idea is to take a
;;; name of a resource, and put some properties on it - the pool of all
;;; allocated resources and a freelist. So building a resource instance
;;; uses (at least) three cons cells - one on the pool, one on the
;;; freelist, and one on a description list for the default matcher. The
;;; default matcher isn't particularly efficient either since it uses
;;; position and nth; but most internal uses of resources have no args
;;; (all resources identical) so matcher isn't used anyway. Better would
;;; be to build a structure that keeps the description and other info in
;;; it, then we have to be able to find the structure on a free (symbolics
;;; passes an extra value to the user for just this purpose I beleive -
;;; I've not looked at their source code for this, of course :-)
;;;
;;; resources won't be that useful for tiny thingos, since the alloc/free
;;; cycle kicks out a cons cell per. For bigger things, the efficiency
;;; depends on use. Long lived objects are probably better off just using
;;; the usual gc. Particularly since on non-lispms we can't prevent
;;; scavenge attempts of resources anyway.
;;;
;;; another (internal) use of resources is to write code that may
;;; eventually depend on malloc - so the user has to get used to
;;; explicitly freeing things anyway. (Shocker back end is an example of
;;; this). Of course lisps that allow you to cons things in separate areas
;;; that aren't gc'd can improve efficiency by making sure resources are
;;; created there.  and if CL had some sort of FREE function to explicitly
;;; free a list, (I mean it has to be cheaper for an application to know
;;; what's garbage than to have a gc discover it, no?) then resources
;;; could also be more (generically) efficient.
;;;
;;; Note: on a symbolics we use the built-in stuff (translating args as
;;; appropriate) since I assume that's gonna be more efficient than this
;;; code.

(when (find-package "CL-LIB")
  (in-package "CL-LIB"))


(defun extract-parameters (args) 
  (let (result)
    (dolist (arg args (nreverse result))
      (if (consp arg)
	  (push (car arg) result)
	  ;; (unless (char= #\& (char (string arg) 0))
	  ;;   (push arg result))
	  (find arg lambda-list-keywords)))))

(defmacro defresource ((name args &rest hack
			     &key (initial-copies 0) 
			     initializer deinitializer
			     matcher &allow-other-keys)
		       &body constructor)
  "Name, an unevaluated symbol, will become the name of the new resource 
   (like the lispm, it's a property of Name).
   Args, a lambda list, are used to initialize (create) instances of the 
   resource, and come from allocate-resource (so it can be used to supply, 
   e.g. default arguments)

   constructor is a function to call to make a new object when the resource
   is empty, and uses the parameters Args as arguments (think of it as a
   defun). Note this is required.

   Options are:

	:initial-copies (used to set up the pool to begin with).

	:initializer (called on a newly allocated object, and the other 
        parameters). Note the constructor isn't called on objects that
        are already in the pool.

        :deinitializer (called on a newly freed object) Useful to allow gc
        of objects the resource refers to.

	:matcher Args are like initializer, but is expected to be a predicate
        that succeeds if the unallocated pool object is appropriate for the 
        call. The default one assumes only pool objects created with the same
        parameters are appropriate.
        This is useful if you are going to put different size objects in the
        pool, but don't want to have to create a new object when a (bigger)
        one already exists.

        The lispm supports other options too."

  #+symbolics
  `(scl:defresource ,name ,args :constructor ,@constructor ,@hack)
  #-symbolics
  (let ((parameters (extract-parameters args)))
    `(progn
       (setf (get ',name :resource-cons)
	     #'(lambda ,args ,@constructor))
       (setf (get ',name :resource-pool) nil)
       (setf (get ',name :resource-desc) nil)
       (setf (get ',name :resource-freel) nil)
       (setf (get ',name :resource-default)
	     #'(lambda ,args (list ,@parameters)))
       (setf (get ',name :resource-free)
	     #'(lambda (object) ,deinitializer))
       (setf (get ',name :resource-init)
	     #'(lambda (object ,@args) ,initializer))
       (setf (get ',name :resource-matcher)
	     #'(lambda (object ,@args)
		 ,(or matcher
		      (null args)
		      `(every #'eql (list ,@parameters)
			      (nth (position object
					     (get ',name :resource-pool))
				   (get ',name :resource-desc))))))
       ,@(when (plusp initial-copies)
	   `((dotimes (i ,initial-copies)
	       (declare (ignore i))
	       (push (funcall (get ',name :resource-cons)) 
		     (get ',name :resource-pool))
	       (push (funcall (get ',name :resource-default))
		     (get ',name :resource-desc)))
	     (clear-resource ',name))))))

(defun allocate-resource (name &rest args)
  "Get a copy of the NAMEd resource, given the args (for the initializer, 
   matcher and constructor). Name is evaluated."
  #+symbolics
  (apply #'scl:allocate-resource name args) 
  #-symbolics
  (progn
    (do ((lasttry nil try)
	 (try (get name :resource-freel) (cdr try)))
	((null try) nil)
      (when (if args
		(apply (get name :resource-matcher) (car try) args)
		(funcall (get name :resource-matcher) (car try)))
	(if args 
	    (apply (get name :resource-init) (car try) args)
	    (funcall (get name :resource-init) (car try)))
	(if lasttry
	    (rplacd lasttry (cdr try))
	    (pop (get name :resource-freel)))
	(return-from allocate-resource (car try))))
    ;; none found; init one
    (let ((new (if args (apply (get name :resource-cons) args)
		   (funcall (get name :resource-cons)))))
      (if args
          (apply (get name :resource-init) new args)
          (funcall (get name :resource-init) new))
      (push new (get name :resource-pool))
      (push (copy-list args) (get name :resource-desc))
      new)))

(defun deallocate-resource (name object)
  "Return object to pool name. It's a bad idea to free an object to the wrong
   pool. Name is evaluated."
  #+symbolics
  (scl:deallocate-resource name object)
  #-symbolics
  (funcall (get name :resource-free) object)
  (push object (get name :resource-freel)))

(defun clear-resource (name)
  "Zaps Name's pool, and starts from empty. Normally only used within 
   DefResource when recompiled, or user should call if you change the
   constructor s.t. the old objects in the pool are obsolete. 

   Not a good idea to call this if active objects are in use."
  #+symbolics
  (scl:clear-resource name)
  #-symbolics
  (progn
    (setf (get name :resource-freel) nil)
    (dolist (res (get name :resource-pool))
      (funcall (get name :resource-free) res)
      (push res (get name :resource-freel)))))

(defmacro map-resource (function name)
  "Incompatibly defined wrt Lispm, this one is more like mapcar - the
   function is called over all resources of type Name, which
   is evaluated(!) The args are the object, and t if the object is in use,
    nil otherwise."
  #+symbolics
  `(scl:map-resource ,name ,function)
  #-symbolics
  `(mapcar #'(lambda (ob)
	       (apply ,function ob (member ob (get ',name :resource-freel))))
	   (get ',name :resource-pool)))

(defmacro with-resource ((resource name &rest args) &body body)
  "Resource is bound to a Named resource initialized with Args.
   Name is **not** eval'd but args are.
   The resource is freed when the form is exited."
  #+symbolics
  `(scl:using-resource (,resource ,name ,@args) ,@body)
  #-symbolics
  `(let ((,resource (allocate-resource ',name ,@args)))
     (unwind-protect
	 (progn ,@body)
       (deallocate-resource ',name ,resource))))

;;; *EOF*
