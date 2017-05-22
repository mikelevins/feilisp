(in-package #:fey-internal)

(defun clause-head (clause)
  (first clause))

(defun clause-body (clause)
  (rest clause))

;;; clauses are stored on the predicate's plist

(defun get-clauses (pred)
  (get pred 'clauses))

(defun predicate (relation)
  (first relation))

(defparameter *predicates-db* nil)

(defmacro <- (&rest clause)
  `(add-clause ',(replace-?-vars clause)))

(defun add-clause (clause)
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred)(not (variable-p pred))))
    (pushnew pred *predicates-db*)
    (setf (get pred 'clauses)
          (nconc (get-clauses pred)
                 (list clause)))
    pred))

(defun clear-db ()
  (mapc #'clear-predicate *predicates-db*))

(defun clear-predicate (predicate)
  (setf (get predicate 'clauses)
        nil))

(defun prove (goal bindings other-goals)
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
        (some
         #'(lambda (clause)
             (let ((new-clause (rename-variables clause)))
               (prove-all
                (append (clause-body new-clause) other-goals)
                (unify goal (clause-head new-clause) bindings))))
         clauses)
        (funcall clauses (rest goal) bindings
                 other-goals))))

(defun prove-all (goals bindings)
  (cond ((eq bindings fail) fail)
        ((null goals) bindings)
        (t (prove (first goals) bindings (rest goals)))))

(defun rename-variables (x)
  (sublis (mapcar #'(lambda (var)
                      (cons var (gensym (string var))))
                  (variables-in x))
          x))

(defun variables-in (exp)
  (unique-find-anywhere-if #'variable-p exp))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  (if (atom tree)
      (if (funcall predicate tree)
          (adjoin tree found-so-far)
          found-so-far)
      (unique-find-anywhere-if
       predicate
       (first tree)
       (unique-find-anywhere-if predicate (rest tree)
                                found-so-far))))

(defun replace-?-vars (exp)
  (cond ((eq exp '?) (gensym "?"))
        ((atom exp) exp)
        (t (reuse-cons (replace-?-vars (first exp))
                       (replace-?-vars (rest exp))
                       exp))))

(defmacro ?- (&rest goals)
  `(top-level-prove ',(replace-?-vars goals)))

(defun top-level-prove (goals)
  (prove-all `(,@goals (show-prolog-vars ,@(variables-in goals)))
             no-bindings)
  (format t "~&No.")
  (values))

(defun show-prolog-solutions (vars solutions)
  (if (null solutions)
      (format t "~&No.")
      (mapc #'(lambda (solution) (show-prolog-vars vars solution))
            solutions))
  (values))

;;; primitive show-prolog-vars
(defun show-prolog-vars (vars bindings other-goals)
  (if (null vars)
      (format t "~&Yes.")
      (dolist (var vars)
        (format t "~&~a = ~a" var
                (subst-bindings bindings var))))
  (if (continue-p)
      fail
      (prove-all other-goals bindings)))

;;; register primitives
(setf (get 'show-prolog-vars 'clauses) 'show-prolog-vars)

(defun continue-p ()
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-p))
    (otherwise
     (format t " Type ; to see more or . to stop")
     (continue-p))))
