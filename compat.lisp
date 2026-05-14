;;; compat.lisp — MacLisp compatibility shims
;;;
;;; MacLisp→CL mappings used by the Micro-Planner translation.
;;; None of these shadow standard CL names; they fill in things
;;; that simply don't exist in CL.

(in-package :microplanner)

;;; ---- Arithmetic aliases -------------------------------------------------

(defun add1 (n) (1+ n))
(defun sub1 (n) (1- n))

;;; MacLisp names for arithmetic predicates/ops (CL uses infix symbols)
(defun greaterp (a b) (> a b))
(defun plus    (&rest args) (apply #'+ args))
(defun times   (&rest args) (apply #'* args))
(defun quotient  (a b) (/ a b))
(defun difference (a b) (- a b))

;;; ---- List utilities -----------------------------------------------------

(defun assq (key alist)
  "MacLisp ASSQ: ASSOC with EQ test."
  (assoc key alist :test #'eq))

(defun memq (item list)
  "MacLisp MEMQ: MEMBER with EQ test."
  (member item list :test #'eq))

(defun sassq (key alist default-fn)
  "MacLisp SASSQ: like ASSQ but calls DEFAULT-FN (no args) when KEY absent."
  (or (assoc key alist :test #'eq) (funcall default-fn)))

;;; ---- Property lists -----------------------------------------------------

(defmacro defprop (atom value indicator)
  "MacLisp DEFPROP: set property INDICATOR on ATOM to VALUE at load time."
  `(setf (get ',atom ',indicator) ',value))

(defun putprop (atom value indicator)
  "MacLisp PUTPROP: (putprop atom value indicator) — note arg order differs from CL setf."
  (setf (get atom indicator) value))

;;; ---- Error handling: ERRSET / ERR ---------------------------------------
;;;
;;; MacLisp ERRSET catches any error; returns (list result) on success,
;;; NIL on error.  ERR signals a continuable error (used inside ERRSET
;;; to exit from pattern-match failures).

(define-condition planner-fail (error) ()
  (:report "Planner match failure"))

(defmacro errset (form &optional flag)
  "MacLisp ERRSET: evaluate FORM catching all errors.
   Returns (list result) on success, NIL if any error is signalled."
  (declare (ignore flag))
  `(handler-case (list ,form)
     (error () nil)))

(defun err (x)
  "MacLisp ERR NIL: signal a Planner-level failure, caught by ERRSET."
  (declare (ignore x))
  (error 'planner-fail))

;;; ---- I/O ----------------------------------------------------------------

(defun readch (&optional (stream *standard-input*))
  "MacLisp READCH: read one character, return as single-uppercase-char symbol."
  (let ((c (read-char stream nil nil)))
    (when c (intern (string (char-upcase c)) *package*))))

;;; ---- Symbol utilities ---------------------------------------------------

(defun explode (x)
  "MacLisp EXPLODE: split symbol name into list of single-char symbols."
  (map 'list (lambda (c) (intern (string c) *package*))
             (symbol-name x)))

(defun readlist (chars)
  "MacLisp READLIST: inverse of EXPLODE — build a symbol from a char-symbol list."
  (intern (coerce (mapcar (lambda (s) (char (symbol-name s) 0)) chars)
                  'string)
          *package*))

;;; ---- Oblist (all interned symbols) -------------------------------------
;;;
;;; In MacLisp, MAKOBLIST returns a list of hash-table buckets, each of
;;; which is a list of atoms.  THSTATE iterates with two nested MAPCs.
;;; We return a single-element list containing all package symbols.

(defun makoblist (arg)
  "MacLisp MAKOBLIST: return list-of-lists of all interned symbols."
  (declare (ignore arg))
  (let (syms)
    (do-symbols (s *package*) (push s syms))
    (list syms)))
