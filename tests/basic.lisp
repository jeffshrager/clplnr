;;; tests/basic.lisp — Smoke tests for the Micro-Planner CL translation
;;;
;;; Run with:
;;;   sbcl --noinform \
;;;     --load package.lisp --load compat.lisp --load globals.lisp --load planner.lisp \
;;;     --load tests/basic.lisp --eval "(quit)"
;;;
;;; Each test prints PASS or FAIL.
;;;
;;; Micro-Planner semantics notes:
;;;   THASSERT returns ((assertion)) — the assertion cons'd with its property list.
;;;   THGOAL returns the matched assertion record ((assertion . props)) on success, NIL on failure.
;;;   Variables must be pre-bound (at least THUNASSIGNED) before use in THV goals;
;;;   use THPROG (x y z) to introduce variables into thalist.

(in-package :microplanner)

(defvar *pass* 0)
(defvar *fail* 0)

(defmacro check (label form expected)
  `(let ((result (handler-case ,form
                   (error (c) (list :error (princ-to-string c)))))
         (exp ,expected))
     (cond ((equal result exp)
            (incf *pass*)
            (format t "~&PASS ~A~%" ,label))
           (t
            (incf *fail*)
            (format t "~&FAIL ~A~%  expected: ~S~%  got:      ~S~%"
                    ,label exp result)))))

;;; ---- Reset planner state before tests ------------------------------------

(thinit)

;;; =========================================================================
;;; Test 1: THASSERT returns the assertion record ((assertion . props))
;;; =========================================================================

(let ((r (thassert (human socrates))))
  (check "thassert-returns-record"
         (equal r '((human socrates)))
         t))

;;; =========================================================================
;;; Test 2: THGOAL succeeds on asserted fact — returns assertion record
;;; =========================================================================

(check "thgoal-fact-known"
       (thval '(thgoal (human socrates)) thalist)
       '((human socrates)))

;;; =========================================================================
;;; Test 3: THGOAL fails on unknown fact — returns NIL
;;; =========================================================================

(check "thgoal-fact-unknown"
       (thval '(thgoal (human plato)) thalist)
       nil)

;;; =========================================================================
;;; Test 4: THERASE removes an assertion; subsequent THGOAL fails
;;; =========================================================================

(thassert (mortal socrates))
(therase  (mortal socrates))
(check "therase-removes"
       (thval '(thgoal (mortal socrates)) thalist)
       nil)

;;; =========================================================================
;;; Test 5: THGOAL with variable inside THPROG
;;;
;;; THPROG (x) binds x as THUNASSIGNED into thalist.
;;; (THGOAL (HUMAN (THV X))) matches and binds x to socrates (or turing).
;;; THPROG exhausts its body without THRETURN → returns THNOVAL (truthy).
;;; =========================================================================

(thassert (human turing))

(check "thgoal-with-var-in-thprog"
       ;; THNOVAL is the "succeeded with no explicit return value" sentinel
       (thval '(thprog (x) (thgoal (human (thv x)))) thalist)
       'thnoval)

;;; =========================================================================
;;; Summary
;;; =========================================================================

(format t "~%Results: ~A passed, ~A failed~%" *pass* *fail*)
(when (plusp *fail*)
  (sb-ext:exit :code 1))
