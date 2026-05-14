;;; tests/theorems.lisp — Graduated theorem tests for Micro-Planner CL
;;;
;;; Progression:
;;;   T6–T8  : THCONSE (consequent theorem) — classic manual examples
;;;   T9–T10 : THANTE (antecedent theorem) — trigger on assertion
;;;   T11    : chained THCONSE inference
;;;   T12    : THAND conjunction
;;;   T13    : THOR disjunction
;;;   T14–T15: set-theory proof from setthy.5/6
;;;
;;; Sources:
;;;   manual.180 (3100015): (THASSERT (HUMAN TURING)) + fallible example
;;;   setthy.5/6 (3100090): full set-theory theorem-proving example
;;;
;;; Run with:
;;;   sbcl --noinform \
;;;     --load package.lisp --load compat.lisp --load globals.lisp --load planner.lisp \
;;;     --load tests/theorems.lisp --eval "(quit)"

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
            (format t "~&FAIL ~A~%  expected: ~S~%  got:      ~S~%" ,label exp result)))))

;;; Helpers
(defmacro check-truthy (label form)
  `(let ((result (handler-case ,form
                   (error (c) (list :error (princ-to-string c))))))
     (cond (result
            (incf *pass*)
            (format t "~&PASS ~A  [~S]~%" ,label result))
           (t
            (incf *fail*)
            (format t "~&FAIL ~A  (got NIL)~%" ,label)))))

;;; =========================================================================
;;; T6: Simple THCONSE — manual.180 "fallible" example
;;;
;;; From the Micro-Planner manual (p. 4120):
;;;   (THASSERT (HUMAN TURING))
;;;   (THCONSE (X) (FALLIBLE $?X) (THGOAL (HUMAN $?X)))
;;;   (THGOAL (FALLIBLE TURING) $T)   ; $T = (THTBF THTRUE)
;;; =========================================================================

(thinit)
(thassert (human turing))
(thconse (x) (fallible $?x) (thgoal (human $?x)))

;;; Theorem-based goal: $T enables search of consequent theorems
(check-truthy "T6-fallible-turing-via-theorem"
  (thval '(thgoal (fallible turing) $t) thalist))

;;; No theorem for robot → should fail
(check "T6-fallible-robot-fails"
  (thval '(thgoal (fallible robot) $t) thalist)
  nil)

;;; =========================================================================
;;; T7: THCONSE applies to multiple ground instances
;;; =========================================================================

(thinit)
(thassert (human socrates))
(thassert (human turing))
(thassert (human plato))
(thconse (x) (fallible $?x) (thgoal (human $?x)))

(check-truthy "T7-fallible-socrates" (thval '(thgoal (fallible socrates) $t) thalist))
(check-truthy "T7-fallible-turing"   (thval '(thgoal (fallible turing)   $t) thalist))
(check-truthy "T7-fallible-plato"    (thval '(thgoal (fallible plato)    $t) thalist))
(check "T7-fallible-aristotle-fails" (thval '(thgoal (fallible aristotle) $t) thalist) nil)

;;; =========================================================================
;;; T8: THANTE — antecedent theorem triggers when assertion is made
;;;
;;; (THANTE (X) (HUMAN $?X) (THASSERT (ANIMAL $?X)))
;;; When (HUMAN ARISTOTLE) is asserted, (ANIMAL ARISTOTLE) is also asserted.
;;; =========================================================================

(thinit)
(thante human-is-animal (x) (human $?x) (thassert (animal $?x)))
(thassert (human aristotle))

;;; After assertion, (ANIMAL ARISTOTLE) should now be in the database
(check-truthy "T8-animal-derived-from-human"
  (thval '(thgoal (animal aristotle)) thalist))

;;; =========================================================================
;;; T9: Chained THCONSE — two-step inference
;;;
;;; Human → Fallible → Imperfect
;;; =========================================================================

(thinit)
(thconse (x) (fallible   $?x) (thgoal (human    $?x)))
(thconse (x) (imperfect  $?x) (thgoal (fallible $?x) $t))
(thassert (human socrates))

(check-truthy "T9-imperfect-socrates-chained"
  (thval '(thgoal (imperfect socrates) $t) thalist))

;;; =========================================================================
;;; T10: THCONSE with variable in goal — bind via THPROG
;;;
;;; Assert two humans, ask for any fallible thing.
;;; =========================================================================

(thinit)
(thassert (human turing))
(thassert (human lovelace))
(thconse (x) (fallible $?x) (thgoal (human $?x)))

;;; THPROG introduces the variable, THGOAL binds it, THRETURN captures the value
(check-truthy "T10-find-a-fallible-person"
  (thval '(thprog (x) (thgoal (fallible (thv x)) $t)) thalist))

;;; =========================================================================
;;; T11: THAND — conjunction
;;;
;;; Find someone who is both human and mortal.
;;; =========================================================================

(thinit)
(thassert (human socrates))
(thassert (mortal socrates))
(thassert (human robot))

(check-truthy "T11-thand-human-and-mortal"
  (thval '(thprog (x)
                  (thand (thgoal (human  (thv x)))
                         (thgoal (mortal (thv x)))))
         thalist))

;;; Robot is human but not mortal — shouldn't satisfy both
;;; (This tests that AND requires both conjuncts)
(check "T11-thand-robot-not-mortal"
  (thval '(thprog (x)
                  (thand (thgoal (human  robot))
                         (thgoal (mortal robot))))
         thalist)
  nil)

;;; =========================================================================
;;; T12: THOR — disjunction
;;;
;;; Succeed if either (P A) or (P B) holds.
;;; =========================================================================

(thinit)
(thassert (likes alice))
(thassert (likes bob))

(check-truthy "T12-thor-first-disjunct"
  (thval '(thor (thgoal (likes alice))
                (thgoal (likes nobody)))
         thalist))

(check-truthy "T12-thor-second-disjunct"
  (thval '(thor (thgoal (likes nobody))
                (thgoal (likes bob)))
         thalist))

(check "T12-thor-neither"
  (thval '(thor (thgoal (likes nobody))
                (thgoal (likes anyone)))
         thalist)
  nil)

;;; =========================================================================
;;; T13: SET THEORY — from setthy.5 / setthy.6
;;;
;;; Facts:
;;;   (C0 INTERSECT A0 B0)  — C0 = A0 ∩ B0
;;;   (D0 POWER C0)         — D0 = P(C0)  (power set)
;;;   (E0 POWER A0)         — E0 = P(A0)
;;;   (F0 POWER B0)         — F0 = P(B0)
;;;   (G0 INTERSECT E0 F0)  — G0 = E0 ∩ F0
;;;
;;; Goal: (D0 SUBSET G0)  — prove D0 ⊆ G0
;;;
;;; Theorems:
;;;   TH1  (THCONSE): X ⊆ Y  ←  ∃T: (T∈X ∧ T∈Y)  [subset by element witness]
;;;   TH4-A(THCONSE): T∈Y   ←  (Y=P(X) ∧ T⊆X)   [power set membership]
;;;   TH4-B(THANTE) : T∈P   →  (P=P(X) → T⊆X)   [derive subset from power-set membership]
;;;   TH3  (THCONSE): T∈X   ←  (X=A∩B ∧ T∈A ∧ T∈B) [intersection membership]
;;;   TH2  (THANTE) : T⊆I   →  (I=A∩B → T⊆A ∧ T⊆B) [derive subsets from intersection]
;;; =========================================================================

(thinit)

;;; Theorems (translated from setthy.5)
;;; TH1: to prove $?X SUBSET $?Y, introduce witness T, assert T∈X, then goal T∈Y
(thconse th1 (x y (t (gensym)))
  ($?x subset $?y)
  (thunique 'th1 $?x $?y)
  (thassert ($?t element $?x) $t)
  (thgoal   ($?t element $?y) $t))

;;; TH4-A: T∈Y  ← Y=P(X) ∧ T⊆X
(thconse th4-a (t x y)
  ($?t element $?y)
  (thgoal ($?y power $?x))
  (thunique 'th4-a $?t $?x $?y)
  (thgoal ($?t subset $?x) $t))

;;; TH4-B: T∈P → P=P(X) → T⊆X
(thante th4-b (t p x)
  ($?t element $?p)
  (thgoal ($?p power $?x))
  (thassert ($?t subset $?x) $t))

;;; TH3: T∈X  ← X=A∩B ∧ T∈A ∧ T∈B
(thconse th3 (t x a b)
  ($?t element $?x)
  (thgoal ($?x intersect $?a $?b))
  (thunique 'th3 $?t $?x $?a $?b)
  (thgoal ($?t element $?a) $t)
  (thgoal ($?t element $?b) $t))

;;; TH2: T⊆I → I=A∩B → T⊆A ∧ T⊆B
(thante th2 (t i x y)
  ($?t subset $?i)
  (thgoal ($?i intersect $?x $?y))
  (thassert ($?t subset $?x) $t)
  (thassert ($?t subset $?y) $t))

;;; Database facts
(thdata
  ((c0 intersect a0 b0))
  ((d0 power c0))
  ((e0 power a0))
  ((f0 power b0))
  ((g0 intersect e0 f0)))

;;; The query: prove D0 ⊆ G0
(check-truthy "T13-set-theory-d0-subset-g0"
  (thval '(thgoal (d0 subset g0) $t) thalist))

;;; =========================================================================
;;; Summary
;;; =========================================================================

(format t "~%Results: ~A passed, ~A failed~%" *pass* *fail*)
(when (plusp *fail*)
  (sb-ext:exit :code 1))
