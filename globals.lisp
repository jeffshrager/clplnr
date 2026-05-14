;;; globals.lisp — Micro-Planner global (dynamic/special) variables
;;;
;;; Every variable declared SPECIAL in plnr.181 becomes a DEFVAR here.
;;; Because DEFVAR marks a variable globally special in CL, any subsequent
;;; binding (LET, PROG, function parameter) automatically creates a dynamic
;;; binding — exactly matching MacLisp's SPECIAL declaration semantics.

(in-package :microplanner)

;;; ---- Core interpreter state --------------------------------------------

(defvar thtree nil
  "Backtracking/continuation stack.
   Each frame is a list (FRAME-TYPE . data); the CAR names the type
   whose THFAIL and THSUCCEED handlers are looked up via GET.")

(defvar thalist '((nil nil))
  "Variable binding alist.  Each entry is (VAR-NAME VALUE . restrictions).
   THUNASSIGNED in the VALUE slot means the variable is not yet bound.")

(defvar thvalue 'thnoval
  "Current result value threaded through the THVAL interpreter loop.
   NIL triggers the THFAIL handler; non-NIL triggers THSUCCEED.
   THNOVAL is the initial 'no value yet' sentinel.")

(defvar thexp nil
  "One-slot lookahead: next expression THVAL should evaluate before
   proceeding to the next body statement.  THASSERT sets this to
   trigger antecedent theorems; THPROG/THPROGA advance it through body.")

(defvar tholist nil
  "Saved outer THALIST — captured before entering a theorem or THPROG
   so THMATCH2 can distinguish outer-scope from inner-scope variables.")

(defvar thlevel nil
  "Stack of (THTREE THALIST) pairs saved across nested THVAL calls.
   Each THVAL call pushes on entry and pops on return.")

(defvar thbranch nil
  "THTREE snapshot at the most recent success point, for backtracking.")

(defvar thabranch nil
  "THALIST snapshot corresponding to THBRANCH.")

(defvar thmessage nil
  "Non-NIL when THMESSAGE is active; holds (frame-pointer message-expr).")

(defvar thinf nil
  "Misc flag used by the THERT read loop.")

;;; ---- Tracing and stepping -----------------------------------------------

(defvar thtrace nil
  "When non-NIL, print a trace of each THGOAL, THASSERT, etc.")

(defvar thstep nil
  "If non-NIL, EVAL this expression at the start of each THVAL step.")

(defvar thstepd nil
  "If non-NIL, EVAL after each THVAL step (down).")

(defvar thstept nil
  "If non-NIL, EVAL on each success.")

(defvar thstepf nil
  "If non-NIL, EVAL on each failure.")

;;; ---- Naming ------------------------------------------------------------

(defvar thgename 0
  "Counter used by THGENAME to generate unique theorem/variable names.")

(defvar thversion 181
  "Micro-Planner version number (mirrors the ITS file version).")

;;; ---- Variable-marker list ----------------------------------------------

(defvar thv '(thv thnv)
  "List of symbols that mark Planner variables in patterns.
   Set to (THV THNV) by THINIT and also at the start of each THVAL call.")

;;; ---- Temporaries shared across function calls --------------------------
;;; These are declared SPECIAL in plnr.181 and accessed as free variables
;;; across function-call boundaries (e.g. THGOAL → THTRY).

(defvar thxx nil   "Temp used by THGAL, THV1 for error reporting.")
(defvar ^a   nil   "ITS ctrl-A interrupt flag; checked each THVAL step.")

;;; Variables shared between THGOAL and THTRY (THGOAL binds them
;;; dynamically; THTRY reads them as free variables):
(defvar tha2 nil)   ; instantiated goal pattern
(defvar thy  nil)   ; assertion candidate bucket
(defvar thy1 nil)   ; flag: assertion bucket already computed
(defvar thz  nil)   ; consequent theorem bucket
(defvar thz1 nil)   ; flag: theorem bucket already computed

;;; Variables shared between THADD/THREMOVE and THIP/THREM1:
(defvar thtt  nil)  ; the theorem/assertion being added
(defvar thttl nil)  ; the "canonical" form of THTT
(defvar thfst nil)  ; flag: first pass through index
(defvar thfstp nil) ; flag: first-pass with variable check
(defvar thlas nil)  ; length of pattern
(defvar thnf  nil)  ; position counter within pattern
(defvar thwh  nil)  ; property indicator (THASSERTION, THCONSE, etc.)

;;; Variables shared between THREMOVE and THREM1:
(defvar thbs  nil)
(defvar thon  nil)
(defvar thal  nil)
(defvar thpc  nil)

;;; Variables shared between THMATCH1 and THMATCH2:
(defvar thml  nil)  ; list of undo assignments made during this match

;;; Misc specials from various functions:
(defvar thbranch1 nil)  ; local in THFAIL — shadow for outer thbranch

;;; Variables shared between THASS1 and THTAE (THASS1 binds dynamically;
;;; THTAE reads them as free variables via MacLisp dynamic scoping):
(defvar thx    nil)  ; current assertion/pattern being processed
(defvar thtype nil)  ; theorem type: THANTE or THERASING
