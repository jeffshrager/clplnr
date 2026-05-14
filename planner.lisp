;;; planner.lisp — Micro-Planner in Common Lisp
;;;
;;; Direct translation of plnr.181 (ITS/MacLisp, Jan 25 1978) from
;;; MITDDC_MicroPlanner_repo/plnr/3100090/plnr/plnr.181.
;;;
;;; Translation conventions (documented in notes/translation_decisions.md):
;;;   FEXPR foo (ARGS) body  →  (defmacro foo (&rest args) `(foo-fn ',args))
;;;                              (defun foo-fn (args) body)
;;;   DEFPROP a v i          →  (setf (get 'a 'i) 'v)   [via defprop macro]
;;;   PUTPROP a v i          →  (putprop a v i)           [in compat.lisp]
;;;   *CATCH / *THROW        →  catch / throw
;;;   ERRSET / ERR           →  handler-case / error      [in compat.lisp]
;;;   SASSQ                  →  sassq                      [in compat.lisp]
;;;   (FUNCTION (LAMBDA...)) →  (lambda ...)  or  #'(lambda ...)
;;;   (PROG2 0. x y)         →  (prog2 nil x y)
;;;   SUB1/ADD1              →  sub1/add1                  [in compat.lisp]
;;;   MacLisp specials       →  defvar globals             [in globals.lisp]

(in-package :microplanner)

;;; =========================================================================
;;; THE $ READ MACRO
;;; =========================================================================
;;;
;;; In MacLisp, SSTATUS MACRO $ 'THREAD installs THREAD as the reader for $.
;;; We install it here via SET-MACRO-CHARACTER.
;;;
;;; $?X  → (THV X)       ; value of Planner variable X
;;; $_X  → (THNV X)      ; new (unbound) Planner variable X
;;; $EX  → (THEV X)      ; evaluate X as Lisp, use result as pattern
;;; $T   → (THTBF THTRUE)
;;; $R   → THRESTRICT
;;; $G   → THGOAL
;;; $A   → THASSERT
;;; $N n → (THANUM n)
;;; && … && → comment

(defun thread (stream dispatch-char)
  "Reader function for the $ prefix macro character."
  (declare (ignore dispatch-char))
  (let ((char (char-upcase (read-char stream t nil t))))
    (cond ((char= char #\?) (list 'thv  (read stream t nil t)))
          ((char= char #\_) (list 'thnv (read stream t nil t)))
          ((char= char #\E) (list 'thev (read stream t nil t)))
          ((char= char #\&)
           (loop (when (char= (read-char stream t nil t) #\&)
                   (return '(comment)))))
          ((char= char #\T) '(thtbf thtrue))
          ((char= char #\R) 'threstrict)
          ((char= char #\G) 'thgoal)
          ((char= char #\A) 'thassert)
          ((char= char #\N) (list 'thanum (read stream t nil t)))
          (t (error "Illegal $ prefix character: $~C" char)))))

(eval-when (:load-toplevel :execute)
  (set-macro-character #\$ #'thread))

;;; =========================================================================
;;; BASIC MACROS AND UTILITIES
;;; =========================================================================

(defmacro thpush (place item)
  "MacLisp (THPUSH LIST ITEM) ≡ (SETQ LIST (CONS ITEM LIST))."
  `(setq ,place (cons ,item ,place)))

(defun evlis (x)
  "Evaluate each element of X (for side effects); return X."
  (mapc #'eval x))

(defun thprintc (x) (terpri) (princ x) (princ " "))
(defun thprint2 (x) (princ " ") (princ x))

(defun thtraces (kind what)
  "Simple trace output — print KIND and WHAT."
  (format t "~&[~A ~S]~%" kind what))

(defun thtrue (x)
  "The universal filter — always succeeds."
  (declare (ignore x))
  t)

;;; =========================================================================
;;; THPOPT — pop one frame off THTREE
;;; =========================================================================

(defun thpopt ()
  (setq thtree (cdr thtree)))

;;; =========================================================================
;;; THVAR — test whether X is a Planner variable form
;;; =========================================================================

(defun thvar (x)
  "(THVAR X) — T iff X is (THV name) or (THNV name)."
  (and (consp x) (memq (car x) '(thv thnv))))

;;; =========================================================================
;;; THPUTPROP — backtrackable PUTPROP
;;; =========================================================================
;;;
;;; Unlike plain PUTPROP, THPUTPROP records the old value on THTREE so
;;; the change can be undone on backtracking (via THMUNG/THMUNGF).

(defun thputprop (ato val ind)
  (thpush thtree
          (list 'thmung
                (list (list 'putprop (list 'quote ato)
                            (list 'quote (get ato ind))
                            (list 'quote ind)))))
  (putprop ato val ind))

;;; =========================================================================
;;; VARIABLE BINDING: THBIND, THBI1, THGAL, THSGAL
;;; =========================================================================

(defun thbind (a)
  "Bind variables A onto THALIST; push a THREMBIND restore frame onto THTREE."
  (let ((tholist thalist))
    (setq tholist thalist)
    (or (null a)
        (prog ()
         go
          (cond ((null a)
                 (thpush thtree (list 'thrembind tholist))
                 (return t)))
          (thpush thalist
                  (cond ((atom (car a))
                         (list (car a) 'thunassigned))
                        ((eq (caar a) 'threstrict)
                         (nconc (thbi1 (cadar a)) (cddar a)))
                        (t (list (caar a) (eval (cadar a))))))
          (setq a (cdr a))
          (go go)))))

(defun thbi1 (x)
  (cond ((atom x) (list x 'thunassigned))
        (t (list (car x) (eval (cadr x))))))

(defun thgal (x y)
  "(THGAL var-form thalist) — return binding cell of variable named (CADR x)."
  (setq thxx x)
  (sassq (cadr x) y
         (lambda ()
           (print thxx)
           (thert-fn (list 'thunbound 'thgal)))))

(defun thsgal (x)
  "Like THGAL but for THSETQ — adds a new unbound entry if absent."
  (sassq (cadr x) thalist
         (lambda ()
           (prog (y)
             (setq y (list (cadr x) 'thunassigned))
             (nconc (get 'thalist 'value) (list y))
             (return y)))))

;;; =========================================================================
;;; VARIABLE SUBSTITUTION: THRPLACAS, THRPLACDS, THVARS2, THVARSUBST
;;; =========================================================================

(defun thrplacas (cell val)
  "Set CAR of CELL to VAL and record the undo on THML."
  (thpush thml (list 'rplaca (list 'quote cell) (list 'quote (car cell))))
  (rplaca cell val))

(defun thrplacds (cell val)
  "Set CDR of CELL to VAL and record the undo on THML."
  (thpush thml (list 'rplacd (list 'quote cell) (list 'quote (cdr cell))))
  (rplacd cell val))

(defun thrplaca (x y)
  (prog (thml)
    (thrplacas x y)
    (thpush thtree (list 'thmung thml))
    (return y)))

(defun thrplacd (x y)
  (prog (thml)
    (thrplacds x y)
    (thpush thtree (list 'thmung thml))
    (return y)))

(defmacro thurplaca (&rest l)
  `(rplaca ,(car l) ,(cadr l)))

(defmacro thurplacd (&rest l)
  `(rplacd ,(car l) ,(cadr l)))

(defun thvars2 (x)
  "Substitute an assigned variable (or $E form) within a pattern element."
  (prog (a)
    (cond ((and (consp x) (eq (car x) 'thev))
           (return (thval (cadr x) tholist)))
          ((and (consp x) (memq (car x) '(thv thnv)))
           (setq a (sassq (cadr x) thalist (lambda () nil)))
           (return (cond ((null a) x)
                         ((eq (cadr a) 'thunassigned) x)
                         (t (cadr a)))))
          (t (return x)))))

(defun thvarsubst (thx thy)
  "Substitute current bindings into pattern THX.
   THY non-NIL means check at top level for $E or bare variable."
  (cond ((and (consp thx) (eq (car thx) 'thev))
         (setq thx (thval (cadr thx) thalist)))
        ((thvar thx) (setq thx (eval thx))))
  (cond ((atom thx) thx)
        (t (mapcar #'thvars2 thx))))

;;; =========================================================================
;;; PATTERN MATCHING: THCHECK, THUNION, THMATCH2, THMATCH1, THMATCHLIST
;;; =========================================================================

(defun thcheck (thprd thx)
  "Check that all restriction predicates in THPRD pass for THX."
  (or (null thprd)
      (eq thx 'thunassigned)
      (errset (mapc (lambda (thy)
                      (or (funcall thy thx) (err nil)))
                    thprd))))

(defun thunion (l1 l2)
  "Set union: add elements of L1 not already in L2."
  (mapc (lambda (thx)
          (cond ((member thx l2))
                (t (setq l2 (cons thx l2)))))
        l1)
  l2)

(defun thmatch2 (thx thy)
  "Match one element THX of a pattern against one element THY of a candidate.
   Binds/links variables via THRPLACAS; signals ERR on mismatch."
  ;; Handle $E on either side
  (and (consp thx) (eq (car thx) 'thev)
       (setq thx (thval (cadr thx) tholist)))
  (and (consp thy) (eq (car thy) 'thev)
       (setq thy (thval (cadr thy) thalist)))
  (cond
    ;; wildcards
    ((eq thx '?) t)
    ((eq thy '?) t)
    ;; either side is a variable (or restricted variable)
    ((or (memq (car-safe thx) '(thv thnv threstrict))
         (memq (car-safe thy) '(thv thnv threstrict)))
     ;; look up binding cells for x and y
     (let ((xpair (cond ((thvar thx) (thgal thx tholist))
                        ((eq (car-safe thx) 'threstrict)
                         (cond ((eq (cadr thx) '?)
                                (prog2 nil
                                       (cons '? (cons 'thunassigned
                                                      (append (cddr thx) nil)))
                                       (setq thx '(thnv ?))))
                               (t (let ((u (thgal (cadr thx) tholist)))
                                    (thrplacds (cdr u)
                                               (thunion (cddr u) (cddr thx)))
                                    (setq thx (cadr thx))
                                    u))))
                        (t nil)))
           (ypair (cond ((thvar thy) (thgal thy thalist))
                        ((eq (car-safe thy) 'threstrict)
                         (cond ((eq (cadr thy) '?)
                                (prog2 nil
                                       (cons '? (cons 'thunassigned
                                                      (append (cddr thy) nil)))
                                       (setq thy '(thnv ?))))
                               (t (let ((u (thgal (cadr thy) thalist)))
                                    (thrplacds (cdr u)
                                               (thunion (cddr u) (cddr thy)))
                                    (setq thy (cadr thy))
                                    u))))
                        (t nil))))
       (cond
         ;; THX is an unassigned variable
         ((and xpair
               (or (eq (car thx) 'thnv)
                   (and (eq (car thx) 'thv)
                        (eq (cadr xpair) 'thunassigned)))
               (thcheck (cddr xpair)
                        (cond (ypair (cadr ypair)) (t thy))))
          (cond (ypair
                 ;; link two variables
                 (thrplacas (cdr xpair) (cadr ypair))
                 (and (cddr ypair)
                      (thrplacds (cdr xpair)
                                 (thunion (cddr xpair) (cddr ypair))))
                 (thrplacds ypair (cdr xpair)))
                (t
                 ;; assign variable to constant
                 (thrplacas (cdr xpair) thy))))
         ;; THY is an unassigned variable
         ((and ypair
               (or (eq (car thy) 'thnv)
                   (and (eq (car thy) 'thv)
                        (eq (cadr ypair) 'thunassigned)))
               (thcheck (cddr ypair)
                        (cond (xpair (cadr xpair)) (t thx))))
          (cond (xpair
                 (thrplacas (cdr ypair) (cadr xpair)))
                (t
                 (thrplacas (cdr ypair) thx))))
         ;; THX is assigned — check value equals THY
         ((and xpair (equal (cadr xpair)
                            (cond (ypair (cadr ypair)) (t thy)))))
         ;; THX constant, THY assigned variable equal to THX
         ((and ypair (equal (cadr ypair) thx)))
         ;; mismatch
         (t (err nil)))))
    ;; neither is a variable — must be EQUAL
    ((equal thx thy))
    (t (err nil))))

(defun car-safe (x)
  "Return CAR of X if X is a cons, else NIL."
  (and (consp x) (car x)))

(defun thmatch1 (thx thy)
  "Match pattern THX against candidate THY; return T and record bindings,
   or return NIL and undo any partial bindings."
  (prog (thml)
    (cond ((and (equal (length
                        (cond ((and (consp thx) (eq (car thx) 'thev))
                               (setq thx (thval (cadr thx) tholist)))
                              (t thx)))
                       (length thy))
                (errset (mapc #'thmatch2 thx thy)))
           (and thml (thpush thtree (list 'thmung thml)))
           (return t))
          (t (evlis thml) (return nil)))))

(defun thmatch (thx &rest args)
  "THMATCH: match with optional explicit tholist/thalist arguments."
  (let ((tholist (cond ((> thx 2) (nth 2 args)) (t thalist)))
        (thalist (cond ((> thx 3) (nth 3 args)) (t thalist))))
    (thmatch1 (first args) (second args))))

(defun thmatchlist (thtb thwh)
  "Search the database for candidates matching pattern THTB under indicator THWH.
   Returns the shortest candidate bucket found."
  (prog (thb1 thb2 thl thnf thal tha1 tha2 thrn thl1 thl2 thrvc)
    (setq thl 34359738367)        ; very large initial minimum
    (setq thnf 0)
    (setq thal (length thtb))
    (setq thb1 thtb)
   thp1
    (or thb1 (return (cond (thl2 (append thl1 thl2)) (thl1))))
    (setq thnf (add1 thnf))
    (setq thb2 (car thb1))
    (setq thb1 (cdr thb1))
   thp3
    (cond ((or (not (atom thb2))
               (numberp thb2)
               (eq thb2 '?))
           (go thp1))
          ((not (setq tha1 (get thb2 thwh)))
           (setq tha1 '(0 0)))
          ((eq tha1 'thnohash) (go thp1))
          ((not (setq tha1 (assq thnf (cdr tha1))))
           (setq tha1 '(0 0)))
          ((not (setq tha1 (assq thal (cdr tha1))))
           (setq tha1 '(0 0))))
    (setq thrn (cadr tha1))
    (setq tha1 (cddr tha1))
    ;; assertions: don't need to check variable bucket
    (and (eq thwh 'thassertion) (go thp2))
    ;; theorems: also check THVRB bucket (patterns with variables)
    (cond ((not (setq tha2 (get 'thvrb thwh)))
           (setq tha2 '(0 0)))
          ((not (setq tha2 (assq thnf (cdr tha2))))
           (setq tha2 '(0 0)))
          ((not (setq tha2 (assq thal (cdr tha2))))
           (setq tha2 '(0 0))))
    (setq thrvc (cadr tha2))
    (setq tha2 (cddr tha2))
    (and (greaterp (plus thrvc thrn) thl) (go thp1))
    (setq thl (plus thrvc thrn))
    (setq thl1 tha1)
    (setq thl2 tha2)
    (go thp1)
   thp2
    (cond ((eq thrn 0) (return nil))
          ((greaterp thl thrn) (setq thl1 tha1) (setq thl thrn)))
    (go thp1)))

;;; =========================================================================
;;; DATABASE: THIP, THREM1, THADD, THREMOVE
;;; =========================================================================

(defun thip (thi)
  "Add item THI to the inverted index under THWH at position THNF.
   Free variables: THWH, THNF, THLAS, THTTL, THFST, THFSTP, THFOO."
  (prog (tht1 tht3 thsv tht2 thi1)
    (setq thnf (add1 thnf))
    ;; classify THI
    (cond ((and (atom thi) (not (eq thi '?)) (not (numberp thi)))
           (setq thi1 thi))
          ((or (eq thi '?) (memq (car-safe thi) '(thv thnv)))
           (cond (thfst (return 'thvrb))
                 (t (setq thi1 'thvrb))))
          (t (return 'thvrb)))
    ;; navigate/create the three-level index: atom → position → length
    (cond ((not (setq tht1 (get thi1 thwh)))
           (putprop thi1
                    (list nil (list thnf (list thlas 1 thttl)))
                    thwh))
          ((eq tht1 'thnohash) (return 'thbqf))
          ((not (setq tht2 (assq thnf (cdr tht1))))
           (nconc tht1 (list (list thnf (list thlas 1 thttl)))))
          ((not (setq tht3 (assq thlas (cdr tht2))))
           (nconc tht2 (list (list thlas 1 thttl))))
          ((and (or thfst thfstp)
                (cond ((eq thwh 'thassertion) (assoc thtt (cddr tht3)))
                      (t (memq thtt (cddr tht3)))))
           ;; already present
           (return nil))
          ((setq thsv (cddr tht3))
           (rplaca (cdr tht3) (add1 (cadr tht3)))
           (rplacd (cdr tht3) (nconc (list thttl) thsv))))
    (return 'thok)))

(defun threm1 (thb)
  "Remove item THB from the inverted index under THWH.
   Free variables: THWH, THNF, THAL, THON, THBS, THFST, THFSTP."
  (prog (tha thsv tha1 tha2 tha3 tha4 tha5 thone thpc)
    (setq thnf (add1 thnf))
    (cond ((and (atom thb) (not (eq thb '?)) (not (numberp thb)))
           (setq tha thb))
          ((or (eq thb '?) (memq (car-safe thb) '(thv thnv)))
           (cond (thfst (return 'thvrb))
                 (t (setq tha 'thvrb))))
          (t (return 'thvrb)))
    (setq tha1 (get tha thwh))
    (or tha1 (return nil))
    (and (eq tha1 'thnohash) (return 'thbqf))
    (setq tha2 (thba thnf tha1))
    (or tha2 (return nil))
    (setq tha3 (thba thal (cadr tha2)))
    (or tha3 (return nil))
    (setq tha4 (cadr tha3))
    (setq thpc (not (eq thwh 'thassertion)))
    (setq tha5
          (cond ((or thfst thfstp) (thbap thbs (cdr tha4)))
                (t (thba (cond (thpc thon) (t (car thon)))
                         (cdr tha4)))))
    (or tha5 (return nil))
    (setq thone (cadr tha5))
    (rplacd tha5 (cddr tha5))
    (and (not (eq (cadr tha4) 1))
         (or (setq thsv (cddr tha4)) t)
         (rplaca (cdr tha4) (sub1 (cadr tha4)))
         (return thone))
    (setq thsv (cddr tha3))
    (rplacd tha3 thsv)
    (and (cdadr tha2) (return thone))
    (setq thsv (cddr tha2))
    (rplacd tha2 thsv)
    (and (cdr tha1) (return thone))
    (remprop tha thwh)
    (return thone)))

(defun thba (th1 th2)
  "Like ASSQ but return the cell BEFORE the matching one. Used by THIP/THREM1."
  (prog (thp)
    (setq thp th2)
   thp1
    (and (eq (cond (thpc (cadr thp)) (t (caadr thp))) th1)
         (return thp))
    (or (cdr (setq thp (cdr thp))) (return nil))
    (go thp1)))

(defun thbap (th1 th2)
  "Like THBA but with EQUAL instead of EQ."
  (prog (thp)
    (setq thp th2)
   thp1
    (and (equal (cond (thpc (cadr thp)) (t (caadr thp))) th1)
         (return thp))
    (or (cdr (setq thp (cdr thp))) (return nil))
    (go thp1)))

(defun thadd (thtt thpl)
  "Add theorem name or assertion THTT to the database.
   THPL is the property list for the item (NIL for plain assertions).
   Returns NIL if already present, else returns THTTL (the canonical form)."
  (prog (thnf thwh thck thlas tht1 thfst thfstp thttl thfoo)
    (setq thck
          (cond ((atom thtt)
                 ;; asserting a named theorem
                 (or (setq tht1 (get thtt 'theorem))
                     (prog2 (print thtt)
                            (thert-fn '(cant thassert no theorem - thadd))))
                 (setq thwh (car tht1))
                 (setq thttl thtt)
                 (and thpl
                      (prog ()
                       lp (thputprop thtt (cadr thpl) (car thpl))
                          (cond ((setq thpl (cddr thpl)) (go lp)))))
                 (caddr tht1))
                (t (setq thwh 'thassertion)
                   (setq thttl (cons thtt thpl))
                   thtt)))
    (setq thnf 0)
    (setq thlas (length thck))
    (setq thfst t)
    (setq thfoo nil)
   thp1
    (cond ((null thck)
           (setq thck thfoo)
           (setq thnf 0)
           (setq thfoo (setq thfst nil))
           (setq thfstp t)
           (go thp1))
          ((null (setq tht1 (thip (car thck)))) (return nil))
          ((eq tht1 'thok))
          ((setq thfoo
                 (nconc thfoo
                        (list (cond ((eq tht1 'thvrb) (car thck))))))
           (setq thck (cdr thck))
           (go thp1)))
    (setq thfst nil)
    (mapc #'thip (cdr thck))
    (setq thnf 0)
    (mapc #'thip thfoo)
    (return thttl)))

(defun thremove (thb)
  "Remove assertion or theorem THTT from database.
   Returns the removed item or NIL if not found."
  (prog (thb1 thwh thnf thal thon thbs thfst thfstp thfoo tht1)
    (setq thnf 0)
    (setq thb1
          (cond ((atom thb)
                 (or (setq tht1 (get thb 'theorem))
                     (return nil))
                 (setq thwh (car tht1))
                 (setq thbs thb)
                 (setq thon (list thb))
                 (caddr tht1))
                (t (setq thwh 'thassertion)
                   (setq thbs thb)
                   (setq thon (list thb))
                   thb)))
    (setq thal (length thb1))
    (setq thfst t)
    (setq thfoo nil)
   thp1
    (cond ((null thb1)
           (setq thb1 thfoo)
           (setq thnf 0)
           (setq thfoo (setq thfst nil))
           (setq thfstp t)
           (go thp1))
          ((null (setq tht1 (threm1 (car thb1)))) (return nil))
          ((eq tht1 'thok))
          ((setq thfoo
                 (nconc thfoo
                        (list (cond ((eq tht1 'thvrb) (car thb1))))))
           (setq thb1 (cdr thb1))
           (go thp1)))
    (setq thfst nil)
    (mapc #'threm1 (cdr thb1))
    (setq thnf 0)
    (mapc #'threm1 thfoo)
    (return thbs)))

;;; =========================================================================
;;; ASSERTION & ERASURE: THASS1, THASSERT, THERASE, related handlers
;;; =========================================================================

(defun thass1 (tha p)
  "Common implementation of THASSERT (P=T) and THERASE (P=NIL).
   THA = (pattern recommendation...) as passed by THASSERT/THERASE FEXPR."
  (prog (thx thy1 thy thtype pseudo)
    (and (cdr tha) (eq (caadr tha) 'thpseudo) (setq pseudo t))
    (or (atom (setq thx (car tha)))
        (thpure (setq thx (thvarsubst thx nil)))
        pseudo
        (prog2 (print thx)
               (thert-fn '(impure assertion or erasure - thass1))))
    (and thtrace (not pseudo)
         (thtraces (cond (p 'thassert) (t 'therase)) thx))
    (setq tha (cond (pseudo (cddr tha)) (t (cdr tha))))
    (or (setq thx
              (cond (pseudo (list thx))
                    (p (thadd thx
                              (setq thy
                                    (cond ((and tha (eq (caar tha) 'thprop))
                                           (prog2 0
                                                  (eval (cadar tha))
                                                  (setq tha (cdr tha))))))))
                    (t (thremove thx))))
        (return nil))
    (cond (p (setq thtype 'thante))
          (t (setq thtype 'therasing)))
    (or pseudo
        (thpush thtree
                (list (cond (p 'thassert) (t 'therase))
                      thx thy)))
    (setq thy (mapcan #'thtae tha))
    (cond (thy (setq thexp (cons 'thdo thy))))
    (return thx)))

;;; FEXPR → macro wrappers
(defmacro thassert (&rest tha) `(thass1 ',tha t))
(defmacro therase  (&rest tha) `(thass1 ',tha nil))

;;; THASSERTF/THASSERTT: THFAIL/THSUCCEED handlers for THASSERT frame
(defun thassertf ()
  (thremove (cond ((atom (cadar thtree)) (cadar thtree))
                  (t (caadar thtree))))
  (thpopt) nil)

(defun thassertt () (prog2 nil (cadar thtree) (thpopt)))

;;; THERASEF/THERASET: handlers for THERASE frame
(defun therasef ()
  (thadd (cond ((atom (cadar thtree)) (cadar thtree))
               (t (caadar thtree)))
         (cond ((atom (cadar thtree)) nil)
               (t (cdadar thtree))))
  (thpopt) nil)

(defun theraset () (prog2 nil (cadar thtree) (thpopt)))

;;; =========================================================================
;;; THASVAL — test whether a Planner variable has a value
;;; =========================================================================

(defmacro thasval (x)
  `(let ((cell (sassq ',(cadr x) thalist (lambda () nil))))
     (and cell (not (eq (cadr cell) 'thunassigned)))))

;;; =========================================================================
;;; THEOREM DEFINITION: THDEF, THCONSE, THANTE, THERASING
;;; =========================================================================

(defun thdef (thmtype thx)
  "Define (and optionally assert) an antecedent, consequent, or erasing theorem."
  (prog (thnoassert? thmname thmbody)
    (cond ((not (atom (car thx)))
           (setq thmbody thx)
           (cond ((eq thmtype 'thconse)  (setq thmname (thgename-fn 'tc-g)))
                 ((eq thmtype 'thante)   (setq thmname (thgename-fn 'ta-g)))
                 ((eq thmtype 'therasing)(setq thmname (thgename-fn 'te-g)))))
          (t (setq thmname (car thx)) (setq thmbody (cdr thx))))
    (cond ((eq (car thmbody) 'thnoassert)
           (setq thnoassert? t)
           (setq thmbody (cdr thmbody))))
    (thputprop thmname (cons thmtype thmbody) 'theorem)
    (cond (thnoassert?
           (print (list thmname 'defined 'but 'not 'asserted)))
          ((thass1 (list thmname) t)
           (print (list thmname 'defined 'and 'asserted)))
          (t (print (list thmname 'redefined))))
    (return t)))

(defmacro thconse  (&rest thx) `(thdef 'thconse  ',thx))
(defmacro thante   (&rest thx) `(thdef 'thante   ',thx))
(defmacro therasing (&rest thx) `(thdef 'therasing ',thx))

;;; =========================================================================
;;; THEOREM APPLICATION: THAPPLY, THAPPLY1
;;; =========================================================================

(defmacro thapply (&rest l)
  `(thapply1 ',(car l) (get ',(car l) 'theorem) ',(cadr l)))

(defun thapply1 (thm thb dat)
  "Try to apply theorem THM (with property THB) to data pattern DAT."
  (cond ((and (thbind (cadr thb)) (thmatch1 dat (caddr thb)))
         (and thtrace (thtraces 'theorem thm))
         (thpush thtree (list 'thprog (cddr thb) nil (cddr thb)))
         (thproga)
         t)
        (t (setq thalist tholist) (thpopt) nil)))

;;; =========================================================================
;;; THTRY — build candidate list for THGOAL
;;; =========================================================================

(defun thtry (x)
  "Expand one recommendation X into a list of concrete match candidates."
  (cond ((atom x) nil)
        ((eq (car x) 'thtbf)
         (cond ((not thz1) (setq thz1 t) (setq thz (thmatchlist tha2 'thconse))))
         (cond (thz (list (list 'thtbf (cadr x) thz))) (t nil)))
        ((eq (car x) 'thdbf)
         (cond ((not thy1) (setq thy1 t) (setq thy (thmatchlist tha2 'thassertion))))
         (cond (thy (list (list 'thdbf (cadr x) thy))) (t nil)))
        ((eq (car x) 'thuse)
         (list (list 'thtbf 'thtrue (cdr x))))
        ((eq (car x) 'thnum) (list x))
        (t (print x)
           (thtry (thert-fn (list 'unclear 'recommendation '- 'thtry))))))

;;; =========================================================================
;;; THTRY1 — try next candidate for a THGOAL frame
;;; =========================================================================

(defun thtry1 ()
  "Pop the next candidate from the current THGOAL frame and try it.
   Returns T (with bindings) if a candidate matches; NIL if all exhausted."
  (prog (thx thy thz thw theorem)
    (setq thz (car thtree))         ; (THGOAL pattern candidates . counter)
    (setq thy (cddr thz))           ; (candidates . counter)
    (rplacd thy (sub1 (cdr thy)))
   nxtrec
    (cond ((or (null (car thy)) (zerop (cdr thy)))
           (return nil)))
    (setq thx (caar thy))
    ;; MacLisp computed GO — dispatch on tag stored as (car thx)
    (let ((tag (car thx)))
      (cond ((eq tag 'thnum) (go thnum))
            ((eq tag 'thdbf) (go thdbf))
            ((eq tag 'thtbf) (go thtbf))
            (t (error "Unknown THTRY1 tag: ~S" tag))))
   thnum
    (rplacd thy (cadr thx))
    (rplaca thy (cdar thy))
    (go nxtrec)
   thdbf
    (setq tholist thalist)
    (cond ((null (caddr thx))
           (rplaca thy (cdar thy))
           (go nxtrec))
          ((prog2 nil
                  (and (funcall (cadr thx) (setq thw (caaddr thx)))
                       (thmatch1 (cadr thz) (car thw)))
                  (rplaca (cddr thx) (cdaddr thx)))
           (return thw))
          (t (go thdbf)))
   thtbf
    (cond ((null (caddr thx))
           (rplaca thy (cdar thy))
           (go nxtrec)))
    (setq theorem (caaddr thx))
   thtbf1
    (cond ((not (and (setq thw (get theorem 'theorem))
                     (eq (car thw) 'thconse)))
           (print theorem)
           (cond ((eq (setq theorem
                            (thert-fn (list 'bad 'theorem '- 'thtry1)))
                      't)
                  (go nxtrec))
                 (t (go thtbf1)))))
    (cond ((prog2 nil
                  (and (funcall (cadr thx) (caaddr thx))
                       (thapply1 theorem thw (cadr thz)))
                  (rplaca (cddr thx) (cdaddr thx)))
           (return t))
          (t (go thtbf)))))

;;; =========================================================================
;;; THGOAL — attempt to prove a goal pattern
;;; =========================================================================

(defun thgoal-fn (tha)
  "Implementation of THGOAL FEXPR.
   THA = (pattern recommendation...).  Returns NIL (triggers THGOALF)."
  (prog (thy thy1 thz thz1 tha1 tha2)
    (setq tha2 (thvarsubst (car tha) t))
    (setq tha1 (cdr tha))
    ;; if no recommendations or none suitable, add default THDBF
    (cond ((or (null tha1)
               (and (not (and (eq (caar tha1) 'thanum)
                              (setq tha1
                                    (cons (list 'thnum (cadar tha1))
                                          (cons (list 'thdbf 'thtrue)
                                                (cdr tha1))))))
                    (not (and (eq (caar tha1) 'thnodb)
                              (prog2 (setq tha1 (cdr tha1)) t)))
                    (not (eq (caar tha1) 'thdbf))))
           (setq tha1 (cons (list 'thdbf 'thtrue) tha1))))
    (setq tha1 (mapcan #'thtry tha1))
    (and thtrace (thtraces 'thgoal tha2))
    (cond ((null tha1) (return nil)))
    (thpush thtree (list 'thgoal tha2 tha1))
    ;; store the search limit as CDR of the candidates cell
    (rplacd (cddar thtree) 262143)
    (return nil)))

(defmacro thgoal (&rest tha) `(thgoal-fn ',tha))

(defun thgoalf ()
  "THFAIL handler for THGOAL: try next candidate."
  (cond (thmessage (thpopt) nil)
        ((thtry1))
        (t (thpopt) nil)))

(defun thgoalt ()
  "THSUCCEED handler for THGOAL: return matched value and pop."
  (prog2 nil
         (cond ((eq thvalue 'thnoval) (thvarsubst (cadar thtree) nil))
               (t thvalue))
         (thpopt)))

;;; =========================================================================
;;; THFIND — collect multiple solutions
;;; =========================================================================

(defun thfind-fn (tha)
  "THFIND (vars) goal... — find solutions, return as list or first."
  (prog (thfvars thfbody thfn thfr thfp)
    (setq thfvars (car tha))           ; variables to collect
    (setq thfbody (cdr tha))           ; goals
    ;; stub: basic THFIND — single solution, like THGOAL
    ;; Full implementation needs THFINALIZE
    (let ((result (thval (cons 'thprog tha) thalist)))
      (return result))))

(defmacro thfind  (&rest tha) `(thfind-fn  ',tha))

(defun thfindf () (thpopt) nil)
(defun thfindt () (prog2 nil (cadar thtree) (thpopt)))

;;; =========================================================================
;;; THPROG — sequential goal execution with backtracking
;;; =========================================================================

(defun thprog-fn (tha)
  "THPROG (vars) body... — bind vars, execute body in sequence."
  (thbind (car tha))
  (thpush thtree (list 'thprog tha nil tha))
  (thproga))

(defmacro thprog (&rest tha) `(thprog-fn ',tha))

(defun thproga ()
  "Advance to the next body expression in the current THPROG frame."
  (let ((x (cdar thtree)))
    (cond ((null (cdar x)) (thpopt) 'thnoval)
          ((atom (cadar x))
           ;; it's a label — set THTAG marker
           (setq thexp (list 'thtag (cadar x)))
           (rplaca x (cdar x))
           thvalue)
          (t (setq thexp (cadar x))
             (rplaca x (cdar x))
             thvalue))))

(defun thprogf ()
  "THFAIL handler for THPROG: undo the branch and fail."
  (thbranchun) nil)

(defun thprogt ()
  "THSUCCEED handler for THPROG: record branch and advance to next step."
  (thbranch) (thproga))

;;; =========================================================================
;;; THBRANCH, THBRANCHUN — manage success/failure branch state
;;; =========================================================================

(defun thbranch ()
  "On success: record the branch point on the THPROG frame for backtracking."
  (cond ((not (cdadar thtree)))
        ((eq thbranch thtree) (setq thbranch nil))
        (t (rplaca (cddar thtree)
                   (cons (list thbranch thabranch (cadar thtree))
                         (caddar thtree)))
           (setq thbranch nil)
           (setq thabranch nil)
           (rplaca (cadar thtree) (cdadar thtree)))))

(defun thbranchun ()
  "On failure: restore the last branch point from the THPROG frame."
  (cond ((null (caddar thtree))
         (thpopt))
        (t (let ((saved (caaddr (car thtree))))
             (setq thtree (car saved))
             (setq thalist (cadr saved))
             (rplaca (cddar thtree) (cdaddr (car thtree)))))))

;;; =========================================================================
;;; THAND — conjunction
;;; =========================================================================

(defun thand-fn (a)
  (or (not a)
      (prog2 (thpush thtree (list 'thand a nil))
             (setq thexp (car a)))))

(defmacro thand (&rest a) `(thand-fn ',a))

(defun thandf () (thbranchun) nil)

(defun thandt ()
  (cond ((cdadar thtree)
         (thbranch)
         (setq thexp (cadr (cadar thtree)))
         (rplaca (cdar thtree) (cdadar thtree)))
        (t (thpopt)))
  thvalue)

;;; =========================================================================
;;; THOR — disjunction
;;; =========================================================================

(defun thor-fn (tha)
  (and tha
       (thpush thtree (list 'thor tha nil))
       (setq thexp (car tha))))

(defmacro thor (&rest tha) `(thor-fn ',tha))

(defun thor2 (p)
  (cond (thmessage (thpopt) nil)
        ((cdadar thtree)
         (setq thexp (caadar thtree))
         (rplaca (cdar thtree) (cdadar thtree))
         t)
        (p (thpopt) nil)
        (t (thpopt) nil)))

(defun thorf () (thor2 t))
(defun thort () (thpopt) thvalue)

;;; =========================================================================
;;; THCOND — Planner conditional
;;; =========================================================================

(defun thcond-fn (tha)
  (and tha
       (thpush thtree (list 'thcond tha nil))
       (setq thexp (caar tha))))

(defmacro thcond (&rest tha) `(thcond-fn ',tha))

(defun thcondf () (thor2 nil))
(defun thcondt ()
  (cond ((cdadar thtree)
         (setq thexp (caadahar thtree)) ; first body of next clause
         t)
        (t (thpopt) thvalue)))

;;; =========================================================================
;;; THDO — execute a list of goals (used internally by THASSERT)
;;; =========================================================================

(defun thdo-fn (a)
  (or (not a)
      (prog2 (thpush thtree (list 'thdo a nil nil))
             (setq thexp (car a)))))

(defmacro thdo (&rest a) `(thdo-fn ',a))

(defun thdo1 ()
  (rplaca (cdar thtree) (cdadar thtree))
  (setq thexp (caadar thtree))
  (cond (thbranch
         (rplaca (cddar thtree)
                 (cons thbranch (caddar thtree)))
         (setq thbranch nil)
         (rplaca (cdddar thtree)
                 (cons thabranch (car (cdddar thtree)))))))

(defun thdob ()
  (cond ((or thmessage (null (cdadar thtree)))
         (rplaca (car thtree) 'thundo)
         t)
        (t (thdo1))))

;;; =========================================================================
;;; THUNDO — undo a THDO sequence
;;; =========================================================================

(defun thundof ()
  (cond ((null (caddar thtree)) (thpopt))
        (t (setq thxx (cddar thtree))
           (setq thalist (caadr thxx))
           (rplaca (cdr thxx) (cdadr thxx))
           (setq thtree (caar thxx))
           (rplaca thxx (cdar thxx))))
  nil)

(defun thundot () (thpopt) t)

;;; =========================================================================
;;; THAMONG — membership test with backtracking
;;; =========================================================================

(defun thamong-fn (tha)
  (cond ((eq (cadr (setq thxx (thgal (cond ((eq (caar tha) 'thev)
                                            (thval (cadar tha) thalist))
                                           (t (car tha)))
                                     thalist)))
             'thunassigned)
         (thpush thtree (list 'thamong thxx (thval (cadr tha) thalist)))
         nil)
        (t (member (cadr thxx) (thval (cadr tha) thalist)))))

(defmacro thamong (&rest tha) `(thamong-fn ',tha))

(defun thamongf ()
  (cond (thmessage (thpopt) nil)
        ((caddar thtree)
         (rplaca (cdadar thtree) (caaddr (car thtree)))
         (rplaca (cddar thtree) (cdaddr (car thtree)))
         (setq thbranch thtree)
         (setq thabranch thalist)
         (thpopt)
         t)
        (t (rplaca (cdadar thtree) 'thunassigned)
           (thpopt)
           nil)))

;;; =========================================================================
;;; THSETQ, THVSETQ — Planner variable assignment
;;; =========================================================================

(defun thsetq-fn (thl1)
  (prog (thml thl)
    (setq thl thl1)
   loop
    (cond ((null thl)
           (thpush thtree (list 'thmung thml))
           (return thvalue))
          ((null (cdr thl))
           (print thl1)
           (thert-fn '(odd number of goodies - thsetq)))
          ((atom (car thl))
           (thpush thml (list 'setq (car thl)
                               (list 'quote (eval (car thl)))))
           (set (car thl) (setq thvalue (eval (cadr thl)))))
          (t (thrplacas (cdr (thsgal (car thl)))
                        (setq thvalue (thval (cadr thl) thalist)))))
    (setq thl (cddr thl))
    (go loop)))

(defmacro thsetq  (&rest thl) `(thsetq-fn  ',thl))

(defun thvsetq-fn (tha)
  (prog (a)
    (setq a tha)
   loop
    (cond ((null a) (return thvalue))
          ((null (cdr a))
           (print tha)
           (thert-fn '(odd number of goodies - thvsetq)))
          (t (setq thvalue
                   (car (rplaca (cdr (thsgal (car a)))
                                (thval (cadr a) thalist))))))
    (setq a (cddr a))
    (go loop)))

(defmacro thvsetq (&rest tha) `(thvsetq-fn ',tha))

;;; =========================================================================
;;; THMUNG — undo a list of assignments (THFAIL/THSUCCEED handlers)
;;; =========================================================================

(defun thmungf () (evlis (cadar thtree)) (thpopt) nil)
(defun thmungt () (thpopt) thvalue)

;;; =========================================================================
;;; THREMBIND — restore outer THALIST (THFAIL/THSUCCEED handlers)
;;; =========================================================================

(defun thrembindf () (setq thalist (cadar thtree)) (thpopt) nil)
(defun thrembindt () (setq thalist (cadar thtree)) (thpopt) thvalue)

;;; =========================================================================
;;; THSUCCEED, THFAIL, THRETURN, THGO — control flow
;;; =========================================================================

(defun thsucceed-fn (tha)
  (or (not tha)
      (prog (thx)
        (and (eq (car tha) 'theorem)
             (setq tha (cons 'thprog (cdr tha))))
        (setq thbranch thtree)
        (setq thabranch thalist)
       loop
        (cond ((null thtree)
               (print tha)
               (thert-fn '(overpop - thsucceed)))
              ((eq (caar thtree) 'thrembind)
               (setq thalist (cadar thtree))
               (thpopt)
               (go loop))
              ((eq (caar thtree) (car tha))
               (thpopt)
               (return (cond ((cdr tha) (eval (cadr tha)))
                             (t 'thnoval))))
              ((and (eq (car tha) 'thtag)
                    (eq (caar thtree) 'thprog)
                    (setq thx (memq (cadr tha) (cadddr (car thtree)))))
               (rplaca (cdar thtree) (cons nil thx))
               (return (thprogt)))
              (t (thpopt) (go loop))))))

(defmacro thsucceed (&rest tha) `(thsucceed-fn ',tha))
(defmacro threturn  (&rest x)   `(thsucceed-fn '(thprog ,@x)))
(defmacro thgo      (&rest x)   `(thsucceed-fn '(thtag ,@x)))

(defun thfail-fn (tha)
  (and tha
       (prog (thtree1 tha1 thx)
        f   (setq tha1
                  (cond ((eq (car tha) 'theorem) 'thprog)
                        ((eq (car tha) 'thtag) 'thprog)
                        (t (car tha))))
            (cond ((null thtree) (return nil))
                  ((and (eq (caar thtree) tha1)
                        (or (not (eq tha1 'thprog))
                            (not (eq (car tha) 'thtag))
                            (memq (cadr tha) (cadddr (car thtree)))))
                   (thpopt)
                   (return nil))
                  (t (thpopt) (go f))))))

(defmacro thfail (&rest tha) `(thfail-fn ',tha))

;;; =========================================================================
;;; THTAG — prog label marker
;;; =========================================================================

(defun thtag-fn (l)
  (and (car l) (thpush thtree (list 'thtag (car l)))))

(defmacro thtag (&rest l) `(thtag-fn ',l))

(defun thtagf () (thpopt) nil)
(defun thtagt () (thpopt) thvalue)

;;; =========================================================================
;;; THMESSAGE — send a message on failure
;;; =========================================================================

(defun thmessage-fn (tha)
  (thpush thtree (cons 'thmessage tha))
  (setq thexp (car tha)))

(defmacro thmessage (&rest tha) `(thmessage-fn ',tha))

(defun thmessagef ()
  (prog (bod)
    (setq bod (cdar thtree))
    (thpopt)
    (return (thval (cadr bod) thalist))))

(defun thmessaget () (thpopt) thvalue)

;;; =========================================================================
;;; THBKPT — breakpoint
;;; =========================================================================

(defmacro thbkpt (&rest l)
  `(or (and thtrace (thtraces 'thbkpt ',l)) thvalue))

;;; =========================================================================
;;; THNOT — Planner negation
;;; =========================================================================

(defmacro thnot (&rest tha)
  `(setq thexp (list 'thfail? ',tha)))

(defun thfail?-fn (tha)
  (thpush thtree (list 'thfail? (car tha)))
  (setq thexp (car tha)))

(defun thfail?f ()
  (cond ((eval (cadar thtree)) (thpopt) nil)
        (t (thpopt) t)))

(defun thfail?t () (thpopt) thvalue)

;;; =========================================================================
;;; THV, THNV, THEV — variable accessors
;;; =========================================================================

(defun thv1 (x)
  "(THV1 name) — return current value of Planner variable NAME."
  (setq thxx x)
  (let ((cell (sassq x thalist
                     (lambda ()
                       (print thxx)
                       (thert-fn (list 'thunbound '- 'thv1))))))
    (cond ((eq (cadr cell) 'thunassigned)
           (print thxx)
           (thert-fn (list 'thunassigned '- 'thv1)))
          (t (cadr cell)))))

(defmacro thv  (x) `(thv1 ',x))
(defmacro thnv (x) `(thv1-new ',x))   ; new variable — just return unassigned marker

(defun thv1-new (x)
  "(THNV X) — introduce new variable X (currently just looks it up or errors)."
  (thv1 x))

;;; =========================================================================
;;; THPURE — check that a pattern has no unassigned variables
;;; =========================================================================

(defun thpure (xx)
  "Return non-NIL iff XX contains no variable forms; NIL if any found."
  (errset (mapc (lambda (y) (and (thvar y) (err nil))) xx)))

;;; =========================================================================
;;; THGENAME — generate a unique name with a given prefix
;;; =========================================================================

(defun thgename-fn (prefix)
  (intern (format nil "~A~A" prefix (setq thgename (add1 thgename)))
          *package*))

(defmacro thgename (&rest x) `(thgename-fn ',(car x)))

;;; =========================================================================
;;; THUNIQUE — assert a unique fact (no duplicates)
;;; =========================================================================

(defun thunique-fn (tha)
  (setq tha (cons 'thunique (mapcar #'eval tha)))
  (prog (x)
    (setq x thalist)
   lp
    (cond ((null x) (thpush thalist tha) (return t))
          ((and (eq (caar x) 'thunique)
                (equal (car x) tha))
           (return nil)))
    (setq x (cdr x))
    (go lp)))

(defmacro thunique (&rest tha) `(thunique-fn ',tha))

;;; =========================================================================
;;; THTAE — process recommendation list for THASSERT/THERASE
;;; =========================================================================

(defun thtae (xx)
  "Build THAPPLY calls from recommendation list entry XX."
  (cond ((atom xx) nil)
        ((eq (car xx) 'thuse)
         (mapcar (lambda (x)
                   (cond ((not (and (setq thxx (get x 'theorem))
                                    (eq (car thxx) thtype)))
                          (print x)
                          (list 'thapply
                                (thert-fn (list 'bad 'theorem '- 'thtae))
                                (car thx)))
                         (t (list 'thapply x (car thx)))))
                 (cdr xx)))
        ((eq (car xx) 'thtbf)
         (mapcan (lambda (y)
                   (cond ((funcall (cadr xx) y)
                          (list (list 'thapply y (car thx))))))
                 (cond (thy1 thy)
                       (t (setq thy1 t)
                          (setq thy (thmatchlist (car thx) thtype))))))
        (t (print xx)
           (thtae (thert-fn '(unclear recommendation - thtae))))))

;;; =========================================================================
;;; THFINALIZE — used by THFIND for result collection
;;; =========================================================================

(defun thfinalize-fn (tha)
  ;; Stub: THFINALIZE is the success action for THFIND
  ;; Records found values into the result list
  (declare (ignore tha))
  thvalue)

(defmacro thfinalize (&rest tha) `(thfinalize-fn ',tha))

;;; =========================================================================
;;; THFLUSH — remove all assertions and theorems
;;; =========================================================================

(defun thflush ()
  "Remove all Planner data from all interned symbols."
  (dolist (bucket (makoblist nil))
    (dolist (atom bucket)
      (remprop atom 'thassertion)
      (remprop atom 'thante)
      (remprop atom 'thconse)
      (remprop atom 'therasing)
      (remprop atom 'theorem)))
  nil)

;;; =========================================================================
;;; THDATA, THSTATE — print the current database state
;;; =========================================================================

(defun thdata ()
  "Print all assertions in the database (stub)."
  (format t "~&(THDATA)~%")
  (dolist (bucket (makoblist nil))
    (dolist (atom bucket)
      (let ((asrt (get atom 'thassertion)))
        (when asrt
          (dolist (pos-bucket (cdr asrt))
            (dolist (len-bucket (cdr pos-bucket))
              (dolist (item (cddr len-bucket))
                (print item)))))))))

(defmacro thstate (&rest indicators)
  `(thstate-fn ',indicators))

(defun thstate-fn (indicators)
  (print '(thdata))
  (dolist (bucket (makoblist nil))
    (dolist (atom bucket)
      (dolist (thwh (or indicators '(thassertion thante thconse therasing)))
        (let ((prop (get atom thwh)))
          (and prop
               (let ((sub (assoc 1 (cdr prop))))
                 (and sub
                      (dolist (len-bucket (cdr sub))
                        (dolist (asrt (cddr len-bucket))
                          (cond ((eq thwh 'thassertion) (print asrt))
                                (t (print (list asrt))))))))))))))

;;; =========================================================================
;;; THERT — error/break handler
;;; =========================================================================
;;;
;;; In the original, THERT enters an interactive read-eval loop.
;;; Here we signal a Lisp error; the message is printed first.

(defun thert-fn (message)
  "Print MESSAGE and signal a Planner error."
  (format *error-output* "~%>>> ~{~A ~}~%" (if (listp message) message (list message)))
  (error 'planner-fail))

(defmacro thert (&rest message)
  `(thert-fn ',message))

;;; =========================================================================
;;; THVAL — the Micro-Planner interpreter loop
;;; =========================================================================
;;;
;;; THVAL is the heart of Micro-Planner.  It runs a trampoline:
;;;   1. Eval the current expression (THEXP / THE).
;;;   2. On NIL result → call the THFAIL handler of the top THTREE frame.
;;;   3. On non-NIL result → call the THSUCCEED handler.
;;;   4. Repeat until THTREE empties.
;;;
;;; The THTREE is a stack of frames; each frame's CAR is a symbol that
;;; has THFAIL and THSUCCEED properties naming zero-arg handler functions.
;;;
;;; Translation notes vs. MacLisp original:
;;;   - PROG local vars that are globally special → dynamically bound by PROG.
;;;   - ((PROG2 0. THEXP (SETQ THEXP NIL))) → (FUNCALL (PROG2 NIL THEXP ...)).
;;;   - ERRSET → HANDLER-CASE.
;;;   - (EVAL THE) works unchanged: macros expand correctly under EVAL.

(defun thval (thexp thalist)
  "Evaluate Planner expression THEXP in variable context THALIST.
   Returns the final THVALUE on success, NIL on failure."
  ;; Save call-stack state
  (push (list thtree thalist) thlevel)
  (prog (thtree thvalue thbranch tholist thabranch the thmessage)
    (setq thv '(thv thnv))
    (setq thvalue 'thnoval)

   go
    (setq the thexp)
    (setq thexp nil)
    ;; ctrl-A interrupt check (ITS-specific; ^A is a defvar)
    (when ^a
      (setq ^a nil)
      (unless (thert-fn '(^a - thval)) (go fail)))
    ;; optional stepping hooks
    (when thstep (eval thstep))
    ;; evaluate the current expression, catching Lisp errors
    (handler-case (setq thvalue (eval the))
      (error (c)
        (print the)
        (format *error-output* "~&Lisp error: ~A~%" c)
        (setq thvalue nil)))

   go1
    (when thstepd (eval thstepd))
    (cond (thmessage (go mfail))
          (thexp     (go go))
          (thvalue   (go succeed))
          (t         (go fail)))

   succeed
    (when thstept (eval thstept))
    ;; record branch point if not already set
    (cond ((null thbranch)
           (setq thbranch thtree)
           (setq thabranch thalist)))
    ;; if stack empty we're done
    (cond ((null thtree)
           (setq thlevel (cdr thlevel))
           (return thvalue))
          ;; call the THSUCCEED handler of the top frame
          ((setq thexp (get (caar thtree) 'thsucceed))
           (go go2))
          (t (thert-fn '(bad succeed - thval)) (go succeed)))

   mfail
    (cond ((eq (car thmessage) thtree)
           (setq thexp (cadr thmessage))
           (setq thmessage nil)
           (go go))
          (t (go fail)))

   fail
    (when thstepf (eval thstepf))
    (cond ((null thtree)
           (setq thlevel (cdr thlevel))
           (return nil))
          ;; call the THFAIL handler of the top frame
          ((setq thexp (get (caar thtree) 'thfail))
           (go go2))
          (t (thert-fn '(bad fail - thval)) (go succeed)))

   go2
    ;; Dispatch: THEXP holds the name of a zero-arg handler function.
    ;; Retrieve it, clear THEXP, call the function.
    (setq thvalue (funcall (prog2 nil thexp (setq thexp nil))))
    (go go1)))

;;; =========================================================================
;;; THINIT — initialise / reset the Planner system
;;; =========================================================================

(defun thinit-fn (&optional flush-p)
  "Reset Planner state.  If FLUSH-P, also clear the database."
  (when flush-p (thflush))
  (setq thgename 0)
  (setq thstep nil)
  (setq thstepd nil)
  (setq thstept nil)
  (setq thstepf nil)
  (setq thxx nil)
  (setq thtrace nil)
  (setq thalist '((nil nil)))
  (setq thtree nil)
  (setq thlevel nil)
  (setq thbranch nil)
  (setq thabranch nil)
  (setq thvalue 'thnoval)
  (setq thexp nil)
  (setq ^a nil)
  (set-macro-character #\$ #'thread)
  (format t "~&Micro-Planner ~A ready.~%" thversion)
  t)

(defmacro thinit (&rest l) `(thinit-fn ',(car l)))

;;; =========================================================================
;;; THRESTRICT — define a restricted variable
;;; =========================================================================

(defun threstrict-fn (tha)
  "THRESTRICT — push a restricted variable binding."
  (thpush thalist (list 'threstrict (car tha) (thval (cadr tha) thalist))))

(defmacro threstrict (&rest tha) `(threstrict-fn ',tha))

;;; =========================================================================
;;; THREMPROP — remove a property, backtrackably
;;; =========================================================================

(defun thremprop (atom indicator)
  (thpush thtree
          (list 'thmung
                (list (list 'putprop (list 'quote atom)
                            (list 'quote (get atom indicator))
                            (list 'quote indicator)))))
  (remprop atom indicator))

;;; =========================================================================
;;; THTRACES — tracing support
;;; =========================================================================

(defun thnofail (thx)
  "Temporary disable THPROG failure (for THFIND-like operations)."
  (cond (thx (setf (get 'thprog 'thfail) 'thprogt))
        (t   (setf (get 'thprog 'thfail) 'thprogf))))

;;; =========================================================================
;;; DEFPROP dispatch table — THFAIL and THSUCCEED handlers
;;; =========================================================================
;;;
;;; Each frame type on THTREE has:
;;;   (get 'TYPE 'THFAIL)    → name of zero-arg failure handler function
;;;   (get 'TYPE 'THSUCCEED) → name of zero-arg success handler function

(eval-when (:load-toplevel :execute)
  (setf (get 'thtag    'thfail)    'thtagf)
  (setf (get 'thtag    'thsucceed) 'thtagt)
  (setf (get 'thgoal   'thsucceed) 'thgoalt)
  (setf (get 'thgoal   'thfail)    'thgoalf)
  (setf (get 'thfail?  'thfail)    'thfail?f)
  (setf (get 'thfail?  'thsucceed) 'thfail?t)
  (setf (get 'thamong  'thfail)    'thamongf)
  (setf (get 'thfind   'thfail)    'thfindf)
  (setf (get 'thfind   'thsucceed) 'thfindt)
  (setf (get 'thprog   'thsucceed) 'thprogt)
  (setf (get 'thand    'thsucceed) 'thandt)
  (setf (get 'thmung   'thsucceed) 'thmungt)
  (setf (get 'therase  'thsucceed) 'theraset)
  (setf (get 'thassert 'thsucceed) 'thassertt)
  (setf (get 'thor     'thsucceed) 'thort)
  (setf (get 'thcond   'thsucceed) 'thcondt)
  (setf (get 'thand    'thfail)    'thandf)
  (setf (get 'thprog   'thfail)    'thprogf)
  (setf (get 'thmung   'thfail)    'thmungf)
  (setf (get 'thassert 'thfail)    'thassertf)
  (setf (get 'therase  'thfail)    'therasef)
  (setf (get 'thcond   'thfail)    'thcondf)
  (setf (get 'thor     'thfail)    'thorf)
  (setf (get 'thdo     'thsucceed) 'thdob)
  (setf (get 'thdo     'thfail)    'thdob)
  (setf (get 'thundo   'thfail)    'thundof)
  (setf (get 'thundo   'thsucceed) 'thundot)
  (setf (get 'thmessage 'thfail)   'thmessagef)
  (setf (get 'thmessage 'thsucceed)'thmessaget)
  (setf (get 'thrembind 'thsucceed)'thrembindt)
  (setf (get 'thrembind 'thfail)   'thrembindf))
