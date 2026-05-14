(defpackage :microplanner
  (:nicknames :plnr)
  (:use :common-lisp)
  (:export
   ;; User-facing Planner operators (FEXPRs in original → macros here)
   #:thassert #:therase #:thgoal #:thfind #:thprog
   #:thand #:thor #:thcond #:thconse #:thante #:therasing
   #:thamong #:thsetq #:thvsetq #:thsucceed #:thfail #:threturn
   #:thbkpt #:thmessage #:thnot #:thdo #:thgo #:thflush
   #:thstate #:thinit #:thdump #:thunique #:thgename
   #:thasval #:thtag #:threstrict #:thapply
   ;; Variable markers (appear in patterns as data, also callable)
   #:thv #:thnv #:thev
   ;; Database query
   #:thdata
   ;; Key globals
   #:thtree #:thalist #:thvalue #:thtrace #:thversion
   ;; Utilities accessible from user code
   #:thvar #:thval #:thgal #:thmatch1 #:thmatch
   #:thgename #:thpure #:thdef
   ;; Sentinel values
   #:thnoval #:thunassigned #:thvrb #:thtrue))
