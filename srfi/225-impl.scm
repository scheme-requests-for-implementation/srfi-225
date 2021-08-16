(include "indexes.scm")
(include "externals.scm")
(include "default-impl.scm")
(include "alist-impl.scm")
(include "plist-impl.scm")

(cond-expand
  ((library (srfi 69))
   (define srfi-69-dtd
     (let ()
       (include "srfi-69-impl.scm")
       (make-srfi-69-dtd))))
  (else))
