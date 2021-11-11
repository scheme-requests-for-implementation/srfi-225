(define-library
  (srfi 225)

  (import (scheme base)
          (scheme case-lambda)
          (scheme write)
          (srfi 1)
          (srfi 128))

  (cond-expand
    ((library (srfi 145)) (import (srfi 145)))
    (else (include "assumptions.scm")))

  (export

    ;; predicates
    dictionary?
    dict-empty?
    dict-contains?
    dict=?
    dict-mutable?

    ;; lookup
    dict-ref
    dict-ref/default

    ;; mutation
    dict-set
    dict-set!
    dict-adjoin
    dict-adjoin!
    dict-delete
    dict-delete!
    dict-delete-all
    dict-delete-all!
    dict-replace
    dict-replace!
    dict-intern
    dict-intern!
    dict-update
    dict-update!
    dict-update/default
    dict-update/default!
    dict-pop
    dict-pop!
    dict-map
    dict-map!
    dict-filter
    dict-filter!
    dict-remove
    dict-remove!
    dict-alter
    dict-alter!

    ;; whole dictionary
    dict-size
    dict-count
    dict-any
    dict-every
    dict-keys
    dict-values
    dict-entries
    dict-fold
    dict-map->list
    dict->alist
    dict-comparator

    ;; iteration
    dict-for-each
    dict-for-each<
    dict-for-each<=
    dict-for-each>
    dict-for-each>=
    dict-for-each-in-open-interval
    dict-for-each-in-closed-interval
    dict-for-each-in-open-closed-interval
    dict-for-each-in-closed-open-interval
    
    ;; generator procedures
    make-dict-generator
    dict-set-accumulator
    dict-adjoin-accumulator

    ;; dictionary type descriptors
    dtd?
    make-dtd
    dtd
    make-alist-dtd
    dtd-ref

    ;; exceptions
    dictionary-error
    dictionary-error?
    dictionary-message
    dictionary-irritants

    ;; proc indeces
    dictionary?-id
    dict-empty?-id
    dict-contains?-id
    dict=?-id
    dict-mutable?-id
    dict-ref-id
    dict-ref/default-id
    dict-set-id
    dict-adjoin-id
    dict-delete-id
    dict-delete-all-id
    dict-replace-id
    dict-intern-id
    dict-update-id
    dict-update/default-id
    dict-pop-id
    dict-map-id
    dict-filter-id
    dict-remove-id
    dict-alter-id
    dict-size-id
    dict-count-id
    dict-any-id
    dict-every-id
    dict-keys-id
    dict-values-id
    dict-entries-id
    dict-fold-id
    dict-map->list-id
    dict->alist-id
    dict-comparator-id
    dict-for-each-id
    dict-for-each<-id
    dict-for-each<=-id
    dict-for-each>-id
    dict-for-each>=-id
    dict-for-each-in-open-interval-id
    dict-for-each-in-closed-interval-id
    dict-for-each-in-open-closed-interval-id
    dict-for-each-in-closed-open-interval-id
    make-dict-generator-id
    dict-set-accumulator-id
    dict-adjoin-accumulator-id

    ;; basic DTDs
    alist-eqv-dtd
    alist-equal-dtd)

    ;; implementations
    (include "indexes.scm")
    (include "externals.scm")
    (include "default-impl.scm")
    (include "alist-impl.scm")

    ;; library-dependent DTD exports
    ;; and implementations
    ;;
    ;;srfi-69-dtd
    ;;hash-table-dtd
    ;;srfi-126-dtd
    ;;mapping-dtd
    ;;hash-mapping-dtd

    (cond-expand
        ((library (srfi 69))
         (import (prefix (srfi 69) t69-))
         (include "srfi-69-impl.scm")
         (export srfi-69-dtd))
        (else))

    (cond-expand
        ((library (srfi 125))
         (import (prefix (srfi 125) t125-))
         (include "srfi-125-impl.scm")
         (export hash-table-dtd))
        (else))

    (cond-expand
        ((library (srfi 126))
         (import (prefix (srfi 126) t126-))
         (include "srfi-126-impl.scm")
         (export srfi-126-dtd))
        (else))

    (cond-expand
        ((and (library (srfi 146))
              (library (srfi 146 hash)))
         (import (srfi 146)
                 (srfi 146 hash))
         (include "srfi-146-impl.scm"
                  "srfi-146-hash-impl.scm")
         (export mapping-dtd
                 hash-mapping-dtd))
        (else)))
