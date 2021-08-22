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

    ;; constructor
    make-dictionary
    dict-unfold

    ;; predicates
    dictionary?
    dict-empty?
    dict-contains?

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
    dict-search
    dict-search!

    ;; whole dictionary
    dict-copy
    dict-size
    dict-for-each
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

    ;; dictionary type descriptors
    dtd?
    make-dtd
    dtd
    make-modified-dtd
    make-alist-dtd
    dtd-ref

    ;; exceptions
    dictionary-error?
    dictionary-message
    dictionary-irritants

    ;; proc indeces
    make-dictionary-id
    dict-unfold-id
    dictionary?-id
    dict-empty?-id
    dict-contains?-id
    dict-ref-id
    dict-ref/default-id
    dict-set-id
    dict-set!-id
    dict-adjoin-id
    dict-adjoin!-id
    dict-delete-id
    dict-delete!-id
    dict-delete-all-id
    dict-delete-all!-id
    dict-replace-id
    dict-replace!-id
    dict-intern-id
    dict-intern!-id
    dict-update-id
    dict-update!-id
    dict-update/default-id
    dict-update/default!-id
    dict-pop-id
    dict-pop!-id
    dict-map-id
    dict-map!-id
    dict-filter-id
    dict-filter!-id
    dict-remove-id
    dict-remove!-id
    dict-search-id
    dict-search!-id
    dict-copy-id
    dict-size-id
    dict-for-each-id
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

    ;; basic DTDs
    plist-dtd
    alist-eqv-dtd
    alist-equal-dtd)

    ;; implementations
    (include "indexes.scm")
    (include "externals.scm")
    (include "default-impl.scm")
    (include "alist-impl.scm")
    (include "plist-impl.scm")

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
         (import (prefix (srfi 69) t69:))
         (include "srfi-69-impl.scm")
         (export srfi-69-dtd))
        (else))

    (cond-expand
        ((library (srfi 125))
         (import (prefix (srfi 125) t125:))
         (include "srfi-125-impl.scm")
         (export hash-table-dtd))
        (else))

    (cond-expand
        ((library (srfi 126))
         (import (prefix (srfi 126) t126:))
         (include "srfi-126-impl.scm")
         (export srfi-126-dtd))
        (else))

    (cond-expand
        ((library (srfi 146))
         (import (srfi 146)
                 (srfi 146 hash))
         (include "srfi-146-impl.scm"
                  "srfi-146-hash-impl.scm")
         (export mapping-dtd
                 hash-mapping-dtd))))
