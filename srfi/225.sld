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
    dict-pure?

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
    dict-find-update
    dict-find-update!

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
    dict->generator
    dict-set-accumulator
    dict-set!-accumulator
    dict-adjoin-accumulator
    dict-adjoin!-accumulator

    ;; dictionary type descriptors
    dto?
    make-dto
    make-alist-dto
    dto-ref

    ;; exceptions
    dictionary-error
    dictionary-error?
    dictionary-message
    dictionary-irritants

    ;; proc indeces

    ;; required
    dictionary?-id
    dict-find-update-id
    dict-comparator-id
    dict-map-id
    dict-pure?-id
    dict-remove-id
    dict-size-id

    ;; extra
    dict->alist-id
    dict-adjoin-accumulator-id
    dict-adjoin-id
    dict-any-id
    dict-contains?-id
    dict-count-id
    dict-delete-all-id
    dict-delete-id
    dict-empty?-id
    dict-entries-id
    dict-every-id
    dict-filter-id
    dict-fold-id
    dict-for-each-id
    dict-intern-id
    dict-keys-id
    dict-map->list-id
    dict-map-id
    dict-pop-id
    dict-ref-id
    dict-ref/default-id
    dict-remove-id
    dict-replace-id
    dict-set-accumulator-id
    dict-set-id
    dict-update-id
    dict-update/default-id
    dict-values-id
    dict=?-id
    dict->generator-id

    ;; basic DTOs
    alist-eqv-dto
    alist-equal-dto)

    ;; implementations
    (include "indexes.scm")
    (include "externals.scm")
    (include "default-impl.scm")
    (include "alist-impl.scm")

    ;; library-dependent DTO exports
    ;; and implementations
    ;;
    ;;srfi-69-dto
    ;;hash-table-dto
    ;;srfi-126-dto
    ;;mapping-dto
    ;;hash-mapping-dto

    (cond-expand
        ((library (srfi 69))
         (import (prefix (srfi 69) t69-))
         (include "srfi-69-impl.scm")
         (export srfi-69-dto))
        (else))

    (cond-expand
        ((library (srfi 125))
         (import (prefix (srfi 125) t125-))
         (include "srfi-125-impl.scm")
         (export hash-table-dto))
        (else))

    (cond-expand
        ((library (srfi 126))
         (import (prefix (srfi 126) t126-))
         (include "srfi-126-impl.scm")
         (export srfi-126-dto))
        (else))

    (cond-expand
        ((and (library (srfi 146))
              (library (srfi 146 hash)))
         (import (srfi 146)
                 (srfi 146 hash))
         (include "srfi-146-impl.scm"
                  "srfi-146-hash-impl.scm")
         (export mapping-dto
                 hash-mapping-dto))
        (else)))
