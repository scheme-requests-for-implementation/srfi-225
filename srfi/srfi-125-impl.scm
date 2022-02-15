(define hash-table-dto
  (let ()

   (define-syntax guard-immutable
     (syntax-rules ()
       ((_ table body ... final-expr)
        (if (t125-hash-table-mutable? table)
            (let ()
             body ...
             final-expr)
            (let ((table (t125-hash-table-copy table #t)))
             body ...
             (let ((table (t125-hash-table-copy table #f)))
              final-expr))))))

   (define (t125-hash-table-pure?* dto table)
     (not (t125-hash-table-mutable? table)))

    (define (t125-hash-table-set* dto table . obj)
      (guard-immutable table
        (apply t125-hash-table-set! (cons table obj))
        table))

    (define (t125-hash-table-update* dto table key updater fail success)
      (guard-immutable table
        (t125-hash-table-update! table key updater fail success)
        table))

    (define (t125-hash-table-update/default* dto table key proc default)
      (guard-immutable table
        (t125-hash-table-update!/default table key proc default)
        table))

    (define (t125-hash-table-intern* dto table key failure)
      (guard-immutable table
        (define val (t125-hash-table-intern! table key failure))
        (values table val)))

    (define (t125-hash-table-pop* dto table)
      (if (t125-hash-table-empty? table)
          (error "popped empty dictionary")
          (guard-immutable table
            (define-values
              (key value)
              (t125-hash-table-pop! table))
            (values table key value))))

    (define (t125-hash-table-delete-all* dto table keys)
      (guard-immutable table
        (for-each
          (lambda (key)
            (t125-hash-table-delete! table key))
          keys)
        table))

    (define (t125-hash-table-map* dto proc table)
      (guard-immutable table
        (t125-hash-table-map! proc table)
        table))

    (define (t125-hash-table-filter* dto proc table)
      (guard-immutable table
        (t125-hash-table-prune!
          (lambda (key value)
            (not (proc key value)))
          table)
        table))

    (define (t125-hash-table-remove* dto proc table)
      (guard-immutable table
        (t125-hash-table-prune! proc table)
        table))

    (define (t125-hash-table-find-update* dto table key fail success)
      ;; instead of running immediately,
      ;; add an indirection through thunk
      ;; to guarantee call in tail position
      (define (make-success-thunk value)
        (define (update new-key new-value)
          (guard-immutable table
            (unless (eq? new-key key)
              (t125-hash-table-delete! table key))
            (t125-hash-table-set! table new-key new-value)
            table))
        (define (remove)
          (guard-immutable table
            (t125-hash-table-delete! table key)
            table))
        (lambda ()
          (success key value update remove) ))
      (define (make-failure-thunk)
        (define (ignore)
          table)
        (define (insert value)
          (guard-immutable table
            (t125-hash-table-set! table key value)
            table))
        (lambda ()
          (fail insert ignore)))

      (define thunk (t125-hash-table-ref table key make-failure-thunk make-success-thunk))
      (thunk))

    (define (t125-hash-table-comparator* dto table)
      (make-comparator (lambda args #t)
                       (t125-hash-table-equivalence-function table)
                       #f
                       (t125-hash-table-hash-function table)))

    (define (t125-hash-table-copy* dto table)
      (t125-hash-table-copy table #t))

    (define (t125-hash-table-size* dto table)
      (t125-hash-table-size table))

    (define (t125-hash-table-for-each* dto proc table)
      (t125-hash-table-for-each proc table))

    (define (t125-hash-table-keys* dto table)
      (t125-hash-table-keys table))

    (define (t125-hash-table-values* dto table)
      (t125-hash-table-values table))

    (define (t125-hash-table-entries* dto table)
      (t125-hash-table-entries table))

    (define (t125-hash-table-fold* dto proc knil table)
      (t125-hash-table-fold proc knil table))

    (define (t125-hash-table-map->list* dto proc table)
      (t125-hash-table-map->list proc table))

    (define (t125-hash-table->alist* dto table)
      (t125-hash-table->alist table))

    (define (t125-hash-table?* dto table)
      (t125-hash-table? table))

    (define (t125-hash-table-empty?* dto table)
      (t125-hash-table-empty? table))

    (define (t125-hash-table-contains?* dto table key)
      (t125-hash-table-contains? table key))

    (define (t125-hash-table-ref* dto table key failure success)
      (t125-hash-table-ref table key failure success))

    (define (t125-hash-table-ref/default* dto table key default)
      (t125-hash-table-ref/default table key default))

    (make-dto
     dictionary?-id t125-hash-table?*
     dict-pure?-id t125-hash-table-pure?*
     dict-empty?-id t125-hash-table-empty?*
     dict-contains?-id t125-hash-table-contains?*
     dict-ref-id t125-hash-table-ref*
     dict-ref/default-id t125-hash-table-ref/default*
     dict-set-id t125-hash-table-set*
     dict-delete-all-id t125-hash-table-delete-all*
     dict-intern-id t125-hash-table-intern*
     dict-update-id t125-hash-table-update*
     dict-update/default-id t125-hash-table-update/default*
     dict-pop-id t125-hash-table-pop*
     dict-map-id t125-hash-table-map*
     dict-filter-id t125-hash-table-filter*
     dict-remove-id t125-hash-table-remove*
     dict-find-update-id t125-hash-table-find-update*
     dict-size-id t125-hash-table-size*
     dict-for-each-id t125-hash-table-for-each*
     dict-keys-id t125-hash-table-keys*
     dict-values-id t125-hash-table-values*
     dict-entries-id t125-hash-table-entries*
     dict-fold-id t125-hash-table-fold*
     dict-map->list-id t125-hash-table-map->list*
     dict->alist-id t125-hash-table->alist*
     dict-comparator-id t125-hash-table-comparator*)))
