(define hash-table-dtd
  (let ()

    (define (t125:make-hash-table* dtd comparator)
      (t125:hash-table comparator))

    (define (t125:hash-table-set!* dtd table . obj)
      (apply t125:hash-table-set! (cons table obj))
      table)

    (define (t125:hash-table-update!* dtd table key updater fail success)
      (t125:hash-table-update! table key updater fail success)
      table)

    (define (t125:hash-table-update!/default* dtd table key proc default)
      (t125:hash-table-update!/default table key proc default)
      table)

    (define (t125:hash-table-intern!* dtd table key failure)
      (define val (t125:hash-table-intern! table key failure))
      (values table val))

    (define (t125:hash-table-pop!* dtd table)
      (if (t125:hash-table-empty? table)
          (error "popped empty dictionary")
          (call-with-values
              (lambda () (t125:hash-table-pop! table))
            (lambda (key value) (values table key value)))))

    (define (t125:hash-table-delete-all!* dtd table keys)
      (for-each
       (lambda (key)
         (t125:hash-table-delete! table key))
       keys)
      table)

    (define (t125:hash-table-map!* dtd proc table)
      (t125:hash-table-map! proc table)
      table)

    (define (t125:hash-table-filter!* dtd proc table)
      (t125:hash-table-prune!
       (lambda (key value)
         (not (proc key value)))
       table)
      table)

    (define (t125:hash-table-filter* dtd proc table)
      (dict-filter! dtd proc (dict-copy dtd table)))

    (define (t125:hash-table-remove!* dtd proc table)
      (t125:hash-table-prune! proc table)
      table)

    (define (t125:hash-table-remove* dtd proc table)
      (dict-remove! dtd proc (dict-copy dtd table)))

    (define (t125:hash-table-search!* dtd table key fail success)
      (define (handle-success value)
        (define (update new-key new-value obj)
          (unless (eq? new-key key)
            (t125:hash-table-delete! table key))
          (t125:hash-table-set! table new-key new-value)
          (values table obj))
        (define (remove obj)
          (t125:hash-table-delete! table key)
          (values table obj))
        (success key value update remove))
      (define (handle-fail)
        (define (ignore obj)
          (values table obj))
        (define (insert value obj)
          (t125:hash-table-set! table key value)
          (values table obj))
        (fail insert ignore))

      (define default (cons #f #f))
      (t125:hash-table-ref table key handle-fail handle-success))

    (define (t125:hash-table-search* dtd table key fail success)
      (t125:hash-table-search!* dtd (dict-copy dtd table) key fail success))

    (define (t125:hash-table-comparator* dtd table)
      (make-comparator (lambda args #t)
                       (t125:hash-table-equivalence-function table)
                       #f
                       (t125:hash-table-hash-function table)))

    (define (t125:hash-table-copy* dtd table)
      (t125:hash-table-copy table))

    (define (t125:hash-table-size* dtd table)
      (t125:hash-table-size table))

    (define (t125:hash-table-for-each* dtd proc table)
      (t125:hash-table-for-each proc table))

    (define (t125:hash-table-keys* dtd table)
      (t125:hash-table-keys table))

    (define (t125:hash-table-values* dtd table)
      (t125:hash-table-values table))

    (define (t125:hash-table-entries* dtd table)
      (t125:hash-table-entries table))

    (define (t125:hash-table-fold* dtd proc knil table)
      (t125:hash-table-fold proc knil table))

    (define (t125:hash-table-map->list* dtd proc table)
      (t125:hash-table-map->list proc table))

    (define (t125:hash-table->alist* dtd table)
      (t125:hash-table->alist table))

    (define (t125:hash-table?* dtd table)
      (t125:hash-table? table))

    (define (t125:hash-table-empty?* dtd table)
      (t125:hash-table-empty? table))

    (define (t125:hash-table-contains?* dtd table key)
      (t125:hash-table-contains? table key))

    (define (t125:hash-table-ref* dtd table key failure success)
      (t125:hash-table-ref table key failure success))

    (define (t125:hash-table-ref/default* dtd table key default)
      (t125:hash-table-ref/default table key default))

    (make-dtd
     make-dictionary-id t125:make-hash-table*
     dictionary?-id t125:hash-table?*
     dict-empty?-id t125:hash-table-empty?*
     dict-contains?-id t125:hash-table-contains?*
     dict-ref-id t125:hash-table-ref*
     dict-ref/default-id t125:hash-table-ref/default*
     dict-set!-id t125:hash-table-set!*
     dict-delete-all!-id t125:hash-table-delete-all!*
     dict-intern!-id t125:hash-table-intern!*
     dict-update!-id t125:hash-table-update!*
     dict-update/default!-id t125:hash-table-update!/default*
     dict-pop!-id t125:hash-table-pop!*
     dict-map!-id t125:hash-table-map!*
     dict-filter!-id t125:hash-table-filter!*
     dict-filter-id t125:hash-table-filter*
     dict-remove!-id t125:hash-table-remove!*
     dict-remove-id t125:hash-table-remove*
     dict-search!-id t125:hash-table-search!*
     dict-search-id t125:hash-table-search*
     dict-size-id t125:hash-table-size*
     dict-for-each-id t125:hash-table-for-each*
     dict-keys-id t125:hash-table-keys*
     dict-values-id t125:hash-table-values*
     dict-entries-id t125:hash-table-entries*
     dict-fold-id t125:hash-table-fold*
     dict-map->list-id t125:hash-table-map->list*
     dict->alist-id t125:hash-table->alist*
     dict-comparator-id t125:hash-table-comparator*
     dict-copy-id t125:hash-table-copy*)))
