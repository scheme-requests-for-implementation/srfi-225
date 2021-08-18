(define srfi-126-dtd
  (let ()

    (define (prep-dtd-arg proc)
      (lambda (dtd . args)
        (apply proc args)))

    (define (t126:make-hashtable* dtd comparator)
      (t126:make-hashtable (comparator-hash-function comparator)
                           (comparator-equality-predicate comparator)))

    (define (t126:hashtable-ref* dtd table key fail success)
      (define-values (value found?) (t126:hashtable-lookup table key))
      (if found?
          (success value)
          (fail)))

    (define (t126:hashtable-ref/default* dtd table key default)
      (t126:hashtable-ref table key default))

    (define (t126:hashtable-set!* dtd table . obj)
      (let loop ((obj obj))
        (if (null? obj)
            table
            (begin
              (t126:hashtable-set! table (car obj) (cadr obj))
              (loop (cddr obj))))))

    (define (t126:hashtable-delete-all!* dtd table keys)
      (for-each
       (lambda (key)
         (t126:hashtable-delete! table key))
       keys)
      table)

    (define (t126:hashtable-intern!* dtd table key default)
      (define val (t126:hashtable-intern! table key default))
      (values table val))

    (define (t126:hashtable-update/default!* dtd table key updater default)
      (t126:hashtable-update! table key updater default)
      table)

    (define (t126:hashtable-pop!* dtd table)
      (if (t126:hashtable-empty? table)
          (error "popped empty dictionary")
          (call-with-values
              (lambda () (t126:hashtable-pop! table))
            (lambda (key value) (values table key value)))))

    (define (t126:hashtable-update-all!* dtd proc table)
      (t126:hashtable-update-all! table proc)
      table)

    (define (t126:hashtable-filter!* dtd proc table)
      (t126:hashtable-prune! table
                             (lambda (key value)
                               (not (proc key value))))
      table)

    (define (t126:hashtable-filter* dtd proc table)
      (dict-filter! dtd proc (dict-copy dtd table)))

    (define (t126:hashtable-remove!* dtd proc table)
      (t126:hashtable-prune! table proc)
      table)

    (define (t126:hashtable-remove* dtd proc table)
      (dict-remove! dtd proc (dict-copy dtd table)))

    (define (t126:hashtable-search!* dtd table key fail success)
      (define (handle-success value)
        (define (update new-key new-value obj)
          (unless (eq? new-key key)
            (t126:hashtable-delete! table key))
          (t126:hashtable-set! table new-key new-value)
          (values table obj))
        (define (remove obj)
          (t126:hashtable-delete! table key)
          (values table obj))
        (success key value update remove))
      (define (handle-fail)
        (define (ignore obj)
          (values table obj))
        (define (insert value obj)
          (t126:hashtable-set! table key value)
          (values table obj))
        (fail insert ignore))

      (define default (cons #f #f))
      (define found (t126:hashtable-ref table key default))
      (if (eq? default found)
          (handle-fail)
          (handle-success found)))

    (define (t126:hashtable-search* dtd table key fail success)
      (dict-search! dtd (dict-copy dtd table) key fail success))

    (define (t126:hashtable-for-each* dtd proc table)
      (t126:hashtable-walk table proc)
      table)

    (define (t126:hashtable-map->lset* dtd proc table)
      (t126:hashtable-map->lset table proc))

    (define (t126:hashtable-keys* dtd table)
      (vector->list (t126:hashtable-keys table)))

    (define (t126:hashtable-values* dtd table)
      (vector->list (t126:hashtable-values table)))

    (define (t126:hashtable-entries* dtd table)
      (call-with-values
          (lambda () (t126:hashtable-entries table))
        (lambda (keys vals)
          (values
           (vector->list keys)
           (vector->list vals)))))

    (define (t126:hashtable-copy* dtd table)
      (t126:hashtable-copy table #t))

    (make-dtd
     make-dictionary-index t126:make-hashtable*
     dictionary?-index (prep-dtd-arg t126:hashtable?)
     dict-empty?-index (prep-dtd-arg t126:hashtable-empty?)
     dict-contains?-index (prep-dtd-arg t126:hashtable-contains?)
     dict-ref-index t126:hashtable-ref*
     dict-ref/default-index t126:hashtable-ref/default*
     dict-set!-index t126:hashtable-set!*
     dict-delete-all!-index t126:hashtable-delete-all!*
     dict-intern!-index t126:hashtable-intern!*
     dict-update/default!-index t126:hashtable-update/default!*
     dict-pop!-index t126:hashtable-pop!*
     dict-map!-index t126:hashtable-update-all!*
     dict-filter!-index t126:hashtable-filter!*
     dict-filter-index t126:hashtable-filter*
     dict-remove!-index t126:hashtable-remove!*
     dict-remove-index t126:hashtable-remove*
     dict-search!-index t126:hashtable-search!*
     dict-search-index t126:hashtable-search*
     dict-size-index (prep-dtd-arg t126:hashtable-size)
     dict-for-each-index t126:hashtable-for-each*
     dict-keys-index t126:hashtable-keys*
     dict-values-index t126:hashtable-values*
     dict-entries-index t126:hashtable-entries*
     dict-map->list-index t126:hashtable-map->lset*
     dict-copy-index t126:hashtable-copy*)))
