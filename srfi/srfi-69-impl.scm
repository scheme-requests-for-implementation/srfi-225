(define srfi-69-dtd
  (let ()

    (define (t69:make-hash-table* dtd comparator)
      (define constructor-args
        (if (not comparator)
            '()
            (let ((pred (comparator-equality-predicate comparator))
                  (hash (comparator-hash-function comparator)))
              (if hash
                  (list pred hash)
                  (list pred)))))
      (apply t69:make-hash-table constructor-args))

    (define (t69:hash-table-ref* dtd table key fail success)
      (define default (cons #f #f))
      (define found (t69:hash-table-ref/default table key default))
      (if (eq? found default)
          (fail)
          (success found)))

    (define (t69:hash-table-set!* dtd table . obj)
      (let loop ((obj obj))
        (if (null? obj)
            table
            (begin
              (t69:hash-table-set! table (car obj) (cadr obj))
              (loop (cddr obj))))))

    (define (t69:hash-table-update!/default* dtd table key proc default)
      (t69:hash-table-update!/default table key proc default)
      table)

    (define (t69:hash-table-delete-all!* dtd table keys)
      (for-each
       (lambda (key)
         (t69:hash-table-delete! table key))
       keys)
      table)

    (define (t69:hash-table-foreach* dtd proc table)
      (t69:hash-table-walk table proc))

    (define (t69:hash-table-map!* dtd proc table)
      (t69:hash-table-walk table (lambda (key value)
                               (t69:hash-table-set! table key (proc key value))))
      table)

    (define (t69:hash-table-filter!* dtd proc table)
      (t69:hash-table-walk table
                       (lambda (key value)
                         (unless (proc key value)
                           (t69:hash-table-delete! table key))))
      table)

    (define (t69:hash-table-filter* dtd proc table)
      (dict-filter! dtd proc (dict-copy dtd table)))

    (define (t69:hash-table-fold* dtd proc knil table)
      (t69:hash-table-fold table proc knil))

    (define (t69:hash-table-search!* dtd table key fail success)
      (define (handle-success value)
        (define (update new-key new-value obj)
          (unless (eq? new-key key)
            (t69:hash-table-delete! table key))
          (t69:hash-table-set! table new-key new-value)
          (values table obj))
        (define (remove obj)
          (t69:hash-table-delete! table key)
          (values table obj))
        (success key value update remove))
      (define (handle-fail)
        (define (ignore obj)
          (values table obj))
        (define (insert value obj)
          (t69:hash-table-set! table key value)
          (values table obj))
        (fail insert ignore))

      (define default (cons #f #f))
      (define found (t69:hash-table-ref/default table key default))
      (if (eq? default found)
          (handle-fail)
          (handle-success found)))

    (define (t69:hash-table-search* dtd table key fail success)
      (t69:hash-table-search!* dtd (dict-copy dtd table) key fail success))

    (define (t69:hash-table-values* dtd table)
      (t69:hash-table-values table))

    (define (t69:hash-table->alist* dtd table)
      (t69:hash-table->alist table))

    (define (t69:hash-table-keys* dtd table)
      (t69:hash-table-keys table))

    (define (t69:hash-table-size* dtd table)
      (t69:hash-table-size table))

    (define (t69:hash-table-exists?* dtd table key)
      (t69:hash-table-exists? table key))

    (define (t69:hash-table-ref/default* dtd table key default)
      (t69:hash-table-ref/default table key default))

    (define (t69:hash-table?* dtd table)
      (t69:hash-table? table))

    (define (t69:hash-table-comparator* dtd table)
      (make-comparator (lambda args #t)
                       (or (t69:hash-table-equivalence-function table)
                           equal?)
                       #f
                       (t69:hash-table-hash-function table)))

    (define (t69:hash-table-copy* dtd table)
      (t69:hash-table-copy table))

    (make-dtd
     make-dictionary-index t69:make-hash-table*
     dictionary?-index t69:hash-table?*
     dict-ref-index t69:hash-table-ref*
     dict-ref/default-index t69:hash-table-ref/default*
     dict-set!-index t69:hash-table-set!*
     dict-delete-all!-index t69:hash-table-delete-all!*
     dict-contains?-index t69:hash-table-exists?*
     dict-update/default!-index t69:hash-table-update!/default*
     dict-size-index t69:hash-table-size*
     dict-keys-index t69:hash-table-keys*
     dict-values-index t69:hash-table-values*
     dict-map!-index t69:hash-table-map!*
     dict-filter!-index t69:hash-table-filter!*
     dict-filter-index t69:hash-table-filter*
     dict-for-each-index t69:hash-table-foreach*
     dict-fold-index t69:hash-table-fold*
     dict->alist-index t69:hash-table->alist*
     dict-search-index t69:hash-table-search*
     dict-search!-index t69:hash-table-search!*
     dict-comparator-index t69:hash-table-comparator*
     dict-copy-index t69:hash-table-copy*)))
