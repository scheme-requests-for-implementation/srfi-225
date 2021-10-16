(define srfi-69-dtd
  (let ()

    (define (prep-dtd-arg proc)
      (lambda (dtd . args)
        (apply proc args)))
    
    (define (t69-hash-table-mutable?* dtd table)
      #t)

    (define (t69-hash-table-ref* dtd table key fail success)
      (define default (cons #f #f))
      (define found (t69-hash-table-ref/default table key default))
      (if (eq? found default)
          (fail)
          (success found)))

    (define (t69-hash-table-set!* dtd table . obj)
      (let loop ((obj obj))
        (if (null? obj)
            table
            (begin
              (t69-hash-table-set! table (car obj) (cadr obj))
              (loop (cddr obj))))))

    (define (t69-hash-table-update!/default* dtd table key proc default)
      (t69-hash-table-update!/default table key proc default)
      table)

    (define (t69-hash-table-delete-all!* dtd table keys)
      (for-each
       (lambda (key)
         (t69-hash-table-delete! table key))
       keys)
      table)

    (define (t69-hash-table-foreach* dtd proc table)
      (t69-hash-table-walk table proc))

    (define (t69-hash-table-map!* dtd proc table)
      (t69-hash-table-walk table (lambda (key value)
                               (t69-hash-table-set! table key (proc key value))))
      table)

    (define (t69-hash-table-filter!* dtd proc table)
      (t69-hash-table-walk table
                       (lambda (key value)
                         (unless (proc key value)
                           (t69-hash-table-delete! table key))))
      table)

    (define (t69-hash-table-fold* dtd proc knil table)
      (t69-hash-table-fold table proc knil))

    (define (t69-hash-table-alter!* dtd table key fail success)
      (define (handle-success value)
        (define (update new-key new-value)
          (unless (eq? new-key key)
            (t69-hash-table-delete! table key))
          (t69-hash-table-set! table new-key new-value)
          table)
        (define (remove)
          (t69-hash-table-delete! table key)
          table)
        (success key value update remove))
      (define (handle-fail)
        (define (ignore)
          table)
        (define (insert value)
          (t69-hash-table-set! table key value)
          table)
        (fail insert ignore))

      (define default (cons #f #f))
      (define found (t69-hash-table-ref/default table key default))
      (if (eq? default found)
          (handle-fail)
          (handle-success found)))

    (define (t69-hash-table-comparator* dtd table)
      (make-comparator (lambda args #t)
                       (or (t69-hash-table-equivalence-function table)
                           equal?)
                       #f
                       (t69-hash-table-hash-function table)))

    (make-dtd
     dictionary?-id (prep-dtd-arg t69-hash-table?)
     dict-mutable?-id t69-hash-table-mutable?*
     dict-ref-id t69-hash-table-ref*
     dict-ref/default-id (prep-dtd-arg t69-hash-table-ref/default)
     dict-set-id t69-hash-table-set!*
     dict-delete-all-id t69-hash-table-delete-all!*
     dict-contains?-id (prep-dtd-arg t69-hash-table-exists?)
     dict-update/default-id t69-hash-table-update!/default*
     dict-size-id (prep-dtd-arg t69-hash-table-size)
     dict-keys-id (prep-dtd-arg t69-hash-table-keys)
     dict-values-id (prep-dtd-arg t69-hash-table-values)
     dict-map-id t69-hash-table-map!*
     dict-filter-id t69-hash-table-filter!*
     dict-for-each-id t69-hash-table-foreach*
     dict-fold-id t69-hash-table-fold*
     dict->alist-id (prep-dtd-arg t69-hash-table->alist)
     dict-alter-id t69-hash-table-alter!*
     dict-comparator-id t69-hash-table-comparator*)))
