(define srfi-126-dtd
  (let ()

   (define-syntax guard-immutable
     (syntax-rules ()
       ((_ table body ... final-expr)
        (if (t126-hashtable-mutable? table)
            (let ()
             body ...
             final-expr)
            (let ((table (t126-hashtable-copy table #t)))
             body ...
             (let ((table (t126-hashtable-copy table #f)))
              final-expr))))))

    (define (prep-dtd-arg proc)
      (lambda (dtd . args)
        (apply proc args)))

    (define (t126-hashtable-ref* dtd table key fail success)
      (define-values (value found?) (t126-hashtable-lookup table key))
      (if found?
          (success value)
          (fail)))

    (define (t126-hashtable-ref/default* dtd table key default)
      (t126-hashtable-ref table key default))

    (define (t126-hashtable-set* dtd table . obj)
      (guard-immutable table
        (let loop ((obj obj))
         (if (null? obj)
             #t
             (begin
               (t126-hashtable-set! table (car obj) (cadr obj))
               (loop (cddr obj)))))
        table))

    (define (t126-hashtable-delete-all* dtd table keys)
      (guard-immutable table
        (for-each
          (lambda (key)
            (t126-hashtable-delete! table key))
          keys)
        table))

    (define (t126-hashtable-intern* dtd table key default)
      (guard-immutable table
        (define val (t126-hashtable-intern! table key default))
        (values table val)))

    (define (t126-hashtable-update/default* dtd table key updater default)
      (guard-immutable table
        (t126-hashtable-update! table key updater default)
        table))

    (define (t126-hashtable-pop* dtd table)
      (if (t126-hashtable-empty? table)
          (error "popped empty dictionary")
          (guard-immutable table
            (define-values
              (key value)
              (t126-hashtable-pop! table))
            (values table key value))))

    (define (t126-hashtable-update-all* dtd proc table)
      (guard-immutable table
        (t126-hashtable-update-all! table proc)
        table))

    (define (t126-hashtable-filter* dtd proc table)
      (guard-immutable table
        (t126-hashtable-prune! table
                               (lambda (key value)
                                 (not (proc key value))))
        table))

    (define (t126-hashtable-remove* dtd proc table)
      (guard-immutable table
        (t126-hashtable-prune! table proc)
        table))

    (define (t126-hashtable-alter* dtd table key fail success)
      (define (handle-success value)
        (define (update new-key new-value)
          (guard-immutable table
            (unless (eq? new-key key)
              (t126-hashtable-delete! table key))
            (t126-hashtable-set! table new-key new-value)
            table))
        (define (remove)
          (guard-immutable table
            (t126-hashtable-delete! table key)
            table))
        (success key value update remove))
      (define (handle-fail)
        (define (ignore)
          table)
        (define (insert value)
          (guard-immutable table
            (t126-hashtable-set! table key value)
            table))
        (fail insert ignore))

      (define default (cons #f #f))
      (define found (t126-hashtable-ref table key default))
      (if (eq? default found)
          (handle-fail)
          (handle-success found)))

    (define (t126-hashtable-for-each* dtd proc table)
      (t126-hashtable-walk table proc)
      table)

    (define (t126-hashtable-map->lset* dtd proc table)
      (t126-hashtable-map->lset table proc))

    (define (t126-hashtable-keys* dtd table)
      (vector->list (t126-hashtable-keys table)))

    (define (t126-hashtable-values* dtd table)
      (vector->list (t126-hashtable-values table)))

    (define (t126-hashtable-entries* dtd table)
      (call-with-values
          (lambda () (t126-hashtable-entries table))
        (lambda (keys vals)
          (values
           (vector->list keys)
           (vector->list vals)))))

    (define (t126-hashtable-comparator* dtd table)
      #f)

    (make-dtd
     dictionary?-id (prep-dtd-arg t126-hashtable?)
     dict-mutable?-id (prep-dtd-arg t126-hashtable-mutable?)
     dict-empty?-id (prep-dtd-arg t126-hashtable-empty?)
     dict-contains?-id (prep-dtd-arg t126-hashtable-contains?)
     dict-ref-id t126-hashtable-ref*
     dict-ref/default-id t126-hashtable-ref/default*
     dict-set-id t126-hashtable-set*
     dict-delete-all-id t126-hashtable-delete-all*
     dict-intern-id t126-hashtable-intern*
     dict-update/default-id t126-hashtable-update/default*
     dict-pop-id t126-hashtable-pop*
     dict-map-id t126-hashtable-update-all*
     dict-filter-id t126-hashtable-filter*
     dict-remove-id t126-hashtable-remove*
     dict-alter-id t126-hashtable-alter*
     dict-size-id (prep-dtd-arg t126-hashtable-size)
     dict-for-each-id t126-hashtable-for-each*
     dict-keys-id t126-hashtable-keys*
     dict-values-id t126-hashtable-values*
     dict-entries-id t126-hashtable-entries*
     dict-map->list-id t126-hashtable-map->lset*
     dict-comparator-id t126-hashtable-comparator*)))
