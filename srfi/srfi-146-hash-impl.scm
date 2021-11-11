(define hash-mapping-dtd
  (let ()

    (define (prep-dtd-arg proc)
      (lambda (dtd . args)
        (apply proc args)))
    
    (define (hashmap-alter* dtd dict key failure success)
      (call/cc
        ;; escape from whole hashmap-search entirely, when success / failure
        ;; return something other than through passed in continuation procedures
        (lambda (k)
          (define-values
            (new-dict ignored)
            (hashmap-search dict key
                            (lambda (insert ignore)
                              ;; handle when continuation procedure is called
                              ;; and force it into tail call
                              (call/cc (lambda (k2)
                                         (define result 
                                           (failure (lambda (value) (call-with-values (lambda () (insert value #f)) k2))
                                                    (lambda () (call-with-values (lambda () (ignore #f)) k2))))
                                         ;; neither insert nor ignore called -- return result to top level escape
                                         (k result))))
                            (lambda (key value update remove)
                              (call/cc (lambda (k2)
                                         (define result
                                           (success 
                                             key
                                             value
                                             (lambda (new-key new-value) (call-with-values (lambda () (update new-key new-value #f)) k2))
                                             (lambda () (call-with-values (lambda () (remove #f)) k2))))
                                         (k result))))))
          new-dict)))

    (make-dtd
     dictionary?-id (prep-dtd-arg hashmap?)
     dict-mutable?-id (lambda _ #f)
     dict-empty?-id (prep-dtd-arg hashmap-empty?)
     dict-contains?-id (prep-dtd-arg hashmap-contains?)
     dict-ref-id (prep-dtd-arg hashmap-ref)
     dict-ref/default-id (prep-dtd-arg hashmap-ref/default)
     dict-set-id (prep-dtd-arg hashmap-set)
     dict-adjoin-id (prep-dtd-arg hashmap-adjoin)
     dict-delete-id (prep-dtd-arg hashmap-delete)
     dict-delete-all-id (prep-dtd-arg hashmap-delete-all)
     dict-replace-id (prep-dtd-arg hashmap-replace)
     dict-intern-id (prep-dtd-arg hashmap-intern)
     dict-update-id (prep-dtd-arg hashmap-update)
     dict-update/default-id (prep-dtd-arg hashmap-update/default)
     dict-pop-id (prep-dtd-arg hashmap-pop)
     dict-filter-id (prep-dtd-arg hashmap-filter)
     dict-remove-id (prep-dtd-arg hashmap-remove)
     dict-alter-id hashmap-alter*
     dict-size-id (prep-dtd-arg hashmap-size)
     dict-for-each-id (prep-dtd-arg hashmap-for-each)
     dict-count-id (prep-dtd-arg hashmap-count)
     dict-keys-id (prep-dtd-arg hashmap-keys)
     dict-values-id (prep-dtd-arg hashmap-values)
     dict-entries-id (prep-dtd-arg hashmap-entries)
     dict-fold-id (prep-dtd-arg hashmap-fold)
     dict-map->list-id (prep-dtd-arg hashmap-map->list)
     dict->alist-id (prep-dtd-arg hashmap->alist)
     dict-comparator-id (prep-dtd-arg hashmap-key-comparator))))
