(define mapping-dtd
  (let ()

    (define (prep-dtd-arg proc)
      (lambda (dtd . args)
        (apply proc args)))
    
    (define (mapping-alter* dtd dict key failure success)
      (call/cc
        ;; escape from whole hashmap-search entirely, when success / failure
        ;; return something other than through passed in continuation procedures
        (lambda (k)
          (define-values
            (new-dict ignored)
            (mapping-search dict key
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
     dictionary?-id (prep-dtd-arg mapping?)
     dict-mutable?-id (lambda _ #f)
     dict-empty?-id (prep-dtd-arg mapping-empty?)
     dict-contains?-id (prep-dtd-arg mapping-contains?)
     dict-ref-id (prep-dtd-arg mapping-ref)
     dict-ref/default-id (prep-dtd-arg mapping-ref/default)
     dict-set-id (prep-dtd-arg mapping-set)
     dict-adjoin-id (prep-dtd-arg mapping-adjoin)
     dict-delete-id (prep-dtd-arg mapping-delete)
     dict-delete-all-id (prep-dtd-arg mapping-delete-all)
     dict-replace-id (prep-dtd-arg mapping-replace)
     dict-intern-id (prep-dtd-arg mapping-intern)
     dict-update-id (prep-dtd-arg mapping-update)
     dict-update/default-id (prep-dtd-arg mapping-update/default)
     dict-pop-id (prep-dtd-arg mapping-pop)
     dict-filter-id (prep-dtd-arg mapping-filter)
     dict-remove-id (prep-dtd-arg mapping-remove)
     dict-alter-id mapping-alter*
     dict-size-id (prep-dtd-arg mapping-size)
     dict-for-each-id (prep-dtd-arg mapping-for-each)
     dict-count-id (prep-dtd-arg mapping-count)
     dict-keys-id (prep-dtd-arg mapping-keys)
     dict-values-id (prep-dtd-arg mapping-values)
     dict-entries-id (prep-dtd-arg mapping-entries)
     dict-fold-id (prep-dtd-arg mapping-fold)
     dict-map->list-id (prep-dtd-arg mapping-map->list)
     dict->alist-id (prep-dtd-arg mapping->alist)
     dict-comparator-id (prep-dtd-arg mapping-key-comparator))))
