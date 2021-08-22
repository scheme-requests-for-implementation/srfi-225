(define default-dtd
  (let ()

    ;; implementation of "default" dtd, used as a filler for undefined
    ;; functions in other dtds

    ;; primitives
    (define (not-implemented name)
      (lambda (dtd . args)
        (raise (dictionary-error (string-append name " not implemented") dtd))))
    (define default-make-dictionary (not-implemented "make-dictionary"))
    (define default-dictionary? (not-implemented "dictionary?"))
    (define default-dict-size (not-implemented "dict-size"))
    (define default-dict-search (not-implemented "dict-search"))
    (define default-dict-search! (not-implemented "dict-search!"))
    (define default-dict-for-each (not-implemented "dict-for-each"))

    (define (default-dict-unfold dtd comparator stop? mapper successor seed)
      (let loop ((dict (make-dictionary dtd comparator))
                 (seed seed))
        (if (stop? seed)
            dict
            (let ()
              (define-values (key value) (mapper seed))
              (define new-seed (successor seed))
              (loop (dict-set! dtd dict key value)
                    new-seed)))))

    (define (default-dict-empty? dtd dictionary)
      (= 0 (dict-size dtd dictionary)))

    (define (default-dict-contains? dtd dictionary key)
      (dict-ref dtd dictionary key
                (lambda () #f) (lambda (x) #t)))

    (define (default-dict-ref dtd dictionary key failure success)
      (define-values
          (new-dict result)
        (dict-search dtd dictionary key
                     (lambda (_ ignore)
                       (ignore (failure)))
                     (lambda (key value update _)
                       (update key value (success value)))))
      result)

    (define (default-dict-ref/default dtd dictionary key default)
      (dict-ref dtd dictionary key
                (lambda () default)
                (lambda (x) x)))

    ;; private
    (define (default-dict-set* dtd dictionary dict-search-proc use-old? objs)
      (let loop ((objs objs)
                 (dictionary dictionary))
        (cond
         ((null? objs)
          dictionary)
         ((null? (cdr objs))
          (error "mismatch of key / values argument list" objs))
         (else (let*-values
                   (((key) (car objs))
                    ((value) (cadr objs))
                    ((new-d _) (dict-search-proc dtd dictionary key
                                                 (lambda (insert ignore)
                                                   (insert value #f))
                                                 (lambda (key old-value update delete)
                                                   (update key (if use-old? old-value value) #f)))))
                 (loop (cddr objs)
                       new-d))))))

    (define (default-dict-set dtd dictionary . objs)
      (default-dict-set* dtd dictionary dict-search #f objs))

    (define (default-dict-set! dtd dictionary . objs)
      (default-dict-set* dtd dictionary dict-search! #f objs))

    (define (default-dict-adjoin dtd dictionary . objs)
      (default-dict-set* dtd dictionary dict-search #t objs))

    (define (default-dict-adjoin! dtd dictionary . objs)
      (default-dict-set* dtd dictionary dict-search! #t objs))

    (define (default-dict-delete dtd dictionary . keys)
      (dict-delete-all dtd dictionary keys))

    (define (default-dict-delete! dtd dictionary . keys)
      (dict-delete-all! dtd dictionary keys))

    (define (default-dict-delete-all* dtd dictionary dict-search-proc keylist)
      (let loop ((keylist keylist)
                 (d dictionary))
        (cond
         ((null? keylist) d)
         (else (let*-values
                   (((key) (car keylist))
                    ((new-d _) (dict-search-proc dtd d key
                                                 (lambda (_ ignore)
                                                   (ignore #f))
                                                 (lambda (key old-value _ delete)
                                                   (delete #f)))))
                 (loop (cdr keylist)
                       new-d))))))

    (define (default-dict-delete-all dtd dictionary keylist)
      (default-dict-delete-all* dtd dictionary dict-search keylist))

    (define (default-dict-delete-all! dtd dictionary keylist)
      (default-dict-delete-all* dtd dictionary dict-search! keylist))

    (define (default-dict-replace* dtd dictionary dict-search-proc key value)
      (define-values
          (new-dict _)
        (dict-search-proc dtd dictionary key
                          (lambda (_ ignore)
                            (ignore #f))
                          (lambda (key old-value update _)
                            (update key value #f))))
      new-dict)

    (define (default-dict-replace dtd dictionary key value)
      (default-dict-replace* dtd dictionary dict-search key value))

    (define (default-dict-replace! dtd dictionary key value)
      (default-dict-replace* dtd dictionary dict-search! key value))

    (define (default-dict-intern* dtd dictionary dict-search-proc key failure)
      (dict-search-proc dtd dictionary key
                        (lambda (insert _)
                          (let ((value (failure)))
                            (insert value value)))
                        (lambda (key value update _)
                          (update key value value))))

    (define (default-dict-intern dtd dictionary key failure)
      (default-dict-intern* dtd dictionary dict-search key failure))

    (define (default-dict-intern! dtd dictionary key failure)
      (default-dict-intern* dtd dictionary dict-search! key failure))

    (define (default-dict-update* dtd dictionary dict-search-proc key updater failure success)
      (define-values
          (new-dict _)
        (dict-search-proc dtd dictionary key
                          (lambda (insert ignore)
                            (insert (updater (failure)) #f))
                          (lambda (key value update _)
                            (update key (updater (success value)) #f))))
      new-dict)

    (define (default-dict-update dtd dictionary key updater failure success)
      (default-dict-update* dtd dictionary dict-search key updater failure success))

    (define (default-dict-update! dtd dictionary key updater failure success)
      (default-dict-update* dtd dictionary dict-search! key updater failure success))

    (define (default-dict-update/default* dtd dictionary dict-update-proc key updater default)
      (dict-update-proc dtd dictionary key updater
                        (lambda () default)
                        (lambda (x) x)))

    (define (default-dict-update/default dtd dictionary key updater default)
      (default-dict-update/default* dtd dictionary dict-update key updater default))

    (define (default-dict-update/default! dtd dictionary key updater default)
      (default-dict-update/default* dtd dictionary dict-update! key updater default))

    (define (default-dict-pop* dtd dictionary dict-delete-proc)
      (define (do-pop)
        (call/cc
         (lambda (cont)
           (dict-for-each dtd
                          (lambda (key value)
                            (define new-dict
                              (dict-delete-proc dtd dictionary key))
                            (cont new-dict key value))
                          dictionary))))
      (define empty? (dict-empty? dtd dictionary))
      (if empty?
          (error "popped empty dictionary")
          (do-pop)))

    (define (default-dict-pop dtd dictionary)
      (default-dict-pop* dtd dictionary dict-delete))

    (define (default-dict-pop! dtd dictionary)
      (default-dict-pop* dtd dictionary dict-delete!))

    (define (default-dict-map* dtd dict-replace-proc mapper dictionary)
      (define keys (dict-keys dtd dictionary))
      (let loop ((keys keys)
                 (dict dictionary))
        (if (null? keys)
            dict
            (let* ((key (car keys))
                   (val (mapper key (dict-ref dtd dict key))))
              (loop (cdr keys)
                    (dict-replace-proc dtd dict key val))))))

    (define (default-dict-map dtd mapper dictionary)
      (default-dict-map* dtd dict-replace mapper dictionary))

    (define (default-dict-map! dtd mapper dictionary)
      (default-dict-map* dtd dict-replace! mapper dictionary))

    (define (default-dict-filter* dtd dict-delete-all-proc pred dictionary)
      (define keys (dict-keys dtd dictionary))
      (define keys-to-delete
        (filter
         (lambda (key)
           (not (pred key (dict-ref dtd dictionary key))))
         keys))
      (dict-delete-all-proc dtd dictionary keys-to-delete))

    (define (default-dict-filter dtd pred dictionary)
      (default-dict-filter* dtd dict-delete-all pred dictionary))

    (define (default-dict-filter! dtd pred dictionary)
      (default-dict-filter* dtd dict-delete-all! pred dictionary))

    (define (default-dict-remove* dtd dict-filter-proc pred dictionary)
      (dict-filter-proc dtd
                        (lambda (key value)
                          (not (pred key value)))
                        dictionary))

    (define (default-dict-remove dtd pred dictionary)
      (default-dict-remove* dtd dict-filter pred dictionary))

    (define (default-dict-remove! dtd pred dictionary)
      (default-dict-remove* dtd dict-filter! pred dictionary))

    (define (default-dict-copy dtd dictionary)
      (define dict (make-dictionary dtd (dict-comparator dtd dictionary)))
      (dict-for-each dtd
                     (lambda (key value)
                       (set! dict (dict-set! dtd dict key value)))
                     dictionary)
      dict)

    (define (default-dict-count dtd pred dictionary)
      (dict-fold dtd
                 (lambda (key value acc)
                   (if (pred key value)
                       (+ 1 acc)
                       acc))
                 0
                 dictionary))

    (define (default-dict-any dtd pred dictionary)
      (call/cc
       (lambda (cont)
         (dict-for-each dtd
                        (lambda (key value)
                          (define ret (pred key value))
                          (when ret
                            (cont ret)))
                        dictionary)
         #f)))

    (define (default-dict-every dtd pred dictionary)
      (define last #t)
      (call/cc
       (lambda (cont)
         (dict-for-each dtd
                        (lambda (key value)
                          (define ret (pred key value))
                          (when (not ret)
                            (cont #f))
                          (set! last ret))
                        dictionary)
         last)))

    (define (default-dict-keys dtd dictionary)
      (reverse
       (dict-fold dtd
                  (lambda (key value acc)
                    (cons key acc))
                  '()
                  dictionary)))

    (define (default-dict-values dtd dictionary)
      (reverse
       (dict-fold dtd
                  (lambda (key value acc)
                    (cons value acc))
                  '()
                  dictionary)))

    (define (default-dict-entries dtd dictionary)
      (define pair
        (dict-fold dtd
                   (lambda (key value acc)
                     (cons (cons key (car acc))
                           (cons value (cdr acc))))
                   (cons '() '())
                   dictionary))
      (values (reverse (car pair))
              (reverse (cdr pair))))

    (define (default-dict-fold dtd proc knil dictionary)
      (define acc knil)
      (dict-for-each dtd
                     (lambda (key value)
                       (set! acc (proc key value acc)))
                     dictionary)
      acc)

    (define (default-dict-map->list dtd proc dictionary)
      (define reverse-lst
        (dict-fold dtd
                   (lambda (key value lst)
                     (cons (proc key value) lst))
                   '()
                   dictionary))
      (reverse reverse-lst))

    (define (default-dict->alist dtd dictionary)
      (dict-map->list dtd
                      cons
                      dictionary))

    (define default-dict-comparator (not-implemented "dict-comparator"))

    (let ()
      (define null-dtd (make-dtd-private (make-vector dict-procedures-count #f)))
      (define default-dtd
        (make-modified-dtd
         null-dtd
         make-dictionary-id default-make-dictionary
         dict-unfold-id default-dict-unfold
         dictionary?-id default-dictionary?
         dict-empty?-id default-dict-empty?
         dict-contains?-id default-dict-contains?
         dict-ref-id default-dict-ref
         dict-ref/default-id default-dict-ref/default
         dict-set-id default-dict-set
         dict-set!-id default-dict-set!
         dict-adjoin-id default-dict-adjoin
         dict-adjoin!-id default-dict-adjoin!
         dict-delete-id default-dict-delete
         dict-delete!-id default-dict-delete!
         dict-delete-all-id default-dict-delete-all
         dict-delete-all!-id default-dict-delete-all!
         dict-replace-id default-dict-replace
         dict-replace!-id default-dict-replace!
         dict-intern-id default-dict-intern
         dict-intern!-id default-dict-intern!
         dict-update-id default-dict-update
         dict-update!-id default-dict-update!
         dict-update/default-id default-dict-update/default
         dict-update/default!-id default-dict-update/default!
         dict-pop-id default-dict-pop
         dict-pop!-id default-dict-pop!
         dict-map-id default-dict-map
         dict-map!-id default-dict-map!
         dict-filter-id default-dict-filter
         dict-filter!-id default-dict-filter!
         dict-remove-id default-dict-remove
         dict-remove!-id default-dict-remove!
         dict-search-id default-dict-search
         dict-search!-id default-dict-search!
         dict-copy-id default-dict-copy
         dict-size-id default-dict-size
         dict-for-each-id default-dict-for-each
         dict-count-id default-dict-count
         dict-any-id default-dict-any
         dict-every-id default-dict-every
         dict-keys-id default-dict-keys
         dict-values-id default-dict-values
         dict-entries-id default-dict-entries
         dict-fold-id default-dict-fold
         dict-map->list-id default-dict-map->list
         dict->alist-id default-dict->alist
         dict-comparator-id default-dict-comparator))

      ;; sanity check
      (vector-for-each
       (lambda (proc index)
         (unless (and proc (procedure? proc))
           (error "Missing or wrong default procedure definition" proc index)))
       (procvec default-dtd)
       (list->vector (iota dict-procedures-count)))

      default-dtd)))
