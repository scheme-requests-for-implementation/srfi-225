(define default-dtd
  (let ()

    ;; implementation of "default" dtd, used as a filler for undefined
    ;; functions in other dtds

    ;; primitives
    (define (not-implemented name)
      (lambda (dtd . args)
        (raise (dictionary-error (string-append name " not implemented") dtd))))
    (define default-dictionary? (not-implemented "dictionary?"))
    (define default-dict-mutable? (not-implemented "dict-mutable?"))
    (define default-dict-size (not-implemented "dict-size"))
    (define default-dict-alter (not-implemented "dict-alter"))
    
    (define (dict-alter* dtd dict key fail success)
      (if (dict-mutable? dtd dict)
          (dict-alter! dtd dict key fail success)
          (dict-alter dtd dict key fail success)))

    (define (default-dict-empty? dtd dictionary)
      (= 0 (dict-size dtd dictionary)))
    
    (define (default-dict=? dtd = dict1 dict2)
      (define (check-entries* keys)
        (cond
          ((null? keys) #t)
          (else (let* ((key (car keys))
                       (d1-value (dict-ref dtd dict1 key)))
                  (dict-ref dtd dict2 key
                            (lambda () #f)
                            (lambda (d2-value)
                              (if (= d1-value d2-value)
                                  (check-entries* (cdr keys))
                                  #f)))))))
      (and (= (dict-size dtd dict1)
              (dict-size dtd dict2))
           (check-entries* (dict-keys dtd dict1))))

    (define (default-dict-contains? dtd dictionary key)
      (dict-ref dtd dictionary key
                (lambda () #f) 
                (lambda (x) #t)))

    (define (default-dict-ref dtd dictionary key failure success)
      (dict-alter* dtd dictionary key
                     (lambda (insert ignore)
                       (failure))
                     (lambda (key value update remove)
                       (success value))))

    (define (default-dict-ref/default dtd dictionary key default)
      (dict-ref dtd dictionary key
                (lambda () default)
                (lambda (x) x)))
    
    (define (default-dict-find-key dtd dict cmp-proc)
      (define cmp (dict-comparator dtd dict))
      (define keys (dict-keys dtd dict))
      (when (not cmp)
        (raise (dictionary-error "dictionary doesn't have comparator")))
      (when (null? keys)
        (error "Cannot find min/max key in empty dictionary"))
      (let loop ((best (car keys))
                 (keys (cdr keys)))
        (cond
          ((null? keys) best)
          ((cmp-proc cmp (car keys) best)
           (loop (car keys) (cdr keys)))
          (else (loop best (cdr keys))))))
    
    (define (default-dict-min-key dtd dict)
      (default-dict-find-key dtd dict <?))
    
    (define (default-dict-max-key dtd dict)
      (default-dict-find-key dtd dict >?))

    ;; private
    (define (default-dict-set* dtd dictionary use-old? objs)
      (let loop ((objs objs)
                 (dictionary dictionary))
        (cond
         ((null? objs)
          dictionary)
         ((null? (cdr objs))
          (error "mismatch of key / values argument list" objs))
         (else (let* ((key (car objs))
                      (value (cadr objs))
                      (new-d (dict-alter* dtd dictionary key
                                         (lambda (insert ignore)
                                           (insert value))
                                         (lambda (key old-value update delete)
                                           (update key (if use-old? old-value value))))))
                 (loop (cddr objs)
                       new-d))))))

    (define (default-dict-set dtd dictionary . objs)
      (default-dict-set* dtd dictionary #f objs))

    (define (default-dict-adjoin dtd dictionary . objs)
      (default-dict-set* dtd dictionary #t objs))

    (define (default-dict-delete dtd dictionary . keys)
      (dict-delete-all dtd dictionary keys))

    (define (default-dict-delete-all dtd dictionary keylist)
      (let loop ((keylist keylist)
                 (d dictionary))
        (cond
          ((null? keylist) d)
          (else (let* ((key (car keylist))
                       (new-d (dict-alter* dtd d key
                                          (lambda (_ ignore)
                                            (ignore))
                                          (lambda (key old-value _ delete)
                                            (delete)))))
                  (loop (cdr keylist)
                        new-d))))))

    (define (default-dict-replace dtd dictionary key value)
      (dict-alter* dtd dictionary key
                  (lambda (_ ignore)
                    (ignore))
                  (lambda (key old-value update _)
                    (update key value))))

    (define (default-dict-intern dtd dictionary key failure)
      (dict-alter* dtd dictionary key
                  (lambda (insert _)
                    (let ((value (failure)))
                     (values (insert value) value)))
                  (lambda (key value update _)
                    (values dictionary value))))

    (define (default-dict-update dtd dictionary key updater failure success)
      (dict-alter* dtd dictionary key
                  (lambda (insert ignore)
                    (insert (updater (failure))))
                  (lambda (key value update _)
                    (update key (updater (success value))))))

    (define (default-dict-update/default* dtd dictionary dict-update-proc key updater default)
      (dict-update-proc dtd dictionary key updater
                        (lambda () default)
                        (lambda (x) x)))

    (define (default-dict-update/default dtd dictionary key updater default)
      (dict-update dtd dictionary key updater
                        (lambda () default)
                        (lambda (x) x)))

    (define (default-dict-pop dtd dictionary)
      (define (do-pop)
        (call/cc
         (lambda (cont)
           (dict-for-each dtd
                          (lambda (key value)
                            (define new-dict
                              (dict-delete dtd dictionary key))
                            (cont new-dict key value))
                          dictionary))))
      (define empty? (dict-empty? dtd dictionary))
      (if empty?
          (error "popped empty dictionary")
          (do-pop)))

    (define (default-dict-map dtd mapper dictionary)
      (define keys (dict-keys dtd dictionary))
      (let loop ((keys keys)
                 (dict dictionary))
        (if (null? keys)
            dict
            (let* ((key (car keys))
                   (val (mapper key (dict-ref dtd dict key))))
              (loop (cdr keys)
                    (dict-replace dtd dict key val))))))

    (define (default-dict-filter dtd pred dictionary)
      (define keys (dict-keys dtd dictionary))
      (define keys-to-delete
        (filter
         (lambda (key)
           (not (pred key (dict-ref dtd dictionary key))))
         keys))
      (dict-delete-all dtd dictionary keys-to-delete))

    (define (default-dict-remove dtd pred dictionary)
      (dict-filter dtd (lambda (key value)
                         (not (pred key value)))
                   dictionary))

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
    
    (define default-dict-for-each (not-implemented "dict-for-each"))
    
    (define (default-dict-for-each/filtered dtd pred proc dict)
      (dict-for-each dtd 
                     (lambda (key value)
                       (when (pred key)
                         (proc key value)))
                     dict))
    
    (define (default-dict-for-each< dtd proc dict key)
      (define cmp (dict-comparator dtd dict))
      (define (pred k)
        (<? cmp k key))
      (default-dict-for-each/filtered dtd pred proc dict))
    
    (define (default-dict-for-each<= dtd proc dict key)
      (define cmp (dict-comparator dtd dict))
      (define (pred k)
        (<=? cmp k key))
      (default-dict-for-each/filtered dtd pred proc dict))
    
    (define (default-dict-for-each> dtd proc dict key)
      (define cmp (dict-comparator dtd dict))
      (define (pred k)
        (>? cmp k key))
      (default-dict-for-each/filtered dtd pred proc dict))
    
    (define (default-dict-for-each>= dtd proc dict key)
      (define cmp (dict-comparator dtd dict))
      (define (pred k)
        (>? cmp k key))
      (default-dict-for-each/filtered dtd pred proc dict))
    
    (define (default-dict-for-each-in-open-interval dtd proc dict key1 key2)
      (define cmp (dict-comparator dtd dict))
      (define (pred k)
        (<? cmp key1 k key2))
      (default-dict-for-each/filtered dtd pred proc dict))
    
    (define (default-dict-for-each-in-closed-interval dtd proc dict key1 key2)
      (define cmp (dict-comparator dtd dict))
      (define (pred k)
        (<=? cmp key1 k key2))
      (default-dict-for-each/filtered dtd pred proc dict))
    
    (define (default-dict-for-each-in-open-closed-interval dtd proc dict key1 key2)
      (define cmp (dict-comparator dtd dict))
      (define (pred k)
        (and (<? cmp key1 k)
             (<=? cmp k key2)))
      (default-dict-for-each/filtered dtd pred proc dict))
    
    (define (default-dict-for-each-in-closed-open-interval dtd proc dict key1 key2)
      (define cmp (dict-comparator dtd dict))
      (define (pred k)
        (and (<=? cmp key1 k)
             (<? cmp k key2)))
      (default-dict-for-each/filtered dtd pred proc dict))
    
    (define (default-make-dict-generator dtd dict)
      (define-values (keys vals)
                     (dict-entries dtd dict))
      (lambda ()
        (if (null? keys)
            (eof-object)
            (let ((key (car keys))
                  (value (car vals)))
              (set! keys (cdr keys))
              (set! vals (cdr vals))
              (cons key value)))))
    
    (define (default-dict-accumulator dtd dict acc-proc)
      (lambda (arg)
        (if (eof-object? arg)
            dict
            (set! dict (acc-proc dtd dict (car arg) (cdr arg))))))
    
    (define (default-dict-set-accumulator dtd dict)
      (if (dict-mutable? dtd dict)
          (default-dict-accumulator dtd dict dict-set!)
          (default-dict-accumulator dtd dict dict-set)))
    
    (define (default-dict-adjoin-accumulator dtd dict)
      (if (dict-mutable? dtd dict)
          (default-dict-accumulator dtd dict dict-adjoin!)
          (default-dict-accumulator dtd dict dict-adjoin)))

    (let ()
      (define null-dtd (make-dtd-private (make-vector dict-procedures-count #f)))
      (define default-dtd
        (make-modified-dtd
         null-dtd
         dictionary?-id default-dictionary?
         dict-empty?-id default-dict-empty?
         dict-contains?-id default-dict-contains?
         dict=?-id default-dict=?
         dict-mutable?-id default-dict-mutable?
         dict-ref-id default-dict-ref
         dict-ref/default-id default-dict-ref/default
         dict-min-key-id default-dict-min-key
         dict-max-key-id default-dict-max-key
         dict-set-id default-dict-set
         dict-adjoin-id default-dict-adjoin
         dict-delete-id default-dict-delete
         dict-delete-all-id default-dict-delete-all
         dict-replace-id default-dict-replace
         dict-intern-id default-dict-intern
         dict-update-id default-dict-update
         dict-update/default-id default-dict-update/default
         dict-pop-id default-dict-pop
         dict-map-id default-dict-map
         dict-filter-id default-dict-filter
         dict-remove-id default-dict-remove
         dict-alter-id default-dict-alter
         dict-size-id default-dict-size
         dict-count-id default-dict-count
         dict-any-id default-dict-any
         dict-every-id default-dict-every
         dict-keys-id default-dict-keys
         dict-values-id default-dict-values
         dict-entries-id default-dict-entries
         dict-fold-id default-dict-fold
         dict-map->list-id default-dict-map->list
         dict->alist-id default-dict->alist
         dict-comparator-id default-dict-comparator
         
         dict-for-each-id default-dict-for-each
         dict-for-each<-id default-dict-for-each<
         dict-for-each<=-id default-dict-for-each<=
         dict-for-each>-id default-dict-for-each>
         dict-for-each>=-id default-dict-for-each>
         dict-for-each-in-open-interval-id default-dict-for-each-in-open-interval
         dict-for-each-in-closed-interval-id default-dict-for-each-in-closed-interval
         dict-for-each-in-open-closed-interval-id default-dict-for-each-in-open-closed-interval
         dict-for-each-in-closed-open-interval-id default-dict-for-each-in-closed-open-interval

         ;; generator procedures
         make-dict-generator-id default-make-dict-generator
         dict-set-accumulator-id default-dict-set-accumulator
         dict-adjoin-accumulator-id default-dict-adjoin-accumulator))

      ;; sanity check
      (vector-for-each
       (lambda (proc index)
         (unless (and proc (procedure? proc))
           (error "Missing or wrong default procedure definition" proc index)))
       (procvec default-dtd)
       (list->vector (iota dict-procedures-count)))

      default-dtd)))
