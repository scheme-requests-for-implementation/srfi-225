(define (make-alist-dtd key=)

  (define (make-alist dtd comparator)
    (when comparator
      (raise (dictionary-error "alist dtd doesn't accept comparator" dtd)))
    '())

  (define (alist? dtd l)
    (and (list? l)
         (or (null? l)
             (pair? (car l)))))

  (define (alist-map dtd proc alist)
    (map
     (lambda (e)
       (define key (car e))
       (define value (cdr e))
       (cons key (proc key value)))
     alist))

  (define (alist-map! dtd proc alist)
    (map!
     (lambda (e)
       (define key (car e))
       (define value (cdr e))
       (cons key (proc key value)))
     alist))

  (define (alist-filter dtd pred alist)
    (filter
     (lambda (e)
       (pred (car e) (cdr e)))
     alist))

  (define (alist-filter! dtd pred alist)
    (filter!
     (lambda (e)
       (pred (car e) (cdr e)))
     alist))

  (define (alist-delete dtd key alist)
    (filter
     (lambda (entry)
       (not (key= (car entry) key)))
     alist))

  (define (alist-delete! dtd key alist)
    (filter!
     (lambda (entry)
       (not (key= (car entry) key)))
     alist))

  (define (alist-search* dtd alist-delete-proc alist key failure success)
    (define (handle-success pair)
      (define old-key (car pair))
      (define old-value (cdr pair))
      (define (update new-key new-value obj)
        (cond
         ((and (eq? old-key
                    new-key)
               (eq? old-value
                    new-value))
          (values alist obj))
         (else
          (let ((new-list
                 (alist-cons
                  new-key new-value
                  (alist-delete-proc dtd old-key alist))))
            (values new-list obj)))))
      (define (remove obj)
        (values (alist-delete-proc dtd old-key alist) obj))
      (success old-key old-value update remove))

    (define (handle-failure)
      (define (insert value obj)
        (values (alist-cons key value alist)
                obj))
      (define (ignore obj)
        (values alist obj))
      (failure insert ignore))
    (cond
     ((assoc key alist key=) => handle-success)
     (else (handle-failure))))

  (define (alist-search dtd alist key failure success)
    (alist-search* dtd alist-delete alist key failure success))

  (define (alist-search! dtd alist key failure success)
    (alist-search* dtd alist-delete! alist key failure success))

  (define (alist-size dtd alist)
    (length alist))

  (define (alist-foreach dtd proc alist)
    (define (proc* e)
      (proc (car e) (cdr e)))
    (for-each proc* alist))

  (define (alist-copy dtd alist)
    (map
     (lambda (e)
       (cons (car e) (cdr e)))
     alist))

  (define (alist->alist dtd alist)
    (alist-copy dtd alist))

  (define (alist-comparator dtd dictionary)
    #f)

  (make-dtd
   make-dictionary-id make-alist
   dictionary?-id alist?
   dict-map-id alist-map
   dict-map!-id alist-map!
   dict-filter-id alist-filter
   dict-filter!-id alist-filter!
   dict-search-id alist-search
   dict-search!-id alist-search!
   dict-size-id alist-size
   dict-for-each-id alist-foreach
   dict->alist-id alist->alist
   dict-comparator-id alist-comparator
   dict-copy-id alist-copy))

(define alist-eqv-dtd (make-alist-dtd eqv?))
(define alist-equal-dtd (make-alist-dtd equal?))
