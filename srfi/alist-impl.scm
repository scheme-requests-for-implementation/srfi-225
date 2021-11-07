(define (make-alist-dtd key=)

  (define (alist? dtd l)
    (and (list? l)
         (or (null? l)
             (pair? (car l)))))
  
  (define (alist-mutable? dtd alist)
    #f)

  (define (alist-map dtd proc alist)
    (map
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

  (define (alist-delete dtd key alist)
    (filter
     (lambda (entry)
       (not (key= (car entry) key)))
     alist))

  (define (alist-alter dtd alist key failure success)
    (define (handle-success pair)
      (define old-key (car pair))
      (define old-value (cdr pair))
      (define (update new-key new-value)
        (cond
         ((and (eq? old-key
                    new-key)
               (eq? old-value
                    new-value))
          alist)
         (else
          (let ((new-list
                 (alist-cons
                  new-key new-value
                  (alist-delete dtd old-key alist))))
            new-list))))
      (define (remove)
        (alist-delete dtd old-key alist))
      (success old-key old-value update remove))

    (define (handle-failure)
      (define (insert value)
        (alist-cons key value alist))
      (define (ignore)
        alist)
      (failure insert ignore))
    (cond
     ((assoc key alist key=) => handle-success)
     (else (handle-failure))))

  (define (alist-size dtd alist)
    (length alist))

  (define (alist-foreach dtd proc alist)
    (define (proc* e)
      (proc (car e) (cdr e)))
    (for-each proc* alist))

  (define (alist->alist dtd alist)
    alist)

  (define (alist-comparator dtd dictionary)
    #f)

  (make-dtd
   dictionary?-id alist?
   dict-mutable?-id alist-mutable?
   dict-map-id alist-map
   dict-filter-id alist-filter
   dict-alter-id alist-alter
   dict-size-id alist-size
   dict-for-each-id alist-foreach
   dict->alist-id alist->alist
   dict-comparator-id alist-comparator))

(define alist-eqv-dtd (make-alist-dtd eqv?))
(define alist-equal-dtd (make-alist-dtd equal?))
