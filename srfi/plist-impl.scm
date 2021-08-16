(define plist-dtd
  (let ()

    (define (make-plist dtd comparator)
      (when comparator
        (raise (dictionary-error "plist dtd doesn't accept comparator" dtd)))
      '())

    (define (plist? dtd l)
      (and (list? l)
           (or (null? l)
               (symbol? (car l)))))

    (define (plist-map dtd proc plist)
      (plist-map! dtd proc (dict-copy dtd plist)))

    (define (plist-map! dtd proc plist)
      (let loop ((pl plist))
        (cond
         ((null? pl) plist)
         ((null? (cdr pl)) (error "Malformed plist" plist))
         (else
          (let ((key (car pl))
                (value (cadr pl))
                (rest (cddr pl)))
            (set-car! (cdr pl)
                      (proc key value))
            (loop rest))))))

    (define (plist-filter dtd pred plist)
      (plist-filter! dtd pred (dict-copy dtd plist)))

    (define (plist-filter! dtd pred plist)
      (define head (cons #f plist))
      (let loop ((pl plist)
                 (parent-cell head))
        (cond
         ((null? pl) (cdr head))
         ((null? (cdr pl)) (error "Malformed plist" plist))
         (else
          (let ((key (car pl))
                (value (cadr pl))
                (rest (cddr pl)))
            (if (pred key value)
                (loop rest
                      (cdr pl))
                (loop (begin
                        (set-cdr! parent-cell rest)
                        rest)
                      parent-cell)))))))

    ;; head is a pair, whose cdr is the plist
    ;; if found, returns a pair, whose cdr is rest of plist, and cadr is key that was searched for
    ;; if not found, returns #f
    ;;
    ;; the pair indirection is used so that calling set-cdr! on the result allows the plist to be mutated
    (define (find-plist-entry key head)
      (define plist (cdr head))
      (cond
       ((null? plist) #f)
       ((equal? key (car plist)) head)
       (else (find-plist-entry key (cdr plist)))))

    (define (plist-search dtd plist key failure success)
      (plist-search! dtd (dict-copy dtd plist) key failure success))

    (define (plist-search! dtd plist key failure success)
      (define plist-head (cons #t plist))
      (define (handle-success head)
        (define key-cell (cdr head))
        (define val-cell (cddr head))
        (define (update new-key new-value obj)
          (set-car! key-cell new-key)
          (set-car! val-cell new-value)
          (values plist obj))
        (define (remove obj)
          (set-cdr! head (cddr (cdr head)))
          (values (cdr plist-head) obj))
        (success (car key-cell) (car val-cell) update remove))

      (define (handle-failure)
        (define (insert value obj)
          (values (cons key (cons value plist))
                  obj))
        (define (ignore obj)
          (values plist obj))
        (failure insert ignore))
      (cond
       ((find-plist-entry key plist-head) => handle-success)
       (else (handle-failure))))

    (define (plist-copy dtd plist)
      (list-copy plist))

    (define (plist-size dtd plist)
      (/ (length plist) 2))

    (define (plist-foreach dtd proc plist)
      (let loop ((pl plist))
        (if (null? pl) #t
            (begin
              (proc (car pl) (cadr pl))
              (loop (cddr pl))))))

    (define (plist-comparator dtd plist)
      (make-comparator symbol?
                       equal?
                       #f
                       #f))

    (make-dtd
     make-dictionary-index make-plist
     dictionary?-index plist?
     dict-map-index plist-map
     dict-map!-index plist-map!
     dict-filter-index plist-filter
     dict-filter!-index plist-filter!
     dict-search-index plist-search
     dict-search!-index plist-search!
     dict-copy-index plist-copy
     dict-size-index plist-size
     dict-for-each-index plist-foreach
     dict-comparator-index plist-comparator)
    ))
