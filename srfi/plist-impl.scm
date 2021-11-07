(define plist-dtd
  (let ()

    (define (plist? dtd l)
      (and (list? l)
           (or (null? l)
               (symbol? (car l)))))

    (define (plist-map dtd proc plist)
      (let loop ((pl plist)
                 (new-pl/rev '()))
        (cond
         ((null? pl) (reverse new-pl/rev))
         ((null? (cdr pl)) (error "Malformed plist" plist))
         (else
          (let ((key (car pl))
                (value (cadr pl))
                (rest (cddr pl)))
            (loop rest
                  (append (list (proc key value) key) new-pl/rev)))))))

    (define (plist-filter dtd pred plist)
      (let loop ((pl plist)
                 (new-pl/rev '()))
        (cond
         ((null? pl) (reverse new-pl/rev))
         ((null? (cdr pl)) (error "Malformed plist" plist))
         (else
          (let ((key (car pl))
                (value (cadr pl))
                (rest (cddr pl)))
            (if (pred key value)
                (loop rest
                      (append (list value key) new-pl/rev))
                (loop rest
                      new-pl/rev)))))))

    (define (find-plist-entry key plist)
      (cond
       ((null? plist) #f)
       ((eq? key (car plist)) plist)
       (else (find-plist-entry key (cddr plist)))))
    
    (define (plist-delete key-to-delete plist)
      (let loop ((pl plist)
                 (new-pl/rev '()))
        (cond
          ((null? pl) (reverse new-pl/rev))
          ((null? (cdr pl)) (error "Malformed plist"))
          (else (let ((key (car pl))
                      (value (cadr pl))
                      (rest (cddr pl)))
                  (if (eq? key-to-delete key)
                      (loop rest new-pl/rev)
                      (loop rest (append (list value key) new-pl/rev))))))))

    (define (plist-alter dtd plist key failure success)
      (define (handle-success pair)
        (define old-key (car pair))
        (define old-value (cadr pair))
        (define (update new-key new-value)
          (cond
            ((and (eq? old-key
                       new-key)
                  (eq? old-value
                       new-value))
             plist)
            (else
              (let ((new-list
                      (append (list new-key new-value)
                              (plist-delete old-key plist))))
                new-list))))
        (define (remove)
          (plist-delete old-key plist))
        (success old-key old-value update remove))

      (define (handle-failure)
        (define (insert value)
          (append (list key value) plist))
        (define (ignore)
          plist)
        (failure insert ignore))
      (cond
        ((find-plist-entry key plist) => handle-success)
        (else (handle-failure))))

    (define (plist-size dtd plist)
      (/ (length plist) 2))

    (define (plist-foreach dtd proc plist)
      (let loop ((pl plist))
        (if (null? pl) #t
            (begin
              (proc (car pl) (cadr pl))
              (loop (cddr pl))))))
    
    (define (plist-mutable? dtd plist)
      #f)

    (define (plist-comparator dtd plist)
      #f)

    (make-dtd
     dictionary?-id plist?
     dict-mutable?-id plist-mutable?
     dict-map-id plist-map
     dict-filter-id plist-filter
     dict-alter-id plist-alter
     dict-size-id plist-size
     dict-for-each-id plist-foreach
     dict-comparator-id plist-comparator)))
