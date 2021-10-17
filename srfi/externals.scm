;; procedure definitions that don't rely on concrete implementations

(define-record-type <dtd>
  (make-dtd-private procvec)
  dtd?
  (procvec procvec))

(define-record-type <dtd-err>
  (make-dictionary-error message irritants)
  dictionary-error?
  (message dictionary-message)
  (irritants dictionary-irritants))

;; shorthand access to dtd procedure by index
(define-syntax dtd-ref-stx
  (syntax-rules ()
    ((_ dtd index)
     (begin
       (vector-ref (procvec dtd) index)))))

;; shorthand to define proc with using proc index
(define-syntax define/dict-proc
  (syntax-rules ()
    ((_ proc index)
     (define (proc dtd . args)
       (assume (dtd? dtd))
       (apply (dtd-ref-stx dtd index) dtd args)))))

;; define mutable and immutable versions of a procedure (such as dict-set! and dict-set)
;; with appropriate assertion for dict-mutable? value
;; when dtd is first arg, and dict is second arg
(define-syntax define/dict-proc-pair
  (syntax-rules ()
    ((_ proc-immutable proc-mutable index)
     (begin
       (define (proc-mutable dtd dict . args)
         (assume (dtd? dtd))
         (assume ((dtd-ref-stx dtd dict-mutable?-id) dtd dict) index)
         (apply (dtd-ref-stx dtd index) dtd dict args))
       (define (proc-immutable dtd dict . args)
         (assume (dtd? dtd))
         (assume (not ((dtd-ref-stx dtd dict-mutable?-id) dtd dict)) index)
         (apply (dtd-ref-stx dtd index) dtd dict args))))))

;; define mutable and immutable versions of a procedure (such as dict-set! and dict-set)
;; with appropriate assertion for dict-mutable? value
;; when dtd is first arg, and dict is third arg (ie filter, map shape signature)
(define-syntax define/dict-proc-pair*
  (syntax-rules ()
    ((_ proc-immutable proc-mutable index)
     (begin
       (define (proc-mutable dtd proc dict)
         (assume (dtd? dtd))
         (assume ((dtd-ref-stx dtd dict-mutable?-id) dtd dict) index)
         ((dtd-ref-stx dtd index) dtd proc dict))
       (define (proc-immutable dtd proc dict)
         (assume (dtd? dtd))
         (assume (not ((dtd-ref-stx dtd dict-mutable?-id) dtd dict)) index)
         ((dtd-ref-stx dtd index) dtd proc dict))))))

(define/dict-proc dictionary? dictionary?-id)
(define/dict-proc dict-empty? dict-empty?-id)
(define/dict-proc dict-contains? dict-contains?-id)
(define/dict-proc dict-mutable? dict-mutable?-id)
(define/dict-proc dict=? dict=?-id)

(define dict-ref
  (case-lambda
    ((dtd dict key)
     (dict-ref dtd dict key
               (lambda () (error "Key not found in dictionary" dict key))
               values))

    ((dtd dict key failure)
     (dict-ref dtd dict key failure values))

    ((dtd dict key failure success)
     (assume (dtd? dtd))
     ((dtd-ref-stx dtd dict-ref-id) dtd dict key failure success))))

(define/dict-proc dict-ref/default dict-ref/default-id)
(define/dict-proc dict-min-key dict-min-key-id)
(define/dict-proc dict-max-key dict-max-key-id)
(define/dict-proc-pair dict-set dict-set! dict-set-id)
(define/dict-proc-pair dict-adjoin dict-adjoin! dict-adjoin-id)
(define/dict-proc-pair dict-delete dict-delete! dict-delete-id)
(define/dict-proc-pair dict-delete-all dict-delete-all! dict-delete-all-id)
(define/dict-proc-pair dict-replace dict-replace! dict-replace-id)
(define/dict-proc-pair dict-intern dict-intern! dict-intern-id)

(define dict-update
  (case-lambda
    ((dtd dict key updater)
     (dict-update dtd dict key updater
                  (lambda () (error "Key not found in dictionary" dict key))
                  values))

    ((dtd dict key updater failure)
     (dict-update dtd dict key  updater failure values))

    ((dtd dict key updater failure success)
     (assume (dtd? dtd))
     (assume (not ((dtd-ref-stx dtd dict-mutable?-id) dtd dict)))
     ((dtd-ref-stx dtd dict-update-id) dtd dict key updater failure success))))

(define dict-update!
  (case-lambda
    ((dtd dict key updater)
     (dict-update dtd dict key updater
                  (lambda () (error "Key not found in dictionary" dict key))
                  values))

    ((dtd dict key updater failure)
     (dict-update dtd dict key  updater failure values))

    ((dtd dict key updater failure success)
     (assume (dtd? dtd))
     (assume ((dtd-ref-stx dtd dict-mutable?-id) dtd dict))
     ((dtd-ref-stx dtd dict-update-id) dtd dict key updater failure success))))

(define/dict-proc-pair dict-update/default dict-update/default! dict-update/default-id)
(define/dict-proc-pair dict-pop dict-pop! dict-pop-id)
(define/dict-proc-pair* dict-map dict-map! dict-map-id)
(define/dict-proc-pair* dict-filter dict-filter! dict-filter-id)
(define/dict-proc-pair* dict-remove dict-remove! dict-remove-id)
(define/dict-proc-pair dict-alter dict-alter! dict-alter-id)
(define/dict-proc dict-size dict-size-id)
(define/dict-proc dict-count dict-count-id)
(define/dict-proc dict-any dict-any-id)
(define/dict-proc dict-every dict-every-id)
(define/dict-proc dict-keys dict-keys-id)
(define/dict-proc dict-values dict-values-id)
(define/dict-proc dict-entries dict-entries-id)
(define/dict-proc dict-fold dict-fold-id)
(define/dict-proc dict-map->list dict-map->list-id)
(define/dict-proc dict->alist dict->alist-id)
(define/dict-proc dict-comparator dict-comparator-id)
(define/dict-proc dict-for-each dict-for-each-id)
(define/dict-proc dict-for-each< dict-for-each<-id)
(define/dict-proc dict-for-each<= dict-for-each<=-id)
(define/dict-proc dict-for-each> dict-for-each>-id)
(define/dict-proc dict-for-each>= dict-for-each>=-id)
(define/dict-proc dict-for-each-in-open-interval dict-for-each-in-open-interval-id)
(define/dict-proc dict-for-each-in-closed-interval dict-for-each-in-closed-interval-id)
(define/dict-proc dict-for-each-in-open-closed-interval dict-for-each-in-open-closed-interval-id)
(define/dict-proc dict-for-each-in-closed-open-interval dict-for-each-in-closed-open-interval-id)
(define/dict-proc make-dict-generator make-dict-generator-id)
(define/dict-proc dict-set-accumulator dict-set-accumulator-id)
(define/dict-proc dict-adjoin-accumulator dict-adjoin-accumulator-id)

(define (dtd-ref dtd procindex)
  (dtd-ref-stx dtd procindex))

(define (make-modified-dtd dtd . lst)
  (define vec (vector-copy (procvec dtd)))
  (do ((lst lst (cddr lst)))
      ((null? lst))
    (when (null? (cdr lst))
      (error "Uneven amount of arguments" lst))
    (let ((proc-id (car lst))
          (proc (cadr lst)))
      (unless (procedure? proc)
        (error "Not a procedure" proc))
      (vector-set! vec proc-id proc)))
  (make-dtd-private vec))

(define (make-dtd . lst)
  (apply make-modified-dtd default-dtd lst))

(define-syntax dtd-helper
  (syntax-rules ()
    ((_ (arg ...) (index proc) rest ...)
     (dtd-helper (arg ... index proc) rest ...))
    ((_ (arg ...))
     (make-dtd arg ...))))

(define-syntax dtd
  (syntax-rules ()
    ((_ (index proc) ...)
     (dtd-helper () (index proc) ...))))

(define (dictionary-error message . irritants)
  (make-dictionary-error message irritants))
