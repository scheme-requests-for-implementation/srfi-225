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

(define-syntax define/dict-proc
  (syntax-rules ()
    ((_ proc index)
     (define (proc dtd . args)
       (assume (dtd? dtd))
       (apply (vector-ref (procvec dtd) index) dtd args)))))

(define/dict-proc make-dictionary make-dictionary-index)
(define/dict-proc dictionary? dictionary?-index)
(define/dict-proc dict-empty? dict-empty?-index)
(define/dict-proc dict-contains? dict-contains?-index)

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
     ((vector-ref (procvec dtd) dict-ref-index) dtd dict key failure success))))

(define/dict-proc dict-ref/default dict-ref/default-index)
(define/dict-proc dict-set dict-set-index)
(define/dict-proc dict-set! dict-set!-index)
(define/dict-proc dict-adjoin dict-adjoin-index)
(define/dict-proc dict-adjoin! dict-adjoin!-index)
(define/dict-proc dict-delete dict-delete-index)
(define/dict-proc dict-delete! dict-delete!-index)
(define/dict-proc dict-delete-all dict-delete-all-index)
(define/dict-proc dict-delete-all! dict-delete-all!-index)
(define/dict-proc dict-replace dict-replace-index)
(define/dict-proc dict-replace! dict-replace!-index)
(define/dict-proc dict-intern dict-intern-index)
(define/dict-proc dict-intern! dict-intern!-index)

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
     ((vector-ref (procvec dtd) dict-update-index) dtd dict key updater failure success))))

(define dict-update!
  (case-lambda
    ((dtd dict key updater)
     (dict-update! dtd dict key updater
                   (lambda () (error "Key not found in dictionary" dict key))
                   values))

    ((dtd dict key updater failure)
     (dict-update! dtd dict key  updater failure values))

    ((dtd dict key updater failure success)
     (assume (dtd? dtd))
     ((vector-ref (procvec dtd) dict-update!-index) dtd dict key updater failure success))))

(define/dict-proc dict-update/default dict-update/default-index)
(define/dict-proc dict-update/default! dict-update/default!-index)
(define/dict-proc dict-pop dict-pop-index)
(define/dict-proc dict-pop! dict-pop!-index)
(define/dict-proc dict-map dict-map-index)
(define/dict-proc dict-map! dict-map!-index)
(define/dict-proc dict-filter dict-filter-index)
(define/dict-proc dict-filter! dict-filter!-index)
(define/dict-proc dict-remove dict-remove-index)
(define/dict-proc dict-remove! dict-remove!-index)
(define/dict-proc dict-search dict-search-index)
(define/dict-proc dict-search! dict-search!-index)
(define/dict-proc dict-copy dict-copy-index)
(define/dict-proc dict-size dict-size-index)
(define/dict-proc dict-for-each dict-for-each-index)
(define/dict-proc dict-count dict-count-index)
(define/dict-proc dict-any dict-any-index)
(define/dict-proc dict-every dict-every-index)
(define/dict-proc dict-keys dict-keys-index)
(define/dict-proc dict-values dict-values-index)
(define/dict-proc dict-entries dict-entries-index)
(define/dict-proc dict-fold dict-fold-index)
(define/dict-proc dict-map->list dict-map->list-index)
(define/dict-proc dict->alist dict->alist-index)
(define/dict-proc dict-comparator dict-comparator-index)

(define (dtd-ref dtd procindex)
  (vector-ref (procvec dtd) procindex))

(define (make-modified-dtd dtd . lst)
  (define vec (vector-copy (procvec dtd)))
  (do ((lst lst (cddr lst)))
      ((null? lst))
    (when (null? (cdr lst))
      (error "Uneven amount of arguments" lst))
    (let ((proc-index (car lst))
          (proc (cadr lst)))
      (unless (procedure? proc)
        (error "Not a procedure" proc))
      (vector-set! vec proc-index proc)))
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
