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

(define-syntax dtd-ref-stx
  (syntax-rules ()
    ((_ dtd index)
     (begin
       (vector-ref (procvec dtd) index)))))

(define-syntax define/dict-proc
  (syntax-rules ()
    ((_ proc index)
     (define (proc dtd . args)
       (assume (dtd? dtd))
       (apply (dtd-ref-stx dtd index) dtd args)))))

(define/dict-proc make-dictionary make-dictionary-id)
(define/dict-proc dict-unfold dict-unfold-id)
(define/dict-proc dictionary? dictionary?-id)
(define/dict-proc dict-empty? dict-empty?-id)
(define/dict-proc dict-contains? dict-contains?-id)

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
(define/dict-proc dict-set dict-set-id)
(define/dict-proc dict-set! dict-set!-id)
(define/dict-proc dict-adjoin dict-adjoin-id)
(define/dict-proc dict-adjoin! dict-adjoin!-id)
(define/dict-proc dict-delete dict-delete-id)
(define/dict-proc dict-delete! dict-delete!-id)
(define/dict-proc dict-delete-all dict-delete-all-id)
(define/dict-proc dict-delete-all! dict-delete-all!-id)
(define/dict-proc dict-replace dict-replace-id)
(define/dict-proc dict-replace! dict-replace!-id)
(define/dict-proc dict-intern dict-intern-id)
(define/dict-proc dict-intern! dict-intern!-id)

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
     ((dtd-ref-stx dtd dict-update-id) dtd dict key updater failure success))))

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
     ((dtd-ref-stx dtd dict-update!-id) dtd dict key updater failure success))))

(define/dict-proc dict-update/default dict-update/default-id)
(define/dict-proc dict-update/default! dict-update/default!-id)
(define/dict-proc dict-pop dict-pop-id)
(define/dict-proc dict-pop! dict-pop!-id)
(define/dict-proc dict-map dict-map-id)
(define/dict-proc dict-map! dict-map!-id)
(define/dict-proc dict-filter dict-filter-id)
(define/dict-proc dict-filter! dict-filter!-id)
(define/dict-proc dict-remove dict-remove-id)
(define/dict-proc dict-remove! dict-remove!-id)
(define/dict-proc dict-search dict-search-id)
(define/dict-proc dict-search! dict-search!-id)
(define/dict-proc dict-copy dict-copy-id)
(define/dict-proc dict-size dict-size-id)
(define/dict-proc dict-for-each dict-for-each-id)
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
