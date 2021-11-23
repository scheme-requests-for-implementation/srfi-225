;; procedure definitions that don't rely on concrete implementations

(define-record-type <dto>
  (make-dto-private procvec)
  dto?
  (procvec procvec))

(define-record-type <dto-err>
  (make-dictionary-error message irritants)
  dictionary-error?
  (message dictionary-message)
  (irritants dictionary-irritants))

;; shorthand access to dto procedure by index
(define-syntax dto-ref-stx
  (syntax-rules ()
    ((_ dto index)
     (begin
       (vector-ref (procvec dto) index)))))

;; shorthand to define proc with using proc index
(define-syntax define/dict-proc
  (syntax-rules ()
    ((_ proc index)
     (define (proc dto . args)
       (assume (dto? dto))
       (apply (dto-ref-stx dto index) dto args)))))

;; define mutable and immutable versions of a procedure (such as dict-set! and dict-set)
;; with appropriate assertion for dict-mutable? value
;; when dto is first arg, and dict is second arg
(define-syntax define/dict-proc-pair
  (syntax-rules ()
    ((_ proc-immutable proc-mutable index)
     (begin
       (define (proc-mutable dto dict . args)
         (assume (dto? dto))
         (assume ((dto-ref-stx dto dict-mutable?-id) dto dict) index)
         (apply (dto-ref-stx dto index) dto dict args))
       (define (proc-immutable dto dict . args)
         (assume (dto? dto))
         (assume (not ((dto-ref-stx dto dict-mutable?-id) dto dict)) index)
         (apply (dto-ref-stx dto index) dto dict args))))))

;; define mutable and immutable versions of a procedure (such as dict-set! and dict-set)
;; with appropriate assertion for dict-mutable? value
;; when dto is first arg, and dict is third arg (ie filter, map shape signature)
(define-syntax define/dict-proc-pair*
  (syntax-rules ()
    ((_ proc-immutable proc-mutable index)
     (begin
       (define (proc-mutable dto proc dict)
         (assume (dto? dto))
         (assume ((dto-ref-stx dto dict-mutable?-id) dto dict) index)
         ((dto-ref-stx dto index) dto proc dict))
       (define (proc-immutable dto proc dict)
         (assume (dto? dto))
         (assume (not ((dto-ref-stx dto dict-mutable?-id) dto dict)) index)
         ((dto-ref-stx dto index) dto proc dict))))))

(define/dict-proc dictionary? dictionary?-id)
(define/dict-proc dict-empty? dict-empty?-id)
(define/dict-proc dict-contains? dict-contains?-id)
(define/dict-proc dict-mutable? dict-mutable?-id)
(define/dict-proc dict=? dict=?-id)

(define dict-ref
  (case-lambda
    ((dto dict key)
     (dict-ref dto dict key
               (lambda () (error "Key not found in dictionary" dict key))
               values))

    ((dto dict key failure)
     (dict-ref dto dict key failure values))

    ((dto dict key failure success)
     (assume (dto? dto))
     ((dto-ref-stx dto dict-ref-id) dto dict key failure success))))

(define/dict-proc dict-ref/default dict-ref/default-id)
(define/dict-proc-pair dict-set dict-set! dict-set-id)
(define/dict-proc-pair dict-adjoin dict-adjoin! dict-adjoin-id)
(define/dict-proc-pair dict-delete dict-delete! dict-delete-id)
(define/dict-proc-pair dict-delete-all dict-delete-all! dict-delete-all-id)
(define/dict-proc-pair dict-replace dict-replace! dict-replace-id)
(define/dict-proc-pair dict-intern dict-intern! dict-intern-id)

(define dict-update
  (case-lambda
    ((dto dict key updater)
     (dict-update dto dict key updater
                  (lambda () (error "Key not found in dictionary" dict key))
                  values))

    ((dto dict key updater failure)
     (dict-update dto dict key  updater failure values))

    ((dto dict key updater failure success)
     (assume (dto? dto))
     (assume (not ((dto-ref-stx dto dict-mutable?-id) dto dict)))
     ((dto-ref-stx dto dict-update-id) dto dict key updater failure success))))

(define dict-update!
  (case-lambda
    ((dto dict key updater)
     (dict-update dto dict key updater
                  (lambda () (error "Key not found in dictionary" dict key))
                  values))

    ((dto dict key updater failure)
     (dict-update dto dict key  updater failure values))

    ((dto dict key updater failure success)
     (assume (dto? dto))
     (assume ((dto-ref-stx dto dict-mutable?-id) dto dict))
     ((dto-ref-stx dto dict-update-id) dto dict key updater failure success))))

(define/dict-proc-pair dict-update/default dict-update/default! dict-update/default-id)
(define/dict-proc-pair dict-pop dict-pop! dict-pop-id)
(define/dict-proc-pair* dict-map dict-map! dict-map-id)
(define/dict-proc-pair* dict-filter dict-filter! dict-filter-id)
(define/dict-proc-pair* dict-remove dict-remove! dict-remove-id)
(define/dict-proc-pair dict-find-update dict-find-update! dict-find-update-id)
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

(define (dto-ref dto procindex)
  (dto-ref-stx dto procindex))

(define (make-modified-dto dto . lst)
  (define vec (vector-copy (procvec dto)))
  (do ((lst lst (cddr lst)))
      ((null? lst))
    (when (null? (cdr lst))
      (error "Uneven amount of arguments" lst))
    (let ((proc-id (car lst))
          (proc (cadr lst)))
      (unless (procedure? proc)
        (error "Not a procedure" proc))
      (vector-set! vec proc-id proc)))
  (make-dto-private vec))

(define (make-dto . lst)
  (apply make-modified-dto default-dto lst))

(define-syntax dto-helper
  (syntax-rules ()
    ((_ (arg ...) (index proc) rest ...)
     (dto-helper (arg ... index proc) rest ...))
    ((_ (arg ...))
     (make-dto arg ...))))

(define-syntax dto
  (syntax-rules ()
    ((_ (index proc) ...)
     (dto-helper () (index proc) ...))))

(define (dictionary-error message . irritants)
  (make-dictionary-error message irritants))
