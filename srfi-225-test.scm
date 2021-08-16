(import (scheme base)
        (scheme case-lambda)
        (srfi 1)
        (prefix (srfi 69) t69:)
        (prefix (srfi 125) t125:)
        (srfi 128)
        (srfi 225))

(cond-expand
  (chibi
   (import (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (else
   (import (srfi 64))))

;; returns new wrapper dtd
;; which counts how often each dtd's method was called
;; verify that all functions were tested
(define (wrap-dtd dtd)
  (define proc-count (+ 1 dict-comparator-index))
  (define counter (make-vector proc-count 0))
  (define wrapper-dtd-args
    (let loop ((indexes (iota proc-count))
               (args '()))
      (if (null? indexes)
          args
          (let* ((index (car indexes))
                 (real-proc (dtd-ref dtd index))
                 (wrapper-proc (lambda args
                                 (vector-set! counter index (+ 1 (vector-ref counter index)))
                                 (apply real-proc args))))
            (loop (cdr indexes)
                  (append (list index wrapper-proc)
                          args))))))
  (values
   (apply make-dtd wrapper-dtd-args)
   counter))

(define (do-test real-dtd alist->dict comparator)

  (define-values
      (dtd counter)
    (wrap-dtd real-dtd))

  (test-group
   "make-dictionary"
   (define dict (make-dictionary dtd comparator))
   (test-assert (dictionary? dtd dict))
   (test-assert (dict-empty? dtd dict)))

  (test-group
    "dictionary?"
    (test-assert (not (dictionary? dtd 'foo)))
    (test-assert (dictionary? dtd (alist->dict '())))
    (test-assert (dictionary? dtd (alist->dict '((a . b))))))

  (test-group
    "dict-empty?"
    (test-assert (dict-empty? dtd (alist->dict '())))
    (test-assert (not (dict-empty? dtd (alist->dict '((a . b)))))))

  (test-group
    "dict-contains?"
    (test-assert (not (dict-contains? dtd (alist->dict '()) 'a)))
    (test-assert (not (dict-contains? dtd (alist->dict '((b . c))) 'a)))
    (test-assert (dict-contains? dtd (alist->dict '((a . b))) 'a)))

  (test-group
    "dict-ref"
    (test-assert (dict-ref dtd (alist->dict '((a . b))) 'a (lambda () #f) (lambda (x) #t)))
    (test-assert (dict-ref dtd (alist->dict '((a . b))) 'b (lambda () #t) (lambda (x) #f))))

  (test-group
    "dict-ref/default"
    (test-equal (dict-ref/default dtd (alist->dict '((a . b))) 'a 'c) 'b)
    (test-equal (dict-ref/default dtd (alist->dict '((a* . b))) 'a 'c) 'c))

  (test-group
   "dict-set"
   (define dict-original (alist->dict '((a . b))))
   (define d (dict-set dtd dict-original 'a 'c 'a2 'b2))
   (test-equal 'c (dict-ref dtd d 'a ))
   (test-equal 'b2 (dict-ref dtd d 'a2))
   (test-equal 'b (dict-ref dtd dict-original' a))
   (test-equal #f (dict-ref/default dtd dict-original 'a2 #f)))

  (test-group
    "dict-set!"
    (define d (dict-set! dtd (alist->dict '((a . b))) 'a 'c 'a2 'b2))
    (test-equal 'c (dict-ref dtd d 'a ))
    (test-equal 'b2 (dict-ref dtd d 'a2)))

  (test-group
   "dict-adjoin"
   (define dict-original (alist->dict '((a . b))))
   (define d (dict-adjoin dtd dict-original 'a 'c 'a2 'b2))
   (test-equal 'b (dict-ref dtd d 'a))
   (test-equal 'b2 (dict-ref dtd d 'a2))
   (test-equal #f (dict-ref/default dtd dict-original 'a2 #f)))

  (test-group
    "dict-adjoin!"
    (define d (dict-adjoin! dtd (alist->dict '((a . b))) 'a 'c 'a2 'b2))
    (test-equal 'b (dict-ref dtd d 'a))
    (test-equal 'b2 (dict-ref dtd d 'a2)))

  (test-group
   "dict-delete"
   (define dict-original (alist->dict '((a . b) (c . d))))
   (define d (dict-delete dtd dict-original 'a 'b))
   (test-equal (dict->alist dtd d) '((c . d)))
   (test-equal 'b (dict-ref dtd dict-original 'a)))

  (test-group
    "dict-delete!"
    (define d (dict-delete! dtd (alist->dict '((a . b) (c . d))) 'a 'b))
    (test-equal (dict->alist dtd d) '((c . d))))

  (test-group
   "dict-delete-all"
   (define dict-original (alist->dict '((a . b) (c . d))))
   (define d (dict-delete-all dtd dict-original '(a b)))
   (test-equal (dict->alist dtd d) '((c . d)))
   (test-equal 'b (dict-ref dtd dict-original 'a)))

  (test-group
    "dict-delete-all!"
    (define d (dict-delete-all! dtd (alist->dict '((a . b) (c . d))) '(a b)))
    (test-equal (dict->alist dtd d) '((c . d))))

  (test-group
   "dict-replace"
   (define dict-original (alist->dict '((a . b) (c . d))))
    (define d (dict-replace dtd dict-original 'a 'b2))
    (test-equal 'b2 (dict-ref dtd d 'a))
    (test-equal 'd (dict-ref dtd d 'c))
    (test-equal 'b (dict-ref dtd dict-original 'a)))

  (test-group
    "dict-replace!"
    (define d (dict-replace! dtd (alist->dict '((a . b) (c . d))) 'a 'b2))
    (test-equal 'b2 (dict-ref dtd d 'a))
    (test-equal 'd (dict-ref dtd d 'c)))

  (test-group
    "dict-intern"
    ;; intern existing
    (let ()
     (define-values
       (d value)
       (dict-intern dtd (alist->dict '((a . b))) 'a (lambda () 'd)))
     (test-equal 'b (dict-ref dtd d 'a))
     (test-equal 'b value))
    ;; intern missing
    (let ()
      (define dict-original (alist->dict '((a . b))))
     (define-values
       (d value)
       (dict-intern dtd dict-original 'c (lambda () 'd)))
     (test-equal 'b (dict-ref dtd d 'a))
     (test-equal 'd (dict-ref dtd d 'c))
     (test-equal 'd value)
     (test-equal #f (dict-ref/default dtd dict-original 'c #f))))

  (test-group
    "dict-intern!"
    ;; intern existing
    (let ()
     (define-values
       (d value)
       (dict-intern! dtd (alist->dict '((a . b))) 'a (lambda () 'd)))
     (test-equal 'b (dict-ref dtd d 'a))
     (test-equal 'b value))
    ;; intern missing
    (let ()
     (define-values
       (d value)
       (dict-intern! dtd (alist->dict '((a . b))) 'c (lambda () 'd)))
     (test-equal 'b (dict-ref dtd d 'a))
     (test-equal 'd (dict-ref dtd d 'c))
     (test-equal 'd value)))

  (test-group
    "dict-update"
    ;; update existing
    (define dict-original (alist->dict '((a . "b"))))
    (let ()
     (define d (dict-update dtd dict-original 'a
                      (lambda (value)
                        (string-append value "2"))
                      error
                      (lambda (x) (string-append x "1"))))
     (test-equal "b12" (dict-ref dtd d 'a))
     (test-equal "b" (dict-ref dtd dict-original 'a)))
    ;; update missing
    (let ()
     (define d (dict-update dtd dict-original 'c
                      (lambda (value)
                        (string-append value "2"))
                      (lambda () "d1")
                      (lambda (x) (string-append x "1"))))
     (test-equal "d12" (dict-ref dtd d 'c))
     (test-equal #f (dict-ref/default dtd dict-original 'c #f))))

  (test-group
    "dict-update!"
    ;; update existing
    (let ()
     (define d (dict-update! dtd (alist->dict '((a . "b"))) 'a
                      (lambda (value)
                        (string-append value "2"))
                      error
                      (lambda (x) (string-append x "1"))))
     (test-equal "b12" (dict-ref dtd d 'a)))
    ;; update missing
    (let ()
     (define d (dict-update! dtd (alist->dict '((a . "b"))) 'c
                      (lambda (value)
                        (string-append value "2"))
                      (lambda () "d1")
                      (lambda (x) (string-append x "1"))))
     (test-equal "d12" (dict-ref dtd d 'c))))

  (test-group
    "dict-update/default"
    ;; update existing
    (define dict-original (alist->dict '((a . "b"))))
    (let ()
     (define d (dict-update/default dtd dict-original 'a
                      (lambda (value)
                        (string-append value "2"))
                      "d1"))
     (test-equal "b2" (dict-ref dtd d 'a))
     (test-equal "b" (dict-ref dtd dict-original 'a)))

    ;; update missing
    (let ()
     (define d (dict-update/default dtd dict-original 'c
                      (lambda (value)
                        (string-append value "2"))
                      "d1"))
     (test-equal "d12" (dict-ref dtd d 'c))
     (test-equal #f (dict-ref/default dtd dict-original 'c #f))))

  (test-group
    "dict-update/default!"
    ;; update existing
    (let ()
     (define d (dict-update/default! dtd (alist->dict '((a . "b"))) 'a
                      (lambda (value)
                        (string-append value "2"))
                      "d1"))
     (test-equal "b2" (dict-ref dtd d 'a)))

    ;; update missing
    (let ()
     (define d (dict-update/default! dtd (alist->dict '((a . "b"))) 'c
                      (lambda (value)
                        (string-append value "2"))
                      "d1"))
     (test-equal "d12" (dict-ref dtd d 'c))))

  (test-group
   "dict-pop"
   (define dict-original (alist->dict '((a . b) (c . d))))
    (define-values
      (new-dict key value)
      (dict-pop dtd dict-original))
    (test-assert
      (or
        (and (equal? (dict->alist dtd new-dict) '((c . d)))
             (equal? key 'a)
             (equal? value 'b))

        (and (equal? (dict->alist dtd new-dict) '((a . b)))
             (equal? key 'c)
             (equal? value 'd))))
    (test-assert 'b (dict-ref dtd dict-original 'a))
    (test-assert 'd (dict-ref dtd dict-original 'c)))

  (test-group
    "dict-pop!"
    (define-values
      (new-dict key value)
      (dict-pop! dtd (alist->dict '((a . b) (c . d)))))
    (test-assert
      (or
        (and (equal? (dict->alist dtd new-dict) '((c . d)))
             (equal? key 'a)
             (equal? value 'b))

        (and (equal? (dict->alist dtd new-dict) '((a . b)))
             (equal? key 'c)
             (equal? value 'd)))))

  (test-group
   "dict-map"
   (define dict-original (alist->dict '((a . "a") (b . "b"))))
    (define d (dict-map dtd
                     (lambda (key value)
                       (string-append value "2"))
                     dict-original))
    (test-equal "a2" (dict-ref dtd d 'a))
    (test-equal "b2" (dict-ref dtd d 'b))
    (test-equal "a" (dict-ref dtd dict-original 'a))
    (test-equal "b" (dict-ref dtd dict-original 'b)))

(test-group
    "dict-map!"
    (define d (dict-map! dtd
                     (lambda (key value)
                       (string-append value "2"))
                     (alist->dict '((a . "a") (b . "b")))))
    (test-equal "a2" (dict-ref dtd d 'a))
    (test-equal "b2" (dict-ref dtd d 'b)))

  (test-group
   "dict-filter"
   (define dict-original (alist->dict '((a . b) (c . d))))
    (define d (dict-filter dtd
                     (lambda (key value)
                       (equal? value 'b))
                     dict-original))
    (test-equal '((a . b)) (dict->alist dtd d))
    (test-equal 'd (dict-ref dtd dict-original 'c)))

  (test-group
    "dict-filter!"
    (define d (dict-filter! dtd
                     (lambda (key value)
                       (equal? value 'b))
                     (alist->dict '((a . b) (c . d)))))
    (test-equal '((a . b)) (dict->alist dtd d)))

  (test-group
    "dict-remove"
    (define dict-original (alist->dict '((a . b) (c . d))))
    (define d (dict-remove dtd
                     (lambda (key value)
                       (equal? value 'b))
                     dict-original))
    (test-equal '((c . d)) (dict->alist dtd d))
    (test-equal 'd (dict-ref dtd dict-original 'c)))

  (test-group
    "dict-remove!"
    (define d (dict-remove! dtd
                     (lambda (key value)
                       (equal? value 'b))
                     (alist->dict '((a . b) (c . d)))))
    (test-equal '((c . d)) (dict->alist dtd d)))

  (test-group
    "dict-search"
    ;; ignore
    (let ()
     (define-values
       (dict value)
       (dict-search dtd (alist->dict '((a . b))) 'c
              (lambda (insert ignore)
                (ignore 'foo))
              (lambda args
                (error "shouldn't happen"))))
     (test-equal '((a . b)) (dict->alist dtd dict))
     (test-equal value 'foo))

    ;; insert
    (let ()
      (define dict-original (alist->dict '((a . b))))
     (define-values
       (dict value)
       (dict-search dtd dict-original 'c
              (lambda (insert ignore)
                (insert 'd 'foo))
              (lambda args
                (error "shouldn't happen"))))
     (test-equal 'b (dict-ref dtd dict 'a))
     (test-equal 'd (dict-ref dtd dict 'c))
     (test-equal value 'foo)
     (test-equal #f (dict-ref/default dtd dict-original 'c #f)))

    ;; update
    (let ()
      (define dict-original (alist->dict '((a . b))))
     (define-values
       (dict value)
       (dict-search dtd dict-original 'a
              (lambda args
                (error "shouldn't happen"))
              (lambda (key value update delete)
                (update 'a2 'b2 'foo))))
     (test-equal '((a2 . b2)) (dict->alist dtd dict))
     (test-equal value 'foo)
     (test-equal #f (dict-ref/default dtd dict-original 'a2 #f))
     (test-equal 'b (dict-ref dtd dict-original 'a)))

    ;; delete
    (let ()
      (define dict-original (alist->dict '((a . b) (c . d))))
     (define-values
       (dict value)
       (dict-search dtd dict-original 'a
              (lambda args
                (error "shouldn't happen"))
              (lambda (key value update delete)
                (delete 'foo))))
     (test-equal '((c . d)) (dict->alist dtd dict))
     (test-equal value 'foo)
     (test-equal 'b (dict-ref dtd dict-original 'a))))

  (test-group
    "dict-search!"
    ;; ignore
    (let ()
     (define-values
       (dict value)
       (dict-search! dtd (alist->dict '((a . b))) 'c
              (lambda (insert ignore)
                (ignore 'foo))
              (lambda args
                (error "shouldn't happen"))))
     (test-equal '((a . b)) (dict->alist dtd dict))
     (test-equal value 'foo))

    ;; insert
    (let ()
     (define-values
       (dict value)
       (dict-search! dtd (alist->dict '((a . b))) 'c
              (lambda (insert ignore)
                (insert 'd 'foo))
              (lambda args
                (error "shouldn't happen"))))
     (test-equal 'b (dict-ref dtd dict 'a))
     (test-equal 'd (dict-ref dtd dict 'c))
     (test-equal value 'foo))

    ;; update
    (let ()
     (define-values
       (dict value)
       (dict-search! dtd (alist->dict '((a . b))) 'a
              (lambda args
                (error "shouldn't happen"))
              (lambda (key value update delete)
                (update 'a2 'b2 'foo))))
     (test-equal '((a2 . b2)) (dict->alist dtd dict))
     (test-equal value 'foo))

    ;; delete
    (let ()
     (define-values
       (dict value)
       (dict-search! dtd (alist->dict '((a . b) (c . d))) 'a
              (lambda args
                (error "shouldn't happen"))
              (lambda (key value update delete)
                (delete 'foo))))
     (test-equal '((c . d)) (dict->alist dtd dict))
     (test-equal value 'foo)))

  (test-group
   "dict-copy"
   (define original-dict (alist->dict '((a . b))))
   (define copied-dict (dict-copy dtd original-dict))
   (test-assert (not (eq? original-dict copied-dict)))
   (set! original-dict (dict-set! dtd original-dict 'c 'd))
   (test-equal 'd (dict-ref dtd original-dict 'c))
   (test-equal #f (dict-ref/default dtd copied-dict 'c #f)))

  (test-group
    "dict-size"
    (test-equal 2 (dict-size dtd (alist->dict '((a . b) (c . d)))))
    (test-equal 0 (dict-size dtd (alist->dict '()))))

  (test-group
    "dict-for-each"
    (define lst '())
    (dict-for-each dtd
           (lambda (key value)
             (set! lst (append lst (list key value))))
           (alist->dict '((a . b) (c . d))))
    (test-assert
      (or (equal? '(a b c d) lst)
          (equal? '(c d a b) lst))))

  (test-group
    "dict-count"
    (define count (dict-count dtd
                         (lambda (key value)
                           (equal? value 'b))
                         (alist->dict '((a . b) (c . d)))))
    (test-equal count 1))

  (test-group
    "dict-any"

    (let ()
     (define value
       (dict-any dtd
              (lambda (key value)
                (if (equal? 'b value) 'foo #f))
              (alist->dict '((a . b) (c . d)))))
     (test-equal value 'foo))

    (let ()
     (define value
       (dict-any dtd
              (lambda (key value)
                (if (equal? 'e value) 'foo #f))
              (alist->dict '((a . b) (c . d)))))
     (test-equal value #f)))

  (test-group
    "dict-every"
    (let ()
     (define value
       (dict-every dtd
              (lambda (key value)
                (if (equal? 'b value) 'foo #f))
              (alist->dict '((a . b) (c . b)))))
     (test-equal value 'foo))

    (let ()
     (define value
       (dict-every dtd
              (lambda (key value)
                (if (equal? 'b value) 'foo #f))
              (alist->dict '())))
     (test-equal value #t))

    (let ()
     (define value
       (dict-every dtd
              (lambda (key value)
                (if (equal? 'b value) 'foo #f))
              (alist->dict '((a . b) (c . d)))))
     (test-equal value #f)))

  (test-group
    "dict-keys"
    (define keys
      (dict-keys dtd (alist->dict '((a . b) (c . d)))))
    (test-assert
      (or (equal? '(a c) keys)
          (equal? '(c a) keys))))

  (test-group
    "dict-values"
    (define vals
      (dict-values dtd (alist->dict '((a . b) (c . d)))))
    (test-assert
      (or (equal? '(b d) vals)
          (equal? '(d b) vals))))

  (test-group
    "dict-entries"
    (define-values
      (keys vals)
      (dict-entries dtd (alist->dict '((a . b) (c . d)))))
    (test-assert
      (or (and (equal? '(a c) keys)
               (equal? '(b d) vals))
          (and (equal? '(c a) keys)
               (equal? '(d b) vals)))))

  (test-group
    "dict-fold"
    (define value
      (dict-fold dtd
             (lambda (key value acc)
               (append acc (list key value)))
             '()
             (alist->dict '((a . b) (c . d)))))
    (test-assert
      (or (equal? '(a b c d) value)
          (equal? '(c d a b) value))))

  (test-group
    "dict-map->list"
    (define lst
      (dict-map->list dtd
             (lambda (key value)
               (string-append (symbol->string key)
                              value))
             (alist->dict '((a . "b") (c . "d")))))
    (test-assert
      (or (equal? '("ab" "cd") lst)
          (equal? '("cd" "ab") lst))))

  (test-group
    "dict->alist"
    (define alist
      (dict->alist dtd (alist->dict '((a . b) (c . d)))))
    (test-assert
      (or (equal? '((a . b) (c . d)) alist)
          (equal? '((c . d) (a . b)) alist))))

  (test-group
   "dict-comparator"
   ;; extremelly basic generic test; more useful specific tests defined separately
   ;; for each dtd
   (test-assert (comparator? (dict-comparator dtd (alist->dict '((a . b)))))))

  ;; check all procs were called
  (for-each
   (lambda (index)
     (when (= 0 (vector-ref counter index))
       (error "Untested procedure" index)))
   (iota (vector-length counter))))

(test-begin "Dictionaries")

(test-group
 "default"
 ;; test defaults by overring only procedures that raise error otherwise
 (define alist-dtd (make-alist-dtd equal?))
 (define default-dtd
   (make-modified-dtd
    alist-dtd
    make-dictionary-index (dtd-ref alist-dtd make-dictionary-index)
    dictionary?-index (dtd-ref alist-dtd dictionary?-index)
    dict-size-index (dtd-ref alist-dtd dict-size-index)
    dict-search-index (dtd-ref alist-dtd dict-search-index)
    dict-for-each-index (dtd-ref alist-dtd dict-for-each-index)))
 (do-test
  default-dtd
  alist-copy
  #f))

(test-group
  "alist"
  (do-test
   (make-alist-dtd equal?)
   ;; copy to a mutable list
   ;; so that mutating procedures don't fail
   alist-copy
   #f)

  ;; TODO test alist handling with different alist-dtd variants
  ;; TODO test comparator
  )

(test-group
  "plist"
  (do-test
   plist-dtd
    (lambda (alist)
      (apply append
             (map (lambda (pair)
                    (list (car pair) (cdr pair)))
                  alist)))
    #f)
  ;; TODO test comparator
  )

(test-group
 "srfi-69"
 (do-test
  srfi-69-dtd
  (lambda (alist)
    (define table (t69:make-hash-table equal?))
    (for-each
     (lambda (pair)
       (t69:hash-table-set! table (car pair) (cdr pair)))
     alist)
    table)
  (make-comparator (lambda args #t)
                   equal?
                   #f
                   #f))
 ;; TODO test comparator
 )

(test-group
 "srfi-125"
 (do-test
  hash-table-dtd
  (lambda (alist)
    (define table (t125:make-hash-table equal?))
    (for-each
     (lambda (pair)
       (t125:hash-table-set! table (car pair) (cdr pair)))
     alist)
    table)
  (make-comparator (lambda args #t)
                   equal?
                   #f
                   default-hash))
 ;; TODO test comparator
 )

#|
(cond-expand
  (guile)
  ((library (srfi 126))
   (test-group
     "srfi-126 (r6rs)"
     (include "srfi-126-impl.scm")
     (clear-registry!)
     (register-srfi-126!)
     (do-test (lambda (alist)
                (define table (make-eqv-hashtable))
                (for-each
                  (lambda (pair)
                    (hashtable-set! table (car pair) (cdr pair)))
                  alist)
                table))))
  (else))
|#


(test-end)
