#lang scheme
(require compiler/zo-parse)

(define (prefix-syntax-start pre)
  (length (prefix-toplevels pre)))

(define (prefix-lift-start pre)
  (define syntax-start (prefix-syntax-start pre))
  (define total-stxs (length (prefix-stxs pre)))
  (+ syntax-start total-stxs (if (zero? total-stxs) 0 1)))

(define (eprintf . args)
  (apply fprintf (current-error-port) args))

(define (build-form-memo inner-update #:void? [void? #f])
  (define memo (make-hasheq))
  (define (update form . args)
    (cond
      [(hash-ref memo form #f) 
       => (lambda (x) x)]
      [else
       (if (indirect? form)
           (local [(define i (make-indirect #f))]
             (hash-set! memo form i)
             (local [(define nv (apply inner-update (indirect-v form) args))]
               (unless void?
                 (set-indirect-v! i nv))
               i))
           (local [(define nv (apply inner-update form args))]
             (hash-set! memo form nv)
             nv))]))
  update)

(define lang-info/c
  (or/c #f (vector/c module-path? symbol? any/c)))


(define (build-compiled-path base name)
  (build-path 
   (cond [(path? base) base]
	 [(eq? base 'relative) 'same]
	 [(eq? base #f) (error 'batch "Impossible")])
   "compiled"
   name))


(provide/contract
 [prefix-syntax-start (prefix? . -> . exact-nonnegative-integer?)]
 [prefix-lift-start (prefix? . -> . exact-nonnegative-integer?)]
 [eprintf ((string?) () #:rest (listof any/c) . ->* . void)]
 [build-form-memo 
  (((unconstrained-domain-> any/c))
   (#:void? boolean?)
   . ->* .
   (unconstrained-domain-> any/c))]
 [lang-info/c contract?]
 [build-compiled-path ((or/c path-string? (symbols 'relative) false/c) path-string? . -> . (or/c path-string? (symbols 'same 'up)))])