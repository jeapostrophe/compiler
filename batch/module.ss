#lang scheme
(require compiler/zo-parse
         "util.ss")

(define (symbol->module-path-index s)
  (module-path-index-join `(quote ,s) #f))


(define (wrap-in-kernel-module name lang-info self-modidx top)
  (match top
    [(struct compilation-top (max-let-depth prefix form))
     (define-values (reqs new-forms)
       (partition req? (splice-forms form)))
     (define requires
       (map (compose symbol->module-path-index syntax->datum req-reqs) reqs))
     (make-compilation-top 
      0
      (make-prefix 0 (list #f) empty)
      (make-mod name
                (module-path-index-join #f #f)
                prefix
                empty ; provides
                (list (cons 0 requires))
                new-forms
                empty ; syntax-body
                (list empty empty empty) ; unexported
                max-let-depth
                (make-toplevel 0 0 #f #f) ; dummy
                lang-info
                #t))]))

(provide/contract
 [wrap-in-kernel-module (symbol? lang-info/c module-path-index? compilation-top? . -> . compilation-top?)])