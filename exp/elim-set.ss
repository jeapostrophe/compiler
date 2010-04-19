#lang scheme

(define (formals->list stx)
  (syntax-case stx ()
    [(a . fmls)
     (list* #'a (formals->list #'fmls))]
    [()
     empty]
    [rest
     (list #'rest)]))

(define (find-set! stx)
  (syntax-case stx (set! #%app lambda if quote #%top)
    [(set! x e)
     (list* #'x (find-set! #'e))]
    [(lambda fmls be)
     (remq* (formals->list #'fmls)
            (find-set! #'be))]
    [(if c t f)
     (apply append (map find-set! (syntax->list #'(c t f))))]
    [(#%app e ...)
     (apply append (map find-set! (syntax->list #'(e ...))))]
    [(#%top . id)
     empty]
    [id
     (identifier? #'id)
     empty]
    [(quote e)
     empty]))

(define (ihash-ref ht id not-found)
  (let/ec esc
    (hash-for-each 
     ht
     (lambda (k v)
       (when (free-identifier=? k id)
         (esc v))))
    not-found))

(define (replace-set! box-map stx)
  (syntax-case stx (set! #%app lambda if quote #%top)
    [(set! x e)
     (let ([boxed-x (ihash-ref box-map #'x #f)]
           [ne (replace-set! box-map #'e)])
       (if boxed-x       
           (quasisyntax/loc stx
             (#%app (#%top . set-box!) #,boxed-x #,ne))
           (quasisyntax/loc stx
             (set! x #,ne))))]
    [(lambda fmls be)
     (quasisyntax/loc stx
       (lambda fmls #,(replace-set! box-map #'be)))]
    [(if c t f)
     (quasisyntax/loc stx
       (if #,(replace-set! box-map #'c)
           #,(replace-set! box-map #'t)
           #,(replace-set! box-map #'f)))]
    [(#%app e ...)
     (quasisyntax/loc stx
       (#%app #,@(map (lambda (e-stx) 
                        (replace-set! box-map e-stx))
                      (syntax->list #'(e ...)))))]
    [(#%top . id)
     stx]
    [id
     (identifier? #'id)
     (let ([boxed-id (ihash-ref box-map #'id #f)])
       (if boxed-id
           (quasisyntax/loc stx 
             (#%app (#%top . unbox) #,boxed-id))
           stx))]
    [(quote e)
     stx]))

(define (elim-set! stx)
  (syntax-case stx (set! #%app lambda if quote #%top)
    [(set! x e)
     (quasisyntax/loc stx
       (set! x #,(elim-set! #'e)))]
    [(lambda fmls be)
     (let* ([nbe (elim-set! #'be)]
            [set!-ids (find-set! nbe)]
            [box-map (for/fold ([bm (make-immutable-hash empty)])
                       ([id (formals->list #'fmls)])
                       (if (memf (lambda (i) (free-identifier=? id i)) set!-ids)
                           (hash-set bm id (first (generate-temporaries (list id))))
                           bm))]
            [set!-removed-be (replace-set! box-map nbe)]
            [box-init-be (for/fold ([be set!-removed-be])
                           ([(id box-id) (in-hash box-map)])
                           (quasisyntax/loc stx
                             (#%app (lambda (#,box-id)
                                      #,be)
                                    (#%app (#%top . box) #,id))))])       
       (quasisyntax/loc stx
         (lambda fmls #,box-init-be)))]
    [(if c t f)
     (quasisyntax/loc stx
       (if #,(elim-set! #'c)
           #,(elim-set! #'t)
           #,(elim-set! #'f)))]
    [(#%app e ...)
     (quasisyntax/loc stx
       (#%app #,@(map elim-set!
                      (syntax->list #'(e ...)))))]
    [(#%top . id)
     stx]
    [id
     (identifier? #'id)
     stx]
    [(quote e)
     stx]))

(define test
  #'(#%app
     (#%top . call-with-values)
     (lambda ()
       (#%app
        (lambda (fac t1 t2 t3)
          (#%app
           (lambda (tmp) (#%app (lambda () (#%app fac (quote 10)))))
           (#%app
            (lambda (newtemp:18 newtemp:19 newtemp:20 newtemp:21)
              (#%app
               (lambda (tmp:23)
                 (#%app
                  (lambda (tmp:26)
                    (#%app
                     (lambda (tmp:29) (set! t3 newtemp:21))
                     (set! t2 newtemp:20)))
                  (set! t1 newtemp:19)))
               (set! fac newtemp:18)))
            (lambda (n)
              (if (#%app (#%top . zero?) n) (quote 0) (#%app (#%top . *) n (#%app fac (#%app (#%top . sub1) n)))))
            (lambda args args)
            (lambda (n . args) (#%app (#%top . cons) n args))
            (lambda (n1 n2) (#%app (#%top . cons) n1 n2)))))
        (#%top . <undefined>:31)
        (#%top . <undefined>:31)
        (#%top . <undefined>:31)
        (#%top . <undefined>:31)))
     (#%top . print-values)))

(syntax->datum (elim-set! test))