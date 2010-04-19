#lang scheme

; XXX Eliminate administrative redexs

(define (cps stx k)
  (syntax-case stx (lambda #%app if quote #%top)
    [(lambda args be)
     (with-syntax ([(dyn-k) (generate-temporaries (list #'lambda))])
       (quasisyntax/loc stx
         (#%app #,k (lambda (dyn-k . args) #,(cps #'be #'dyn-k)))))]
    [(#%app f a ...)
     (with-syntax ([(fv) (generate-temporaries (list #'f))])
       (cps #'f
            (quasisyntax/loc stx
              (lambda (fv)
                #,(cps-args #'fv k empty (syntax->list #'(a ...)))))))]
    [(if c t f)
     (with-syntax ([(cv dyn-k) (generate-temporaries (list #'c #'if))])
       (cps #'c 
            (quasisyntax/loc stx
              (lambda (cv) 
                (#%app (#%top . if) cv 
                       (lambda (dyn-k) #,(cps #'t #'dyn-k))
                       (lambda (dyn-k) #,(cps #'f #'dyn-k)))))))]
    [(quote q)
     (quasisyntax/loc stx
       (#%app #,k (quote q)))]
    [(#%top . id)
     (quasisyntax/loc stx
       (#%app #,k (#%top . id)))]
    [id
     (identifier? #'id)
     (quasisyntax/loc stx
       (#%app #,k id))]))

(define (cps-args fv k avs as)
  (if (empty? as)
      (quasisyntax/loc fv
        (#%app #,fv #,k #,@(reverse avs)))
      (let ([a (first as)])
        (with-syntax ([(av) (generate-temporaries (list a))])
          (cps a
               (quasisyntax/loc fv
                 (lambda (av)
                   #,(cps-args fv k (list* #'av avs) (rest as)))))))))

(define (top-cps stx)
  (cps stx #'(lambda (x) x)))

(define test
  #'(#%app
     (#%top . call-with-values)
     (lambda ()
       (#%app
        (lambda (fac t1 t2 t3)
          (#%app
           (lambda (t22655)
             (#%app
              (lambda (t32656)
                (#%app
                 (lambda (fac2653)
                   (#%app
                    (lambda (t12654)
                      (#%app
                       (lambda (tmp) (#%app (lambda () (#%app (#%app (#%top . unbox) fac2653) '10))))
                       (#%app
                        (lambda (newtemp:18 newtemp:19 newtemp:20 newtemp:21)
                          (#%app
                           (lambda (tmp:23)
                             (#%app
                              (lambda (tmp:26)
                                (#%app
                                 (lambda (tmp:29) (#%app (#%top . set-box!) t32656 newtemp:21))
                                 (#%app (#%top . set-box!) t22655 newtemp:20)))
                              (#%app (#%top . set-box!) t12654 newtemp:19)))
                           (#%app (#%top . set-box!) fac2653 newtemp:18)))
                        (lambda (n)
                          (if (#%app (#%top . zero?) n)
                              '0
                              (#%app (#%top . *) n (#%app (#%app (#%top . unbox) fac2653) (#%app (#%top . sub1) n)))))
                        (lambda args args)
                        (lambda (n . args) (#%app (#%top . cons) n args))
                        (lambda (n1 n2) (#%app (#%top . cons) n1 n2)))))
                    (#%app (#%top . box) t1)))
                 (#%app (#%top . box) fac)))
              (#%app (#%top . box) t3)))
           (#%app (#%top . box) t2)))
        (#%top . <undefined>:31)
        (#%top . <undefined>:31)
        (#%top . <undefined>:31)
        (#%top . <undefined>:31)))
     (#%top . print-values)))

(syntax->datum (top-cps test))