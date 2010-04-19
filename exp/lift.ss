#lang scheme

(define (formals->list stx)
  (syntax-case stx ()
    [(a . fmls)
     (list* #'a (formals->list #'fmls))]
    [()
     empty]
    [rest
     (list #'rest)]))

(define (unique l)
  (if (empty? l)
      empty
      (let ([k (first l)]
            [ur (unique (rest l))])
        (if (memf (lambda (i) (free-identifier=? k i)) ur)
            ur
            (list* k ur)))))

(define (lift stx)
  (syntax-case stx (lambda #%app #%const quote #%top)
    [(lambda args be)
     (with-syntax ([(new-lambda) (generate-temporaries (list #'lambda))])
       (let*-values ([(new-lambdas new-be) (lift #'be)]
                     [(fvs) (unique
                             (remove* (formals->list #'args)
                                      (free-vars new-be)
                                      free-identifier=?))])         
         (values (list* (quasisyntax/loc stx (define (new-lambda #,@fvs) (lambda args #,new-be)))
                        new-lambdas)
                 (quasisyntax/loc stx
                   (#%const (#%top . new-lambda) #,@fvs)))))]
    [(#%const e ...)
     (values empty stx)]
    [(#%app e ...)
     (let-values 
         ([(new-lambdas es)
           (for/fold ([new-lambdas empty]
                      [new-es empty])
             ([e (syntax->list #'(e ...))])             
             (let-values ([(e-lambdas new-e) (lift e)])
               (values (append e-lambdas new-lambdas)
                       (list* new-e new-es))))])
       (values new-lambdas
               (quasisyntax/loc stx
                 (#%app #,@(reverse es)))))]    
    [(quote q)
     (values empty stx)]
    [(#%top . id)
     (values empty stx)]
    [id
     (identifier? #'id)
     (values empty stx)]))

(define (free-vars stx)
  (syntax-case stx (#%app #%const lambda quote #%top)
    [(lambda fmls be)
     (remq* (formals->list #'fmls)
            (free-vars #'be))]
    [(#%const e ...)
     (apply append (map free-vars (syntax->list #'(e ...))))]
    [(#%app e ...)
     (apply append (map free-vars (syntax->list #'(e ...))))]
    [(#%top . id)
     empty]
    [id
     (identifier? #'id)
     (list #'id)]
    [(quote e)
     empty]))

(define (top-lift stx)
  (define-values (new-lambdas new-stx) (lift stx))
  (quasisyntax/loc stx
    (program #,@new-lambdas
             #,new-stx)))

(define test
  #'(#%app
     (lambda (temp2657)
       (#%app
        (lambda (temp2658) (#%app (lambda (temp2659) (#%app temp2657 (lambda (x) x) temp2658 temp2659)) (#%top . print-values)))
        (lambda (lambda2660)
          (#%app
           (lambda (temp2661)
             (#%app
              (lambda (temp2662)
                (#%app
                 (lambda (temp2663)
                   (#%app
                    (lambda (temp2664)
                      (#%app
                       (lambda (temp2665) (#%app temp2661 lambda2660 temp2662 temp2663 temp2664 temp2665))
                       (#%top . <undefined>:31)))
                    (#%top . <undefined>:31)))
                 (#%top . <undefined>:31)))
              (#%top . <undefined>:31)))
           (lambda (lambda2666 fac t1 t2 t3)
             (#%app
              (lambda (temp2667)
                (#%app
                 (lambda (temp2669)
                   (#%app (lambda (t22670) (#%app temp2669 (lambda (temp2668) (#%app temp2667 lambda2666 temp2668)) t22670)) t2))
                 (#%top . box)))
              (lambda (lambda2671 t22655)
                (#%app
                 (lambda (temp2672)
                   (#%app
                    (lambda (temp2674)
                      (#%app
                       (lambda (t32675) (#%app temp2674 (lambda (temp2673) (#%app temp2672 lambda2671 temp2673)) t32675))
                       t3))
                    (#%top . box)))
                 (lambda (lambda2676 t32656)
                   (#%app
                    (lambda (temp2677)
                      (#%app
                       (lambda (temp2679)
                         (#%app
                          (lambda (fac2680) (#%app temp2679 (lambda (temp2678) (#%app temp2677 lambda2676 temp2678)) fac2680))
                          fac))
                       (#%top . box)))
                    (lambda (lambda2681 fac2653)
                      (#%app
                       (lambda (temp2682)
                         (#%app
                          (lambda (temp2684)
                            (#%app
                             (lambda (t12685) (#%app temp2684 (lambda (temp2683) (#%app temp2682 lambda2681 temp2683)) t12685))
                             t1))
                          (#%top . box)))
                       (lambda (lambda2686 t12654)
                         (#%app
                          (lambda (temp2687)
                            (#%app
                             (lambda (temp2689)
                               (#%app
                                (lambda (temp2690)
                                  (#%app
                                   (lambda (temp2691)
                                     (#%app
                                      (lambda (temp2692)
                                        (#%app
                                         (lambda (temp2693)
                                           (#%app
                                            temp2689
                                            (lambda (temp2688) (#%app temp2687 lambda2686 temp2688))
                                            temp2690
                                            temp2691
                                            temp2692
                                            temp2693))
                                         (lambda (lambda2694 n1 n2)
                                           (#%app
                                            (lambda (temp2695)
                                              (#%app
                                               (lambda (n12696)
                                                 (#%app (lambda (n22697) (#%app temp2695 lambda2694 n12696 n22697)) n2))
                                               n1))
                                            (#%top . cons)))))
                                      (lambda (lambda2698 n . args)
                                        (#%app
                                         (lambda (temp2699)
                                           (#%app
                                            (lambda (n2700)
                                              (#%app (lambda (args2701) (#%app temp2699 lambda2698 n2700 args2701)) args))
                                            n))
                                         (#%top . cons)))))
                                   (lambda (lambda2702 . args) (#%app lambda2702 args))))
                                (lambda (lambda2703 n)
                                  (#%app
                                   (lambda (temp2715)
                                     (#%app
                                      (lambda (n2716)
                                        (#%app
                                         temp2715
                                         (lambda (temp2704)
                                           (#%app
                                            (#%top . if)
                                            temp2704
                                            (lambda (if2705) (#%app if2705 '0))
                                            (lambda (if2705)
                                              (#%app
                                               (lambda (temp2706)
                                                 (#%app
                                                  (lambda (n2707)
                                                    (#%app
                                                     (lambda (temp2713)
                                                       (#%app
                                                        (lambda (fac26532714)
                                                          (#%app
                                                           temp2713
                                                           (lambda (temp2709)
                                                             (#%app
                                                              (lambda (temp2711)
                                                                (#%app
                                                                 (lambda (n2712)
                                                                   (#%app
                                                                    temp2711
                                                                    (lambda (temp2710)
                                                                      (#%app
                                                                       temp2709
                                                                       (lambda (temp2708) (#%app temp2706 if2705 n2707 temp2708))
                                                                       temp2710))
                                                                    n2712))
                                                                 n))
                                                              (#%top . sub1)))
                                                           fac26532714))
                                                        fac2653))
                                                     (#%top . unbox)))
                                                  n))
                                               (#%top . *)))))
                                         n2716))
                                      n))
                                   (#%top . zero?)))))
                             (lambda (lambda2717 newtemp:18 newtemp:19 newtemp:20 newtemp:21)
                               (#%app
                                (lambda (temp2718)
                                  (#%app
                                   (lambda (temp2720)
                                     (#%app
                                      (lambda (fac26532721)
                                        (#%app
                                         (lambda (newtemp:182722)
                                           (#%app
                                            temp2720
                                            (lambda (temp2719) (#%app temp2718 lambda2717 temp2719))
                                            fac26532721
                                            newtemp:182722))
                                         newtemp:18))
                                      fac2653))
                                   (#%top . set-box!)))
                                (lambda (lambda2723 tmp:23)
                                  (#%app
                                   (lambda (temp2724)
                                     (#%app
                                      (lambda (temp2726)
                                        (#%app
                                         (lambda (t126542727)
                                           (#%app
                                            (lambda (newtemp:192728)
                                              (#%app
                                               temp2726
                                               (lambda (temp2725) (#%app temp2724 lambda2723 temp2725))
                                               t126542727
                                               newtemp:192728))
                                            newtemp:19))
                                         t12654))
                                      (#%top . set-box!)))
                                   (lambda (lambda2729 tmp:26)
                                     (#%app
                                      (lambda (temp2730)
                                        (#%app
                                         (lambda (temp2732)
                                           (#%app
                                            (lambda (t226552733)
                                              (#%app
                                               (lambda (newtemp:202734)
                                                 (#%app
                                                  temp2732
                                                  (lambda (temp2731) (#%app temp2730 lambda2729 temp2731))
                                                  t226552733
                                                  newtemp:202734))
                                               newtemp:20))
                                            t22655))
                                         (#%top . set-box!)))
                                      (lambda (lambda2735 tmp:29)
                                        (#%app
                                         (lambda (temp2736)
                                           (#%app
                                            (lambda (t326562737)
                                              (#%app
                                               (lambda (newtemp:212738) (#%app temp2736 lambda2735 t326562737 newtemp:212738))
                                               newtemp:21))
                                            t32656))
                                         (#%top . set-box!)))))))))))
                          (lambda (lambda2739 tmp)
                            (#%app
                             (lambda (temp2740) (#%app temp2740 lambda2739))
                             (lambda (lambda2741)
                               (#%app
                                (lambda (temp2744)
                                  (#%app
                                   (lambda (fac26532745)
                                     (#%app
                                      temp2744
                                      (lambda (temp2742) (#%app (lambda (temp2743) (#%app temp2742 lambda2741 temp2743)) '10))
                                      fac26532745))
                                   fac2653))
                                (#%top . unbox)))))))))))))))))))
     (#%top . call-with-values)))

(syntax->datum (top-lift test))