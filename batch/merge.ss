#lang scheme
(require compiler/zo-parse
         "util.ss"
         "mpi.ss"
         "nodep.ss"
         "update-toplevels.ss")

(define MODULE-TOPLEVEL-OFFSETS (make-hash))

(define (merge-compilation-top top)
  (match top
    [(struct compilation-top (max-let-depth prefix form))
     (define-values (new-max-let-depth new-prefix gen-new-forms)
       (merge-form max-let-depth prefix form))
     (define total-tls (length (prefix-toplevels new-prefix)))
     (define total-stxs (length (prefix-stxs new-prefix)))
     (define total-lifts (prefix-num-lifts new-prefix))
     (eprintf "max-let-depth ~S to ~S~n" max-let-depth new-max-let-depth)
     (eprintf "total toplevels ~S~n" total-tls)
     (eprintf "total stxs ~S~n" total-stxs)
     (eprintf "num-lifts ~S~n" total-lifts)
     (make-compilation-top 
      new-max-let-depth new-prefix 
      (make-splice (gen-new-forms new-prefix)))]
    [else (error 'merge "unrecognized: ~e" top)]))

(define (merge-forms max-let-depth prefix forms)
  (if (empty? forms)
      (values max-let-depth prefix (lambda _ empty))
      (let*-values ([(fmax-let-depth fprefix gen-fform) (merge-form max-let-depth prefix (first forms))]
                    [(rmax-let-depth rprefix gen-rforms) (merge-forms fmax-let-depth fprefix (rest forms))])
        (values rmax-let-depth
                rprefix
                (lambda args
                  (append (apply gen-fform args)
                          (apply gen-rforms args)))))))

(define (merge-form max-let-depth prefix form)
  (match form
    [(? mod?)
     (merge-module max-let-depth prefix form)]
    [(struct seq (forms))
     (merge-forms max-let-depth prefix forms)]
    [(struct splice (forms))
     (merge-forms max-let-depth prefix forms)]
    [else
     (values max-let-depth prefix (lambda _ (list form)))]))

(define (merge-prefix root-prefix mod-prefix)
  (match root-prefix
    [(struct prefix (root-num-lifts root-toplevels root-stxs))
     (match mod-prefix
       [(struct prefix (mod-num-lifts mod-toplevels mod-stxs))
        (make-prefix (+ root-num-lifts mod-num-lifts)
                     (append root-toplevels mod-toplevels)
                     (append root-stxs mod-stxs))])]))

(define (compute-new-modvar mv rw)
  (match mv
    [(struct module-variable (modidx sym pos phase))
     (match rw
       [(struct modvar-rewrite (self-modidx provide->toplevel))
        (eprintf "Rewriting ~a of ~S~n" pos (mpi->path* modidx))
        (+ (hash-ref MODULE-TOPLEVEL-OFFSETS self-modidx
                     (lambda ()
                       (error 'compute-new-modvar "toplevel offset not yet computed: ~S" self-modidx)))
           (provide->toplevel sym pos))])]))

(define (filter-rewritable-module-variable? toplevel-offset mod-toplevels)
  (define-values
    (i new-toplevels remap)
    (for/fold ([i 0]
               [new-toplevels empty]
               [remap empty])
      ([tl (in-list mod-toplevels)])
      (match tl
        [(and mv (struct module-variable (modidx sym pos phase)))
         (define rw (get-modvar-rewrite modidx))
         (unless (or (not phase) (zero? phase))
           (error 'eliminate-module-variables "Non-zero phases not supported: ~S" mv))
         (cond
           ; Primitive module like #%paramz
           [(symbol? rw)
            (eprintf "~S from ~S~n" sym rw)
            (values (add1 i)
                    (list* tl new-toplevels)
                    (list* (+ i toplevel-offset) remap))]
           [(modvar-rewrite? rw)
            (values i
                    new-toplevels
                    (list* (compute-new-modvar mv rw) remap))]
           [else
            (error 'filter-rewritable-module-variable? "Unsupported module-rewrite: ~S" rw)])]
        [tl
         (values (add1 i)
                 (list* tl new-toplevels)
                 (list* (+ i toplevel-offset) remap))])))
  (values (reverse new-toplevels)
          (reverse remap)))

(define (merge-module max-let-depth top-prefix mod-form)
  (match mod-form
    [(struct mod (name srcname self-modidx mod-prefix provides requires body syntax-body unexported mod-max-let-depth dummy lang-info internal-context))
     (define toplevel-offset (length (prefix-toplevels top-prefix)))
     (define topsyntax-offset (length (prefix-stxs top-prefix)))
     (define lift-offset (prefix-num-lifts top-prefix))
     (define mod-toplevels (prefix-toplevels mod-prefix))     
     (define-values (new-mod-toplevels toplevel-remap) (filter-rewritable-module-variable? toplevel-offset mod-toplevels))
     (define num-mod-toplevels
       (length toplevel-remap))
     (define mod-stxs
       (length (prefix-stxs mod-prefix)))
     (define mod-num-lifts
       (prefix-num-lifts mod-prefix))
     (define new-mod-prefix
       (struct-copy prefix mod-prefix
                    [toplevels new-mod-toplevels]))
     (hash-set! MODULE-TOPLEVEL-OFFSETS self-modidx toplevel-offset)
     (unless (= (length toplevel-remap)
                (length mod-toplevels))
       (error 'merge-module "Not remapping everything: ~S ~S~n" 
              mod-toplevels toplevel-remap))    
     (eprintf "[~S] Incrementing toplevels by ~a~n"
              name
              toplevel-offset)
     (eprintf "[~S] Incrementing lifts by ~a~n"
              name
              lift-offset)
     (eprintf "[~S] Filtered mod-vars from ~a to ~a~n" 
              name
              (length mod-toplevels)
              (length new-mod-toplevels))
     (values (max max-let-depth mod-max-let-depth)
             (merge-prefix top-prefix new-mod-prefix)
             (lambda (top-prefix)
               (define top-lift-start (prefix-lift-start top-prefix))
               (define mod-lift-start (prefix-lift-start mod-prefix))
               (define total-lifts (prefix-num-lifts top-prefix))
               (define max-toplevel (+ top-lift-start total-lifts))
               (define update
                 (update-toplevels 
                  (lambda (n)
                    (cond
                      [(mod-lift-start . <= . n)
                       ; This is a lift
                       (local [(define which-lift (- n mod-lift-start))
                               (define lift-tl (+ top-lift-start lift-offset which-lift))]
                         (when (lift-tl . >= . max-toplevel)
                           (error 'merge-module "[~S] lift error: orig(~a) which(~a) max(~a) lifts(~a) now(~a)" 
                                  name n which-lift num-mod-toplevels mod-num-lifts lift-tl))
                         lift-tl)]
                      [else
                       (list-ref toplevel-remap n)]))
                  (lambda (n)
                    (+ n topsyntax-offset))
                  (prefix-syntax-start top-prefix)))
               (map update body)))]))

(provide/contract
 [merge-compilation-top (compilation-top? . -> . compilation-top?)])