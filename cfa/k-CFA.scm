#lang scheme
;; A simple implementation of k-CFA.
;; Author: Matthew Might
;; Site:   http://matt.might.net/

;; k-CFA is a well-known hierarchy of increasingly precise
;; control-flow analyses that approximate the solution to the
;; control-flow problem.

;; This program is a simple implementation of an 
;; abstract-interpretion-based k-CFA for continuation-
;; passing-style lambda calculus (CPS).

;; Contrary to what one might expect, it does not use
;; constraint-solving.  Rather, k-CFA is implemented
;; as a small-step abstract interpreter.  In fact, it looks
;; suspiciously like an ordinary (if non-deterministic)
;; Scheme interpreter.

;; The analysis consists of exploring the reachable
;; parts of a graph of abstract machine states. 

;; Once constructed, a composite store mapping addresses
;; to values is synthesized.

;; After that, the composite store is further summarized to
;; produce a mapping from variables to simple lambda terms.


;; The language over which the abstract interpreter operates is the
;; continuation-passing style lambda calculus (CPS):

;; exp  ::= (ref    <label> <var>)
;;       |  (lambda <label> (<var1> ... <varN>) <call>)
;; call ::= (call   <label> <exp0> <exp1> ... <expN>)

;; label = integer

;; CPS is a popular intermediate form for functional compilation.
;; Its simplicity also means that it takes less code to construct
;; the abstract interpreter.


;; Syntax.

(define (var? exp) (symbol? exp))
(define (var<? v1 v2) (string<? (symbol->string v1) (symbol->string v2)))

(define (ref? exp) (and (pair? exp) (eq? (car exp) 'ref)))
(define (ref->var exp) (caddr exp))

(define (lambda? exp) (and (pair? exp) (eq? (car exp) 'lambda)))
(define (lambda<? lam1 lam2) (< (lambda->lab lam1) (lambda->lab lam2)))
(define (lambda->lab exp) (cadr exp))
(define (lambda->formals exp) (caddr exp))
(define (lambda->call exp) (cadddr exp))
  
(define (exp<? exp1 exp2)
  (cond
    ((and (var? exp1) (var? exp2))       (var<? exp1 exp2))
    ((var? exp1)                         #t)
    ((and (lambda? exp1) (lambda? exp2)) (lambda<? exp1 exp2))
    ((lambda? exp1)                      #t)
    (else                                (error "Can't compare expressions."))))

(define (call? term) (and (pair? term) (eq? (car term) 'call)))
(define (call->lab call) (cadr call))
(define (call->fun call) (caddr call))
(define (call->args call) (cdddr call))

(define (explode-call call k)
  (k (call->lab call)
     (call->fun call)
     (call->args call)))


;; Abstract state-space.

;; state ::= (<call> <benv> <store> <time>)

(define (make-state call benv store time)
  (list call benv store time))

(define (state->call state) (car state))
(define (state->benv state) (cadr state))
(define (state->store state) (caddr state))
(define (state->time state) (cadddr state))

(define (explode-state state k)
  (k (state->call state)
     (state->benv state)
     (state->store state)
     (state->time state)))


;; benv = alist[var,addr]
;; A binding environment maps variables to addresses.

; benv<? : benv benv -> boolean
(define (benv<? benv1 benv2)
  (lexico<? (lambda (e1 e2)
              (couple<? var<? addr<? e1 e2))
            benv1 benv2))

; benv-lookup : benv var -> addr
(define (benv-lookup benv var)
  (let ((entry (assq var benv)))
    (if entry
        (cadr entry)
        (begin (display "No value for ")
               (display var)
               (display " in ")
               (display benv)
               (newline)
               (error "Couldn't look up variable!")))))
  
; benv-extend : benv var addr -> benv
(define (benv-extend benv var addr)
  (cond
    ((null? benv)                 (list (list var addr)))
    ((var<? var (car (car benv))) (cons (car benv)
                                        (benv-extend (cdr benv) var addr)))
    ((var<? (car (car benv)) var) (cons (list var addr)
                                        benv))
    (else                         (cons (list var addr)
                                        (cdr benv)))))

; benv-extend* : benv list[var] list[addr] -> benv
(define (benv-extend* benv vars addrs)
  (if (and (pair? vars) (pair? addrs))
      (benv-extend* (benv-extend benv (car vars) (car addrs))
                    (cdr vars)
                    (cdr addrs))
      benv))
  

;; store = alist[addr,d]
;; A store (or a heap/memory) maps address to denotable values.

; store-insert : store addr d -> store
(define (store-insert store addr d)
  (if (not (pair? store))
      (list (list addr d))
      (if (equal? (car (car store)) addr)
          (cons (list addr d) (cdr store))
          (cons (car store) (store-insert (cdr store) addr d)))))
          
; store-lookup : store addr -> d
(define (store-lookup store addr)
  (let ((entry (assoc addr store)))
    (if entry (cadr entry) '())))

; store-update : store addr d -> store
(define (store-update store addr value)
  (let ((d (store-lookup store addr)))
    (store-insert store addr (d-join d value))))

; store-update* : store list[addr] list[d] -> store
(define (store-update* store addrs values)
  (if (or (not (pair? addrs)) (not (pair? values)))
      store
      (store-update* (store-update store (car addrs) (car values))
                     (cdr addrs)
                     (cdr values))))

; store-join : store store -> store
(define (store-join store1 store2)
  (unzip-k store2 (lambda (addrs values)
   (store-update* store1 addrs values))))


;; d = set[value]
;; An abstract denotable value is a set of possible values.

; d-join : d d -> d
(define (d-join d1 d2)
  (sorted-set-union value<? d1 d2))


;; value = clo
;; For pure CPS, closures are the only kind of value.

; value<? : value value -> boolean
(define (value<? clo1 clo2)
  (cond
    ((and (closure? clo1) (closure? clo2))   (closure<? clo1 clo2))
    (else                                    (error "Can't compare values."))))
     

;; clo ::= (closure <lambda> <benv>)
;; Closures pair a lambda term with a binding environment that
;; determinse the value of its free variables.

(define (closure? value)      (and (pair? value) (eq? (car value) 'closure)))
(define (closure->lambda clo) (cadr clo))
(define (closure->benv clo)   (caddr clo))

; closure<? : clo clo -> boolean
(define (closure<? clo1 clo2)
  (let ((lam1  (closure->lambda clo1))
        (lam2  (closure->lambda clo2))
        (benv1 (closure->benv clo1))
        (benv2 (closure->benv clo2)))
    (cond
      ((lambda<? lam1 lam2)   #t)
      ((lambda<? lam2 lam1)   #f)
      ((benv<?   benv1 benv2) #t)
      ((benv<?   benv2 benv1) #f)
      (else                   #f))))


;; addr = bind
;; Addresses can point to values in the store.
;; In pure CPS, the only kind of addresses are bindings.

; addr<? : addr addr -> boolean
(define (addr<? a1 a2) 
  (cond
    ((and (binding? a1) (binding? a2))  (binding<? a1 a2))
    (else (error "Can't compare addresses."))))


;; bind ::= (binding <var> <time>)
;; A binding is minted each time a variable gets bound to a value.

(define (binding? a)
  (and (pair? a) (eq? (car a) 'binding)))
(define (binding->var binding)
  (cadr binding))

; binding<? : binding binding -> boolean
(define (binding<? addr1 addr2)
  (let ((v1 (cadr addr1))
        (v2 (cadr addr2))
        (t1 (caddr addr1))
        (t2 (caddr addr2)))
    (cond
      ((var<? v1 v2) #t)
      ((var<? v2 v1) #f)
      ((time<? t1 t2) #t)
      ((time<? t2 t1) #f)
      (else #f))))


;; time = lab^k
;; In k-CFA, time is a bounded memory of program history.
;; In particular, it is the last k call sites through which
;; the program has traversed.

; time<? : time time -> boolean
(define (time<? time1 time2)
  (cond
    ((and (null? time1) (null? time2)) #f)
    ((null? time1) #t)
    ((null? time2) #f)
    ((< (car time1) (car time2)) #t)
    ((> (car time1) (car time2)) #f)
    (else (time<? (cdr time1) (cdr time2)))))



;; Utilities

; lexico<? : (a b -> boolean) list[a] list[b] -> boolean
(define (lexico<? < list1 list2)
  (cond
    ((and (null? list1) (null? list2))   #f)
    ((null? list1) #t)
    ((null? list2) #f)
    ((< (car list1) (car list2)) #t)
    ((< (car list2) (car list1)) #f)
    (else (lexico<? < (cdr list1) (cdr list2)))))
 
; couple<? : (a a -> boolean) (b b -> boolean) (a b) (a b) -> boolean
(define (couple<? <1 <2 p1 p2)
  (cond
    ((<1 (car p1) (car p2)) #t)
    ((<1 (car p2) (car p1)) #f)
    ((<2 (cadr p1) (cadr p2)) #t)
    ((<2 (cadr p2) (cadr p1)) #f)
    (else #f)))
 
; zip : list[a] list[b] -> list[a b]
(define (zip list1 list2)
  (if (and (pair? list1) (pair? list2))
      (cons (list (car list1) (car list2))
            (zip (cdr list1) (cdr list2)))
      '()))

; take : natural -> list[a] -> list[a]
(define (take k lst)
  (if (or (<= k 0) (not (pair? lst)))
      '()
      (cons (car lst) (take (- k 1) lst))))

; set-union : set set -> set
(define (set-union set1 set2)
  (if (not (pair? set1))
      set2
      (set-union (cdr set1) (set-insert (car set1) set2))))

; set-insert : a set[a] -> set[a]
(define (set-insert value set)
  (if (set-member? value set)
      set
      (cons value set)))

; set-member? : a set[a] -> boolean
(define (set-member? value set)
  (if (not (pair? set))
      #f
      (or (equal? (car set) value)
          (set-member? value (cdr set)))))

; sorted-set-union : (a a -> boolean) set[a] set[a] -> set[a]
(define (sorted-set-union < set1 set2)
  (if (not (pair? set1))
      set2
      (sorted-set-union < (cdr set1) (sorted-set-insert < (car set1) set2))))

; sorted-set-insert : (a a -> boolean) a set[a] -> set[a]
(define (sorted-set-insert < value set)
  (cond
    ((not (pair? set)) (list value))
    ((< value (car set)) (cons (car set)
                               (sorted-set-insert < value (cdr set))))
    ((< (car set) value) (cons value set))
    (else                set)))


; unzip-k : alist[a,b] -> (list[a] list[b] -> c) -> c
(define (unzip-k ablist k)
  (if (not (pair? ablist))
      (k '() '())
      (unzip-k (cdr ablist) (lambda (alist blist)
       (k (cons (car (car ablist)) alist)
          (cons (cadr (car ablist)) blist))))))
                        

;; k-CFA parameters

;; Change these to alter the behavior of the analysis.

; k : natural
(define k 1)

; tick : call time -> time
(define (tick call time)
  (take k (cons (call->lab call) time)))

; alloc : time -> var -> addr
(define (alloc time)
  (lambda (var)
    (list 'binding var time)))


;; k-CFA abstract interpreter

; atom-eval : benv store -> exp -> d
(define (atom-eval benv store)
  (lambda (exp)
    (cond
      ((ref? exp)      (store-lookup store (benv-lookup benv (ref->var exp))))
      ((lambda? exp)   (list (list 'closure exp benv)))
      (else            (display exp) (error "unknown expression type: " exp)))))
    

; next : state -> set[state]
(define (next state)
  (explode-state state (lambda (call benv store time)
   (if (not (call? call))
       '()
       (let ((time* (tick call time)))
         (explode-call call (lambda (lab f args)
           (let* ((procs  ((atom-eval benv store) f))
                  (params (map (atom-eval benv store) args)))
             (map (lambda (proc)
                    (cond 
                      ((closure? proc)
                       (let* ((lam   (closure->lambda proc))
                              (benv* (closure->benv proc)))
                         (let* ((formals  (lambda->formals lam))
                                (call*    (lambda->call lam))
                                (bindings (map (alloc time*) formals))
                                (benv**   (benv-extend* benv* formals bindings))
                                (store*   (store-update* store bindings params)))
                           (make-state call* benv** store* time*))))))
                  procs)))))))))
  

;; State-space exploration.

; exlore : set[state] list[state] -> set[state]
(define (explore seen todo)
  (cond
    ((null? todo)                  seen)
    ((set-member? (car todo) seen) (explore seen (cdr todo)))
    (else                          (let ((succs (next (car todo))))
                                     (explore (cons (car todo) seen)
                                              (append succs todo))))))
          

; summarize : set[state] -> store
(define (summarize states) 
  (if (not (pair? states))
      '()
      (store-join (state->store (car states))
                  (summarize (cdr states)))))

; monovariant-store : store -> alist[var,exp]
(define (monovariant-store store)
  (if (not (pair? store))
      '()
      (monovariant-store-update*
       (monovariant-store (cdr store))
       (monovariant-binding (car (car store)))
       (monovariant-values (cadr (car store))))))

; monovariant-binding : binding -> var
(define (monovariant-binding binding)
  (binding->var binding))

; monovariant-values : d -> list[exp]
(define (monovariant-values values)
  (map monovariant-value values))

; monovariant-value : val -> exp
(define (monovariant-value value)
  (cond
    ((closure? value) (closure->lambda value))
    (else             (error "Unsupported value type."))))

; monovariant-store-update* : alist[var,exp] var list[exp] -> alist[var,exp]
(define (monovariant-store-update* monostore var exps)
  (if (not (pair? exps))
      monostore
      (monovariant-store-update* 
       (monovariant-store-update monostore var (car exps))
       var (cdr exps))))

; monovariant-store-update : alist[var,exp] var exp -> alist[var,exp]
(define (monovariant-store-update monostore var exp)
  (cond
    ((not (pair? monostore))             (list (list var (list exp))))
    ((var<? var (car (car monostore)))   (cons (car monostore)
                                             (monovariant-store-update (cdr monostore) var exp)))
    ((var<? (car (car monostore)) var)   (cons (list var (list exp))
                                               monostore))
    (else
     (let ((current-values (cadr (car monostore))))
       (cons (list var (sorted-set-insert exp<? exp current-values))
             (cdr monostore))))))
                                               
  

;; Helper functions for constructing syntax trees:
(define label-count 1)

(define (new-label)
  (set! label-count (+ 1 label-count))
  label-count)

(define (make-ref var) 
  (list 'ref (new-label) var))
  
(define (make-lambda formals call)
  (list 'lambda (new-label) formals call))
  
(define (make-call fun args)
  (cons 'call 
        (cons (new-label) 
              (cons fun 
                    args))))
  
(define (make-let var exp call)
  (make-call (make-lambda (list var) call) (list exp)))




;; The Standard Example
;;
;; In direct-style:
;;
;; (let* ((id (lambda (x) x))
;;        (a  (id (lambda (z) (halt z))))
;;        (b  (id (lambda (y) (halt y)))))
;;   (halt b))
(define standard-example
  (make-let 'id (make-lambda '(x k) (make-call (make-ref 'k) (list (make-ref 'x))))
            (make-call (make-ref 'id)
                       (list (make-lambda '(z) (make-ref 'z))
                             (make-lambda '(a) 
                                          (make-call (make-ref 'id)
                                                     (list (make-lambda '(y) (make-ref 'y))
                                                           (make-lambda '(b) 
                                                                        (make-ref 'b)))))))))
            
(define init-state (make-state standard-example '() '() '()))

(define states (explore '() (list init-state)))

(define summary (summarize states))

(define mono-summary (monovariant-store summary))

mono-summary
