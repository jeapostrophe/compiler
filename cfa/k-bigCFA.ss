#lang scheme
(require scheme/set)

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

;; exp  ::= (make-ref    <label> <var>)
;;       |  (make-lam    <label> (<var1> ... <varN>) <call>)
;;       |  (make-num    <label> <number>)
;; call ::= (make-call      <label> <exp0> <exp1> ... <expN>)
;;       |  (make-prim-call <label> <prim> <exp0> <exp1> ... <expN>)

;; prim ::= +

;; label = uninterned symbol

;; CPS is a popular intermediate form for functional compilation.
;; Its simplicity also means that it takes less code to construct
;; the abstract interpreter.

;; Helpers
(define empty-set (set))

; map-set : (a -> b) (set a) -> (set b)
(define (map-set f s)
  (for/fold ([ns empty-set])
    ([e (in-set s)])
    (set-add ns (f e))))

; take* is like take but allows n to be larger than (length l)
(define (take* l n)
  (for/list ([e (in-list l)]
             [i (in-naturals)]
             #:when (i . < . n))
    e))

(define (list->set l) (apply set l))

;; Syntax.
(define-struct stx (label) #:prefab)
(define-struct (exp stx) () #:prefab)

(define-struct (ref exp) (var)
  #:property prop:custom-write
  (lambda (r p write?)
    (write (ref-var r) p)))
(define-struct (lam exp) (formals call)
  #:property prop:custom-write
  (lambda (l p write?)
    (write `(λ ,(lam-formals l) ,(lam-call l)) p)))

(define-struct (call stx) (fun args)
  #:property prop:custom-write
  (lambda (c p write?)
    (write `(,(call-fun c) ,@(call-args c)) p)))

(define-struct (num exp) (n) 
  #:property prop:custom-write
  (lambda (n p write?)
    (write (num-n n) p)))
(define-struct (prim-call stx) (prim args)
  #:property prop:custom-write
  (lambda (c p write?)
    (write `(,(prim-call-prim c) ,@(prim-call-args c)) p)))

;; Abstract state-space.

;; state ::= (make-state <call> <benv> <store> <time>)
(define-struct state (call benv store time) #:prefab)

;; benv = hash[var,addr]
;; A binding environment maps variables to addresses.
(define empty-benv (make-immutable-hasheq empty))

; benv-lookup : benv var -> addr
(define benv-lookup hash-ref)

; benv-extend : benv var addr -> benv
(define benv-extend hash-set)

; benv-extend* : benv list[var] list[addr] -> benv
(define (benv-extend* benv vars addrs)
  (for/fold ([benv benv])
    ([v (in-list vars)]
     [a (in-list addrs)])
    (benv-extend benv v a)))  

;; store = hash[addr,d]
;; A store (or a heap/memory) maps address to denotable values.
(define empty-store (make-immutable-hasheq empty))

; store-lookup : store addr -> d
(define (store-lookup s a)
  (hash-ref s a d-bot))

; store-update : store addr d -> store
(define (store-update store addr value)
  (hash-update store addr 
               (lambda (d) (d-join d value))
               d-bot))

; store-update* : store list[addr] list[d] -> store
(define (store-update* store addrs values)
  (for/fold ([store store])
    ([a (in-list addrs)]
     [v (in-list values)])
    (store-update store a v)))

; store-join : store store -> store
(define (store-join store1 store2)
  (for/fold ([new-store store1])
    ([(k v) (in-hash store2)])
    (store-update new-store k v)))

;; d = set[value]
;; An abstract denotable value is a set of possible values.
(define d-bot empty-set)

; d-join : d d -> d
(define d-join set-union)

;; value = clo
;; For pure CPS, closures are the only kind of value.

;; clo ::= (make-closure <lambda> <benv>)
;; Closures pair a lambda term with a binding environment that
;; determinse the value of its free variables.
(define-struct closure (lam benv) #:prefab)

;; addr = bind
;; Addresses can point to values in the store.
;; In pure CPS, the only kind of addresses are bindings.

;; bind ::= (make-binding <var> <time>)
;; A binding is minted each time a variable gets bound to a value.
(define-struct binding (var time) #:prefab)

;; time = (listof label)
;; In k-CFA, time is a bounded memory of program history.
;; In particular, it is the last k call sites through which
;; the program has traversed.
(define time-zero empty)

;; k-CFA parameters

;; Change these to alter the behavior of the analysis.

; k : natural
(define k (make-parameter 1))

; tick : call time -> time
(define (tick call time)
  (take* (list* (stx-label call) time) (k)))

; alloc : time -> var -> addr
(define (alloc time)
  (lambda (var)
    (make-binding var time)))

;; k-CFA abstract interpreter

; atom-eval : benv store -> exp -> d
(define (atom-eval benv store)
  (match-lambda
    [(struct num (_ k))
     (set k)]
    [(struct ref (_ var))
     (store-lookup store (benv-lookup benv var))]
    [(? lam? lam)
     (set (make-closure lam benv))]))

; prim-eval : prim (listof d) -> (set (listof d))
(define prim-eval
  (match-lambda*
    [(list '+ (list lhs-s rhs-s))
     (list->set
      (for*/list ([lhs (in-set lhs-s)]
                  [rhs (in-set rhs-s)])
        (match (cons lhs rhs)
          [(cons (? number?) (? number?))
           (list (set (+ lhs rhs)))]
          [_
           ; XXX This seems like it should do this another way
           (list (set 'error))])))]))

; next : state -> set[state]
(define (next st)
  (match-define (struct state (c benv store time)) st)
  (define time* (tick c time))
  (match c
    [(struct call (_ f args))
     (define procs ((atom-eval benv store) f))
     (define params (map (atom-eval benv store) args))
     (for/list ([proc (in-set procs)])
       (match proc
         [(struct closure ((struct lam (_ formals call*)) benv*))
          (define bindings (map (alloc time*) formals))
          (define benv**   (benv-extend* benv* formals bindings))
          (define store*   (store-update* store bindings params))
          (make-state call* benv** store* time*)]))]
    ; XXX The continuation of a primitive call must be a syntactic function
    ;     I could allow anything by uncommenting the lines below and replacing it with 'k'
    ;     But then I have to generate labels as I run, which seems wrong
    ;     Yet, the syntactic function restriction also seems weird
    [(struct prim-call (_ prim (list-rest (struct lam (_ formals call*)) args)))
     (define params (map (atom-eval benv store) args))
     (for/list ([ans (in-set (prim-eval prim params))])
       #;(define formals  (map (lambda (a) (gensym)) ans))
       (define bindings (map (alloc time*) formals))
       (define benv*    (benv-extend* benv formals bindings))
       (define store*   (store-update* store bindings ans))
       #;(define call* (make-call (new-label) k (map make-ref* formals)))
       (make-state call* benv* store* time))]
    [(or (? ref?) (? lam?))
     empty]))

;; State-space exploration.

; exlore : set[state] list[state] -> set[state]
(define (explore seen todo)
  (match todo
    [(list)
     seen]
    [(list-rest (? (curry set-member? seen)) todo)
     (explore seen todo)]
    [(list-rest st0 todo)
     (define succs (next st0))
     (explore (set-add seen st0)
              (append succs todo))]))

;; User Interface

; summarize : set[state] -> store
(define (summarize states) 
  (for/fold ([store empty-store])
    ([state (in-set states)])
    (store-join (state-store state) store)))

(define empty-mono-store (make-immutable-hasheq empty))

; monovariant-store : store -> alist[var,exp]
(define (monovariant-store store)
  (for/fold ([mono-store empty-mono-store])
    ([(b vs) (in-hash store)])
    (hash-update mono-store
                 (binding-var b)
                 (lambda (b-vs)
                   (set-union 
                    b-vs
                    (map-set monovariant-value vs)))
                 empty-set)))

; monovariant-value : val -> exp
(define monovariant-value
  (match-lambda
    [(? symbol? s) s] ; XXX
    [(? number? n) n]
    [(? closure? c) (closure-lam c)]))

; analyze : exp -> mono-summary
(define (analyze exp)
  (define init-state (make-state exp empty-benv empty-store time-zero))
  (define states (explore empty-set (list init-state)))
  (define summary (summarize states))
  (define mono-summary (monovariant-store summary))
  mono-summary)

; print-mono-summary : mono-summary -> void
(define (print-mono-summary ms)
  (for ([(i vs) (in-hash ms)])
    (printf "~a:~n" i)
    (for ([v (in-set vs)])
      (printf "\t~S~n" v))
    (printf "~n")))

;; Helper functions for constructing syntax trees:
(define new-label gensym)

(define (make-ref* var) 
  (make-ref (new-label) var))

(define (make-num* k) 
  (make-num (new-label) k))

(define (make-lambda* formals call)
  (make-lam (new-label) formals call))

(define (make-call* fun . args)
  (make-call (new-label) fun args))

(define (make-prim-call* prim . args)
  (make-prim-call (new-label) prim args))

(define (make-let* var exp call)
  (make-call* (make-lambda* (list var) call) exp))

;; The Standard Example
;;
;; In direct-style:
;;
;; (let* ((id (lambda (x) x))
;;        (a  (id (lambda (z) (halt z))))
;;        (b  (id (lambda (y) (halt y)))))
;;   (halt b))
(define standard-example
  (make-let* 'id (make-lambda* '(x k) (make-call* (make-ref* 'k) (make-ref* 'x)))
             (make-call* (make-ref* 'id)
                         (make-lambda* '(z) (make-ref* 'z))
                         (make-lambda* '(a) 
                                       (make-call* (make-ref* 'id)
                                                   (make-lambda* '(y) (make-ref* 'y))
                                                   (make-lambda* '(b) 
                                                                 (make-ref* 'b)))))))

#;(for ([a-k (in-list (list 0 1 2))])
    (printf "K = ~a~n" a-k)
    (parameterize ([k a-k])
      (print-mono-summary
       (analyze standard-example))))

"Simple +"
(print-mono-summary
 (analyze 
  (make-prim-call* '+
                   (make-lambda* '(ans) (make-ref* 'ans))
                   (make-num* 2)
                   (make-num* 3))))

"Simple + (error)"
(print-mono-summary
 (analyze 
  (make-prim-call* '+
                   (make-lambda* '(ans) (make-ref* 'ans))
                   (make-lambda* '(x) (make-ref* 'x))
                   (make-num* 3))))

"Through function"
(print-mono-summary
 (analyze 
  (make-call* (make-lambda* '(x)
                            (make-prim-call* '+
                                             (make-lambda* '(ans) (make-ref* 'ans))
                                             (make-num* 2)
                                             (make-ref* 'x)))
              (make-num* 3))))

"Through named function"
(print-mono-summary
 (analyze 
  (make-let* 'f (make-lambda* '(x k)
                              (make-prim-call* '+
                                               (make-lambda* '(ans) (make-call* (make-ref* 'k) (make-ref* 'ans)))
                                               (make-num* 2)
                                               (make-ref* 'x)))
             (make-call* (make-ref* 'f)
                         (make-num* 3)
                         (make-lambda* '(fin) (make-ref* 'fin))))))

"Through named function twice"
(print-mono-summary
 (analyze 
  (make-let* 'f (make-lambda* '(x k)
                              (make-prim-call* '+
                                               (make-lambda* '(ans) (make-call* (make-ref* 'k) (make-ref* 'ans)))
                                               (make-num* 2)
                                               (make-ref* 'x)))
             (make-call* (make-ref* 'f)
                         (make-num* 3)
                         (make-lambda* '(inter)
                                       (make-call* (make-ref* 'f)
                                                   (make-ref* 'inter)
                                                   (make-lambda* '(fin) (make-ref* 'fin))))))))

"Through named function twice"
(print-mono-summary
 (analyze 
  (make-let* 'f (make-lambda* '(x k)
                              (make-prim-call* '+
                                               (make-lambda* '(ans) (make-call* (make-ref* 'k) (make-ref* 'x)))
                                               (make-num* 2)
                                               (make-num* 3)))
             (make-call* (make-ref* 'f)
                         (make-num* 3)
                         (make-lambda* '(inter)
                                       (make-call* (make-ref* 'f)
                                                   (make-ref* 'inter)
                                                   (make-lambda* '(fin) (make-ref* 'fin))))))))