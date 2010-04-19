#lang scheme
(require (for-syntax scheme)
         (prefix-in scheme: scheme))
(provide #%module-begin #%datum
         zero? * sub1 cons
         lambda
         if
         set!
         quote
         quote-syntax
         #%app
         ; ----
         case-lambda
         begin
         begin0
         cond
         case
         let*
         letrec
         letrec*
         let-values
         let*-values
         letrec-values
         let)

; XXX Get rid of quote and quote-syntax

(define-syntax one-body-lambda 
  (syntax-rules ()
    [(_ fmls be)
     (scheme:lambda fmls be)]))

(define-syntax lambda
  (syntax-rules ()
    [(_ fmls be ...)
     (one-body-lambda fmls (begin be ...))]))

(define-syntax #%module-begin
  (syntax-rules (def) ; XXX Annoying
    [(_ (def f v) ... e)
     (scheme:#%module-begin
      (letrec ([f v] ...)
        e))]))

(define-syntax (prim-module-begin stx)
  (syntax-case stx ()
    [(_ e)
     (with-syntax ([ee
                    (local-expand #'e
                                  'expression 
                                  empty)])
       (syntax/loc stx
         (scheme:#%module-begin (cps ee (lambda (x) x)))))]))

(define-syntax case-lambda
  (syntax-rules ()
    ((_ (fmls b1 b2 ...))
     (lambda fmls b1 b2 ...))
    ((_ (fmls b1 b2 ...) ...)
     (lambda args
       (let ((n (length args)))
         (case-lambda-help args n
                           (fmls b1 b2 ...) ...))))))

(define-syntax case-lambda-help
  (syntax-rules ()
    ((_ args n)
     (assertion-violation #f
                          "unexpected number of arguments"))
    ((_ args n ((x ...) b1 b2 ...) more ...)
     (if (= n (length ’(x ...)))
         (apply (lambda (x ...) b1 b2 ...) args)
         (case-lambda-help args n more ...)))
    ((_ args n ((x1 x2 ... . r) b1 b2 ...) more ...)
     (if (>= n (length ’(x1 x2 ...)))
         (apply (lambda (x1 x2 ... . r) b1 b2 ...)
                args)
         (case-lambda-help args n more ...)))
    ((_ args n (r b1 b2 ...) more ...)
     (apply (lambda r b1 b2 ...) args))))

(define-syntax begin
  (syntax-rules ()
    [(_ e)
     e]
    [(_ fe re ...)
     ((one-body-lambda (tmp)
                       (begin re ...))
      fe)]))

(define-syntax begin0
  (syntax-rules ()
    [(_ e)
     e]
    [(_ fe re ...)
     (let-values ([(tmp) fe])
       (begin re ... tmp))]))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else result1 result2 ...))
     (begin result1 result2 ...))
    ((cond (test => result))
     (let ((temp test))
       (if temp (result temp))))
    ((cond (test => result) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           (result temp)
           (cond clause1 clause2 ...))))
    ((cond (test)) test)
    ((cond (test) clause1 clause2 ...)
     (let ((temp test))
       (if temp
           temp
           (cond clause1 clause2 ...))))
    ((cond (test result1 result2 ...))
     (if test (begin result1 result2 ...)))
    ((cond (test result1 result2 ...)
           clause1 clause2 ...)
     (if test
         (begin result1 result2 ...)
         (cond clause1 clause2 ...)))))

(define-syntax case
  (syntax-rules (else)
    ((case expr0
       ((key ...) res1 res2 ...)
       ...
       (else else-res1 else-res2 ...))
     (let ((tmp expr0))
       (cond
         ((memv tmp ’(key ...)) res1 res2 ...)
         ...
         (else else-res1 else-res2 ...))))
    ((case expr0
       ((keya ...) res1a res2a ...)
       ((keyb ...) res1b res2b ...)
       ...)
     (let ((tmp expr0))
       (cond
         ((memv tmp ’(keya ...)) res1a res2a ...)
         ((memv tmp ’(keyb ...)) res1b res2b ...)
         ...)))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 expr1) (name2 expr2) ...)
       body1 body2 ...)
     (let ((name1 expr1))
       (let* ((name2 expr2) ...)
         body1 body2 ...)))))

(define-syntax letrec
  (syntax-rules ()
    ((letrec () body1 body2 ...)
     (let () body1 body2 ...))
    ((letrec ((var init) ...) body1 body2 ...)
     (letrec-helper
      (var ...)
      ()
      ((var init) ...)
      body1 body2 ...))))

(define <undefined> #f) ; XXX

(define-syntax letrec-helper
  (syntax-rules ()
    ((letrec-helper
      ()
      (temp ...)
      ((var init) ...)
      body1 body2 ...)
     (let ((var <undefined>) ...)
       (let ((temp init) ...)
         (set! var temp)
         ...)
       (let () body1 body2 ...)))
    ((letrec-helper
      (x y ...)
      (temp ...)
      ((var init) ...)
      body1 body2 ...)
     (letrec-helper
      (y ...)
      (newtemp temp ...)
      ((var init) ...)
      body1 body2 ...))))

(define-syntax letrec*
  (syntax-rules ()
    ((letrec* ((var1 init1) ...) body1 body2 ...)
     ; XXX R6RS says 'let', but I think it should be 'let*'
     (let* ((var1 <undefined>) ...)
       (set! var1 init1)
       ...
       (let () body1 body2 ...)))))

(define-syntax let-values
  (syntax-rules ()
    ((let-values (binding ...) body1 body2 ...)
     (let-values-helper1
      ()
      (binding ...)
      body1 body2 ...))))

(define-syntax let-values-helper1
  ;; map over the bindings
  (syntax-rules ()
    ((let-values
         ((id temp) ...)
       ()
       body1 body2 ...)
     (let ((id temp) ...) body1 body2 ...))
    ((let-values
         assocs
       ((formals1 expr1) (formals2 expr2) ...)
       body1 body2 ...)
     (let-values-helper2
      formals1
      ()
      expr1
      assocs
      ((formals2 expr2) ...)
      body1 body2 ...))))

(define-syntax let-values-helper2
  ;; create temporaries for the formals
  (syntax-rules ()
    ((let-values-helper2
      ()
      temp-formals
      expr1
      assocs
      bindings
      body1 body2 ...)
     (call-with-values
      (lambda () expr1)
      (lambda temp-formals
        (let-values-helper1
         assocs
         bindings
         body1 body2 ...))))
    ((let-values-helper2
      (first . rest)
      (temp ...)
      expr1
      (assoc ...)
      bindings
      body1 body2 ...)
     (let-values-helper2
      rest
      (temp ... newtemp)
      expr1
      (assoc ... (first newtemp))
      bindings
      body1 body2 ...))
    ((let-values-helper2
      rest-formal
      (temp ...)
      expr1
      (assoc ...)
      bindings
      body1 body2 ...)
     (call-with-values
      (lambda () expr1)
      (lambda (temp ... . newtemp)
        (let-values-helper1
         (assoc ... (rest-formal newtemp))
         bindings
         body1 body2 ...))))))

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body1 body2 ...)
     (let () body1 body2 ...))
    ((let*-values (binding1 binding2 ...)
       body1 body2 ...)
     (let-values (binding1)
       (let*-values (binding2 ...)
         body1 body2 ...)))))

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     ((letrec ((tag (lambda (name ...)
                      body1 body2 ...)))
        tag)
      val ...))))

(define-syntax letrec-values
  (syntax-rules ()
    ((letrec-values ((Formals Exp) ...) Body0 Body ...)
     (letrec-values "OUTER" ((Formals Exp) ...)
       () () (begin Body0 Body ...)))
    
    ((letrec-values "OUTER" ((Formals Exp) Decl ...)
       TempsOut DeclsOut Body)
     ;; we need to process a new formals list.
     (letrec-values "INNER" Formals () 
       Formals Exp (Decl ...) TempsOut DeclsOut Body))
    
    ((letrec-values "OUTER" ()
       ((Var Temp) ...)
       ((Formals Exp ((DeclVar DeclTemp) ...)) ...)
       Body)
     ;; we're done processing all the decls.  Time to expand. 
     (let ((Temp #f) ...)
       (call-with-values (lambda () Exp)
                         (lambda Formals
                           (set! DeclTemp DeclVar) ... #f))
       ...
       (let ((Var Temp) ...)
         Body)))
    
    ((letrec-values "INNER" (Var . Rest) InnerTempsOut
       Formals Exp Decls TempsOut DeclsOut Body)
     ;; we found a new variable, so generate a temp and go on.
     (letrec-values "INNER" Rest ((Var temp) . InnerTempsOut)
       Formals Exp Decls ((Var temp) . TempsOut) DeclsOut Body))
    
    ((letrec-values "INNER" () InnerTempsOut
       Formals Exp Decls TempsOut DeclsOut Body)
     ;; we're done with these formals.  Go back to the outer loop.
     (letrec-values "OUTER" Decls
       TempsOut 
       ((Formals Exp InnerTempsOut) . DeclsOut)
       Body))
    
    ((letrec-values "INNER" Var InnerTempsOut
       Formals Exp Decls TempsOut DeclsOut Body)
     ;; we've found a rest variable.  Go back to the outer loop.
     (letrec-values "OUTER" Decls
       ((Var temp) . TempsOut) 
       ((Formals Exp ((Var temp) . InnerTempsOut)) . DeclsOut)
       Body))))