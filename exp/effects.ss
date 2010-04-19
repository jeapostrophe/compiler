#lang scheme/gui
(require scheme/help
         scheme/foreign
         scheme/runtime-path)

; database
(define-runtime-path database-path "database")
(define database (hash-copy (file->value database-path)))

; application
(define (lookup-effect s)
  (hash-ref database s #f))

; testing
(define
  syms
  (apply append
         (for/list ([m (in-list '('#%kernel '#%unsafe '#%paramz '#%foreign '#%futures '#%network '#%place '#%expobs))])
           (define ns (make-base-empty-namespace))
           (define how-many 0)
           (parameterize ([current-namespace ns])
             (namespace-require m)
             (namespace-mapped-symbols)))))

(define (estring cur i)
  (format "~a~a" i
          (if (and cur (symbol=? cur i))
              " (current)"
              "")))

(define next #f)

(define (next-sym!)
  (if (empty? syms)
      #f
      (begin
        (set! next (first syms))
        (set! syms (rest syms))
        (local [(define cur (lookup-effect next))]
          (if cur
              (next-sym!)
              (begin
                (send yesb set-label (estring cur 'yes))
                (send nob set-label (estring cur 'no))
                (send maybeb set-label (estring cur 'maybe))
                (send dontknowb set-label (estring cur 'dontknow))
                (send msg set-label (symbol->string next))
                #t))))))

(define (set-sym! f)
  (hash-set! database next f)
  (next-sym!))

(define WIDTH 400)

(define frame (new frame% [label "Procedure Labeller"]
                   [width WIDTH]
                   [height 200]))
(define msg (new message% [parent frame]
                 [min-width WIDTH]
                 [label ""]))

(define yesb
  (new button% [parent frame]
       [label "Yes"]
       [min-width 200]
       (callback (lambda (button event) (set-sym! 'yes)))))
(define nob
  (new button% [parent frame]
       [label "No"]
       [min-width 200]
       (callback (lambda (button event) (set-sym! 'no)))))
(define maybeb
  (new button% [parent frame]
       [label "Maybe"]
       [min-width 200]
       (callback (lambda (button event) (set-sym! 'maybe)))))
(define dontknowb
  (new button% [parent frame]
       [label "Don't Know"]
       [min-width 200]
       (callback (lambda (button event) (set-sym! 'dontknow)))))

(define helpb
  (new button% [parent frame]
       [label "Help"]
       [callback
        (lambda (button event)
          (eval `(help ,next))
          )]))
(define doneb
  (new button% [parent frame]
       [label "Done"]
       [callback
        (lambda (button event)
          (with-output-to-file database-path (lambda () (write database)) #:exists 'replace))]))

(if (next-sym!)
    (send frame show #t)
    (local [(define ht (make-hasheq))]
      (for ([(k v) (in-hash database)])
        (hash-update! ht v add1 0))
      (for ([(k v) (in-hash ht)])
        (printf "~a ~a~n" k v))))

