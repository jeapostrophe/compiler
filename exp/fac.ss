(module fac "llvm.ss"
  (def fac
    (lambda (n)
      (if (zero? n)
          0
          (* n (fac (sub1 n))))))
  (def t1
    (lambda args args))
  (def t2
    (lambda (n . args) (cons n args)))
  (def t3
    (lambda (n1 n2) (cons n1 n2)))
  
  (fac 10))