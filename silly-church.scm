(import (nanopass))

(define (operator? x) (memq x '(+ - *)))
(define (variable? x) (and (symbol? x) (not (operator? x))))

(define-language
  Lsrc
  (terminals
    (number (n))
    (operator (op)))
  (Expr (e)
    n
    (op e0 e1)))

(define-pass ast-to-Lsrc : * (ast) -> Lsrc ()
  (parse : * (e) -> Expr ()
    (cond
      [(number? e) e]
      [(and (list? e) (= 3 (length e)))
       (let ([op (car e)] [e0 (parse (cadr e))] [e1 (parse (caddr e))])
       `(,op ,e0 ,e1))]))
  (parse ast))

(define-language
  L1
  (extends Lsrc)
  (terminals
    (- (number (n)))
    (+ (variable (v))))
  (Expr (e)
    (- n)
    (+ v)
    (+ (lambda (v) e))
    (+ (apply e0 e1))))

(define-pass encode-numbers : Lsrc (ast) -> L1 ()
  (Expr : Expr (e) -> Expr ()
    [,n (letrec ([go (lambda (n) (if (= n 0) `x `(apply f ,[go (- n 1)])))])
        `(lambda (f) (lambda (x) ,[go n])))]))

(define-language
  L2
  (extends L1)
  (Expr (e)
    (+ op)
    (- (op e0 e1))))

(define-pass curry-operators : L1 (ast) -> L2 ()
  (Expr : Expr (e) -> Expr ()
    [(,op ,e0 ,e1)
     `(apply (apply ,op ,[Expr e0]) ,[Expr e1])]))

(define-language
  L3
  (extends L2)
  (terminals
    (- (operator (op))))
  (Expr (e)
    (- op)))

(define-pass encode-operators : L2 (ast) -> L3 ()
  (definitions
    (with-output-language (L3 Expr)
      (define plus
        `(lambda (m)
           (lambda (n)
             (lambda (f)
               (lambda (x)
                 (apply (apply m f) (apply (apply n f) x)))))))
      (define pred
        `(lambda (n)
           (lambda (f)
             (lambda (x)
               (apply
                 (apply
                   (apply n (lambda (g)
                              (lambda (h)
                                (apply h (apply g f)))))
                   (lambda (u) x))
                 (lambda (u) u))))))
      (define minus
        `(apply
           (lambda (pred)
             (lambda (m)
               (lambda (n)
                 (apply (apply n pred) m))))
           ,pred))
      (define multiply
        `(lambda (m)
           (lambda (n)
             (lambda (f)
               (apply m (apply n f))))))))
  (Expr : Expr (e) -> Expr ()
    [,op
     (case op
       [+ plus]
       [- minus]
       [* multiply]
       [else (error 'encode-operators "unsupported operator" op)])]))

(define-pass output-scheme : L3 (ast) -> * ()
  (Expr : Expr (e) -> * ()
    [,v v]
    [(apply ,e0 ,e1) `(,[Expr e0] ,[Expr e1])]
    [(lambda (,v) ,e) `(lambda (,v) ,[Expr e])]))

(define (encode x)
  (output-scheme
    (encode-operators
      (curry-operators
        (encode-numbers
          (ast-to-Lsrc x))))))

(define (from-church n) ((n (lambda (m) (+ 1 m))) 0))

(define (repl)
  (display "silly-church> ")
  (let ([e (get-line (console-input-port))])
    (cond
      [(equal? e #!eof) (newline)]
      [else
        (with-exception-handler
          (lambda (ex) (display-condition ex) (newline) (repl))
          (lambda ()
            (let ([ast (with-input-from-string e read)])
              (display (from-church (eval (encode ast))))
              (newline)
              (repl))))])))
