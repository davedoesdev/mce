(module expand (main main))

(define (main argv)
    (write (cdr (expand `((

(define-syntax define
  (syntax-rules ()
    ((define (f params ...) body ...)
     (define f (lambda (params ...) body ...)))
    ((define (f params . rest) body ...)
     (define f (lambda (params . rest) body ...)))
    ((define (f . args) body ...)
     (define f (lambda args body ...)))
    ((define var val ...)
     (mce-define var val ...))))

(define-syntax call/cc
  (syntax-rules ()
    ((call/cc f)
     (let/cc k (f k)))))

(define-syntax call-with-current-continuation
  (syntax-rules ()
    ((call-with-current-continuation f)
     (call/cc f))))

; Simple version of values and call-with-values for the common case

(define-syntax values
  (syntax-rules ()
    ((values v ...)
     (list v ...))))

(define-syntax call-with-values
  (syntax-rules ()
    ((call-with-values producer consumer)
     (apply consumer (producer)))))

; R5RS Section 7.3
; https://schemers.org/Documents/Standards/R5RS/HTML/

(define-syntax and
  (syntax-rules ()
    ((and) #t)
    ((and test) test)
    ((and test1 test2 ...)
     (if test1 (and test2 ...) #f))))

(define-syntax or
  (syntax-rules ()
    ((or) #f)
    ((or test) test)
    ((or test1 test2 ...)
     (let ((x test1))
       (if x x (or test2 ...))))))

(define-syntax case
  (syntax-rules (else)
    ((case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (case atom-key clauses ...)))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))))

; R6RS Appendix B
; http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-16.html#node_chap_B

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

(define-syntax letrec-helper
  (syntax-rules ()
    ((letrec-helper
       ()
       (temp ...)
       ((var init) ...)
       body1 body2 ...)
     (let ((var 'undefined) ...)
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
     (let ((var1 'undefined) ...)
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

(define-syntax let*-values
  (syntax-rules ()
    ((let*-values () body1 body2 ...)
     (let () body1 body2 ...))
    ((let*-values (binding1 binding2 ...)
       body1 body2 ...)
     (let-values (binding1)
       (let*-values (binding2 ...)
         body1 body2 ...)))))

(define-syntax toplevel
  (syntax-rules ()
    ((toplevel (f params ...) body ...)
     (f (lambda (params ...) body ...)))
    ((toplevel (f params . rest) body ...)
     (f (lambda (params . rest) body ...)))
    ((toplevel (f . args) body ...)
     (f (lambda args body ...)))
    ((toplevel var val ...)
     (var (begin val ...)))))
) (
(toplevel (list . args) args)

(toplevel (cons* . args)
    (if (null? (cdr args))
        (car args)
        (cons (car args) (apply cons* (cdr args)))))

(toplevel eqv? eq?)

(toplevel (equal? x y)
    (cond ((eqv? x y) #t)
          ((and (string? x) (string? y)) (string=? x y))
          ((and (pair? x) (pair? y))
           (and (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
          ((and (vector? x) (vector? y))
           (let ((len (vector-length x)))
               (if (= (vector-length y) len)
                   (let loop ((i 0))
                       (cond ((= i len) #t)
                             ((equal? (vector-ref x i) (vector-ref y i))
                              (loop (+ i 1)))
                             (else #f)))
                   #f)))
          (else #f)))

(toplevel (memv o l)
    (cond ((null? l) #f)
          ((eqv? (car l) o) #t)
          (else (memv o (cdr l)))))
) 
,(read))))))
