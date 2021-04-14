(module expand
    (main main))

(define (main argv)
    (bigloo-warning-set! 0)

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

(define-syntax not
  (syntax-rules ()
    ((not v)
     (if v #f #t))))

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
     (var initialize (begin val ...)))))

(define-syntax toplevel-helper
  (syntax-rules ()
    ((toplevel-helper (f params ...) body ...)
     (f helper (lambda (params ...) body ...)))
    ((toplevel-helper (f params . rest) body ...)
     (f helper (lambda (params . rest) body ...)))
    ((toplevel-helper (f . args) body ...)
     (f helper (lambda args body ...)))
    ((toplevel-helper var val ...)
     (var helper initialize (begin val ...)))))
) (
(toplevel (list . args) args)

(toplevel (null? v)
    (and (vector? v)
         (= (vector-length v) 0)))

(toplevel (pair? v)
    (and (vector? v)
         (= (vector-length v) 2)))

(toplevel (car v) (vector-ref v 0))

(toplevel (cdr v) (vector-ref v 1))

(toplevel (set-car! v x) (vector-set! v 0 x))

(toplevel (set-cdr! v y) (vector-set! v 1 y))

(toplevel (length l)
    (let loop ((i 0) (l l))
        (if (null? l)
            i
            (loop (+ i 1) (cdr l)))))

(toplevel (vector . els)
    (let* ((len (length els))
           (v (make-vector len)))
        (let loop ((i 0) (els els))
            (if (= i len)
                v
                (begin (vector-set! v i (car els))
                       (loop (+ i 1) (cdr els)))))))

(toplevel (cons x y)
    ;(vector x y))
    (let ((v (make-vector 2)))
        (vector-set! v 0 x)
        (vector-set! v 1 y)
        v))

(toplevel (cons* . args)
    (if (null? (cdr args))
        (car args)
        (cons (car args) (apply cons* (cdr args)))))

(toplevel (list-ref l i)
    (if (= i 0)
        (car l)
        (list-ref (cdr l) (- i 1))))

(toplevel (binary=? x y)
    (let ((len (binary-length x)))
        (if (= (binary-length y) len)
            (let loop ((i 0))
                (cond ((= i len) #t)
                      ((= (binary-ref x i) (binary-ref y i))
                       (loop (+ i 1)))
                      (else #f)))
            #f)))

(toplevel (eq? x y)
    (cond ((or (and (char? x) (char? y))
               (and (symbol? x) (symbol? y)))
           (binary=? x y))
          ((and (boolean? x) (boolean? y))
           (or (and x y) (and (not x) (not y))))
          ((and (number? x) (number? y))
           (= x y))
          ((and (null? x) (null? y))
           #t)
          ((and (string? x) (string? y))
           #f)
          (else
           (same-object? x y))))

(toplevel eqv? eq?)

(toplevel (equal? x y)
    (cond ((eqv? x y) #t)
          ((and (string? x) (string? y))
           (binary=? x y))
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

(toplevel (append l1 l2)
    (if (null? l1)
        l2
        (cons (car l1) (append (cdr l1) l2))))

(toplevel (assoc k alist)
    (if (null? alist)
        #f
        (if (equal? (car (car alist)) k)
            (car alist)
            (assoc k (cdr alist)))))

(toplevel (abs x)
    (if (< x 0) (- x) x))

(toplevel (modulo x y)
    (let ((d (floor (/ x y))))
        (- x (* y d))))

(toplevel (<= x y)
    (or (< x y) (= x y)))

(toplevel (>= x y)
    (or (> x y) (= x y)))

(toplevel (char->integer c)
    (binary-ref c 1))

(toplevel-helper char-code (char->integer #\B))
(toplevel-helper string-code (char->integer #\C))
(toplevel-helper symbol-code (char->integer #\D))

(toplevel (char? x)
    (and (binary? x) (= (binary-length x) 2) (= (binary-ref x 0) char-code)))

(toplevel (string? x)
    (and (binary? x) (> (binary-length x) 0) (= (binary-ref x 0) string-code)))

(toplevel (symbol? x)
    (and (binary? x) (> (binary-length x) 0) (= (binary-ref x 0) symbol-code)))

(toplevel (binary-copy b . args)
    (let* ((numargs (length args))
           (start (if (> numargs 0) (list-ref args 0) 0))
           (end (if (> numargs 1) (list-ref args 1) (binary-length b)))
           (len (- end start))
           (r (make-binary len)))
        (let loop ((i 0))
            (if (< i len)
                (begin (binary-set! r i (binary-ref b (+ start i)))
                       (loop (+ i 1)))))
        r))

(toplevel (for-each f l)
    (if (not (null? l))
        (begin (f (car l)) (for-each f (cdr l)))))

(toplevel (repeat f n)
    (if (not (= n 0))
        (begin (f) (repeat f (- n 1)))))

(toplevel-helper (make-port name output)
    (lambda (x . args)
        (cond ((and (binary? x) (not (null? args)))
               (apply output (cons x args)))
              ((char? x)
               (output x 1 2))
              ((or (symbol? x) (string? x))
               (output x 1 (binary-length x)))
              ((binary? x)
               (apply output (cons x args)))
              (else
               (error name "unknown expression" x)))))

(toplevel-helper output-port (make-port "current-output-port" output-binary-to-stdout))
(toplevel-helper error-port (make-port "current-error-port" output-binary-to-stderr))

(toplevel (current-output-port) output-port)
(toplevel (current-error-port) error-port)

(toplevel-helper hexchars "0123456789abcdef")

(toplevel-helper (byte->binhex b)
    (let ((bin (make-binary 2)))
        (binary-set! bin 1 (binary-ref hexchars (+ 1 (modulo b 16))))
        (binary-set! bin 0 (binary-ref hexchars (+ 1 (floor (/ b 16)))))
        bin))

(toplevel-helper space-code (char->integer #\space))
(toplevel-helper tilde-code (char->integer #\~))
(toplevel-helper backslash-code (char->integer #\\))

(toplevel-helper (printable? i)
    (and (>= i space-code) (<= i tilde-code)))

(toplevel-helper (write-binary b port start wrap-char)
    (let ((wrap-code (char->integer wrap-char))
          (end (binary-length b)))
        (port wrap-char)
        (let loop ((i start) (prev start))
            (if (< i end)
                (let ((c (binary-ref b i)))
                    (cond ((or (= c wrap-code) (= c backslash-code))
                           (if (< prev i)
                               (port b prev i))
                           (port #\\)
                           (port b i (+ i 1))
                           (let ((next (+ i 1)))
                               (loop next next)))
                          ((printable? c)
                           (loop (+ i 1) prev))
                          (else
                           (if (< prev i)
                               (port b prev i))
                           (port #\x)
                           (port (byte->binhex c) 0 2)
                           (let ((next (+ i 1)))
                               (loop next next)))))
                (if (< prev end)
                    (port b prev end))))
        (port wrap-char)))

(toplevel (display-binary b . args)
    (let* ((args-len (length args))
           (port (if (> args-len 0) (list-ref args 0) (current-output-port)))
           (start (if (> args-len 1) (list-ref args 1) 0))
           (end (if (> args-len 2) (list-ref args 2) (binary-length b))))
        (port b start end)))

(toplevel-helper (display-aux x port is-write)
    (cond ((null? x)
           (port "()"))
          ((boolean? x)
           (port (if x "#t" "#f")))
          ((number? x)
           (if (< x 0)
               (port #\-))
           (let* ((ax (abs x))
                  (whole (floor ax))
                  (p '()))
               (let loop ((n whole))
                   (let ((digit (modulo n 10))
                         (b (make-binary 1)))
                       (binary-set! b 0 (+ (char->integer #\0) digit))
                       (set! p (cons b p))
                       (let ((next (floor (/ n 10))))
                           (if (= next 0)
                               (for-each (lambda (b)
                                             (port b 0 1))
                                         p)
                               (loop next)))))
                (let* ((digit (modulo (floor ax) 10))
                       (n (- ax (- (floor ax) digit))))
                    (if (> n digit)
                        (begin (port #\.)
                               (let loop ((i 0) (f (* n 10)) (zeros 0))
                                   (let* ((digit (modulo (floor f) 10))
                                          (n (- f (- (floor f) digit))))
                                       (if (= digit 0)
                                           (set! zeros (+ zeros 1))
                                           (let ((b (make-binary 1)))
                                               (repeat (lambda () (port #\0)) zeros)
                                               (set! zeros 0)
                                               (binary-set! b 0 (+ (char->integer #\0) digit))
                                               (port b 0 1)))
                                       (if (and (< i 6) (> n digit))
                                           (loop (+ i 1) (* n 10) zeros)))))))))
          ((char? x)
           (if is-write
               (let ((c (char->integer x)))
                   (port #\\)
                   (if (printable? c)
                       (port x)
                       (begin (port "x" (port (byte->binhex c) 0 2)))))
               (port x)))
          ((string? x)
           (if is-write
               (write-binary x port 1 #\")
               (port x)))
          ((symbol? x)
           (if is-write
               (write-binary x port 1 #\|)
               (port x)))
          ((pair? x)
           (port #\()
           (let loop ((x x))
               (display-aux (car x) port is-write)
               (let ((y (cdr x)))
                   (if (pair? y)
                       (begin (port #\space)
                              (loop y))
                       (if (not (null? y))
                           (begin (port " . ")
                                  (display-aux y port is-write))))))
           (port #\)))
          ((vector? x)
           (port "#(")
           (let loop ((i 0))
               (if (< i (vector-length x))
                   (begin (if (> i 0)
                              (port #\space))
                          (display-aux (vector-ref x i) port is-write)
                          (loop (+ i 1)))))
           (port #\)))
          ((procedure? x)
           (port "#<procedure>"))
          (else
           (error "display-aux" "unknown expression" x))))

(toplevel-helper (port-from-args args)
    (if (null? args) (current-output-port) (car args)))

(toplevel (display x . args)
    (display-aux x (port-from-args args) #f))

(toplevel (newline . args)
    ((port-from-args args) #\newline))

(toplevel-helper (print-aux args port)
    (for-each (lambda (x) (display-aux x port #f)) args)
    (newline port))

(toplevel (print . args)
    (print-aux args (current-output-port)))

(toplevel (eprint . args)
    (print-aux args (current-error-port)))

(toplevel (write x . args)
    (display-aux x (port-from-args args) #t))
) 
,(read))))))
