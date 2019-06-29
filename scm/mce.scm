(module mce
    (library packrat)
    (export 
        (make-runtime)
        (get-config runtime k)
        (set-config! runtime k v)
        (get-global-function runtime name)
        (register-global-function! runtime name f)
        (unregister-global-function! runtime name)
        (register-kenv-function! runtime f)
        (start-stream runtime stream #!key (is_scan #f) (args '()))
        (start-string runtime s #!key (is_scan #f) (args '()))
        (start argv)
        (send runtime k v)))

(define-struct runtime-ops
    get-config
    set-config!
    get-global-function
    register-global-function!
    unregister-global-function!
    register-kenv-function!
    start-stream
    start-string
    start
    send)

(define (make-runtime)

(define (make-eq-table)
    (vector assq '()))

(define (make-equal-table)
    (vector assoc '()))

(define (table-ref table key)
    ((vector-ref table 0) key (vector-ref table 1)))

(define (ref-value ref)
    (if ref (cdr ref) ref))

(define (table-set! table key val)
    (vector-set! table 1 (cons (cons key val) (vector-ref table 1)))
    val)

(define (table-delete! table key)
    (let ((r (table-ref table key)))
        (if r (remq! r (vector-ref table 1)))
        r))

(define (make-global-ctenv) (list '()))

(define (extend-ctenv ctenv syms)
    (cons syms ctenv))

(define (ctenv-index ctenv sym)
    (let loop1 ((i1 0) (e1 ctenv))
        (if (null? e1)
            #f
            (let loop2 ((i2 0) (e2 (car e1)))
                (if (null? e2)
                    (loop1 (+ i1 1) (cdr e1))
                    (if (equal? (car e2) sym)
                        (cons i1 i2)
                        (loop2 (+ i2 1) (cdr e2))))))))

(define (putin-ctenv! ctenv sym)
    (let ((i (ctenv-index ctenv sym)))
        (if i
            i
            (let ((e (car ctenv)))
                (set-car! ctenv (append e (list sym)))
                (cons 0 (length e))))))

(define (ctenv-lookup i env)
    (let* ((senv (list-ref env (inexact->exact (car i))))
           (vals (cdr senv))
           (index (inexact->exact (cdr i))))
        (if (< index (vector-length vals))
            (vector-ref vals index)
            '())))

(define (extend-vector vec index val)
    (let ((len (vector-length vec))
          (newvec (make-vector (+ index 1))))
        (let loop ((i 0))
            (if (= i index)
                (vector-set! newvec i val)
                (begin (vector-set! newvec i
                           (if (< i len) (vector-ref vec i) '()))
                       (loop (+ i 1)))))
        newvec))

(define (extend-list l index val)
    (let ((newl (make-list (+ index 1))))
        (let loop ((src l) (dst newl))
            (if (null? src)
                (set-car! dst val)
                (begin (set-car! dst (car src))
                       (loop (cdr src) (cdr dst)))))
        newl))

(define (ctenv-setvar! name i val env)
    (let* ((senv (list-ref env (inexact->exact (car i))))
           (syms (car senv))
           (vals (cdr senv))
           (index (inexact->exact (cdr i))))
        (if (< index (vector-length vals))
            (vector-set! vals index val)
            (set-cdr! senv (extend-vector vals index val)))
        (if (< index (length syms))
            (list-set! syms index name)
            (set-car! senv (extend-list syms index name))))
    val)

(define (extend-env env syms values)
    (cons (cons syms (list->vector values)) env))

(define (improper-extend-env env syms values)
    (let loop ((syms syms) (values values) (done-syms '()) (done-values '()))
        (if (symbol? syms)
            (cons (cons (reverse (cons syms done-syms))
                        (list->vector (reverse (cons values done-values))))
                  env)
            (if (and (not (null? syms)) (not (null? values)))
                (loop (cdr syms)
                      (cdr values)
                      (cons (car syms) done-syms)
                      (cons (car values) done-values))
                (cons (cons (reverse done-syms)
                            (list->vector (reverse done-values)))
                      env)))))

(define (make-step-contn k args) (cons 'MCE-STEP-CONTN (cons k args)))
(define (step-contn? exp) (and (pair? exp) (equal? (car exp) 'MCE-STEP-CONTN)))
(define (step-contn-k exp) (cadr exp))
(define (step-contn-args exp) (cddr exp))

(define send cons)

(define (make-global-env) (list (cons '() '#())))

(define (make-env-args env args)
    (cons 'MCE-ENV-ARGS (cons env args)))

(define (env-args? exp) (and (pair? exp) (equal? (car exp) 'MCE-ENV-ARGS)))

(define (env-args-env env-args)
    (if (env-args? env-args) (cadr env-args) (make-global-env)))

(define (env-args-args env-args)
    (if (env-args? env-args) (cddr env-args) env-args))

(define (yield-defn? args)
    (and (not (null? args)) (equal? (car args) 'MCE-YIELD-DEFINITION)))

(define (get-procedure-defn proc)
    (apply proc '(MCE-YIELD-DEFINITION)))

(define (memoize-lambda proc defn)
    (lambda args
        (if (yield-defn? args)
            (if (procedure? defn) (defn) defn)
            (apply proc args))))

(define (improper? l)
    (cond ((null? l) #f)
          ((pair? l) (improper? (cdr l)))
          (else #t)))

(define (->proper l)
    (let loop ((todo l) (done '()))
        (if (null? todo)
            (reverse done)
            (if (symbol? todo)
                (reverse (cons todo done))
                (loop (cdr todo) (cons (car todo) done))))))

(define forms '#())

(define-syntax define-form
    (syntax-rules ()
        ((define-form name body ...)
         (begin
            (define name -1)
            (set! name (vector-length forms))
            (set! forms (extend-vector forms (vector-length forms) body ...))))))

(define-form symbol-lookup
    (lambda (this i)
        (lambda (k env)
            (send k (ctenv-lookup i env)))))

(define-form send-value
    (lambda (this exp)
        (lambda (k env)
            (send k exp))))

(define-form constructed-function
    (lambda (this args cf)
        (let ((r (apply cf args)))
            (if (procedure? r)
                (wrap-global-lambda r this)
                r))))

(define-form global-lambda
    (lambda (this defn)
        (wrap-global-lambda (find-global defn) this)))

(define-form if0
    (lambda (this scan0 scan1 scan2)
        (lambda (k env)
            (scan0 (make-form if1 k env scan1 scan2) env))))

(define-form if1
    (lambda (this k env scan1 scan2)
        (lambda (v)
            (if v
                (scan1 k env)
                (scan2 k env)))))

(define-form sclis0
    (lambda (this first rest)
        (lambda (k env)
            (first (make-form sclis1 k env rest) env))))

(define-form sclis1
    (lambda (this k env rest)
        (lambda (v)
            (rest (make-form sclis2 k v) env))))

(define-form sclis2
    (lambda (this k v)
        (lambda (w)
            (send k (cons v w)))))

(define-form scseq0
    (lambda (this first rest)
        (lambda (k env)
            (first (make-form scseq1 k env rest) env))))

(define-form scseq1
    (lambda (this k env rest)
        (lambda (v) (rest k env))))

(define-form lambda0
    (lambda (this params scanned)
        (lambda (k env)
            (send k (make-form lambda1 params scanned env)))))

(define-form lambda1
    (lambda (this params scanned env)
        (lambda args
            (handle-lambda args params scanned env extend-env))))

(define-form improper-lambda0
    (lambda (this params scanned)
        (lambda (k env)
            (send k (make-form improper-lambda1 params scanned env)))))

(define-form improper-lambda1
    (lambda (this params scanned env)
        (lambda args
            (handle-lambda args params scanned env improper-extend-env))))

(define-form let/cc0
    (lambda (this name scanned)
        (lambda (k env)
            (scanned k (extend-env env
                                   (list name)
                                   (list (make-form let/cc1 k)))))))

(define-form let/cc1
    (lambda (this k)
        (lambda args
            (handle-contn-lambda args k))))

(define-form define0
    (lambda (this name i scanned)
        (lambda (k env)
            (scanned (make-form define1 k env name i) env))))

(define-form define1
    (lambda (this k env name i)
        (lambda (v)
            (send k (ctenv-setvar! name i v env)))))

(define-form application0
    (lambda (this scanned)
        (lambda (k env)
            (scanned (make-form application1 k env) env))))

(define-form application1
    (lambda (this k env)
        (lambda (v)
            (applyx k env (car v) (cdr v)))))

(define-form evalx-initial
    (lambda (this k env scanned)
        (lambda (v)
            (scanned k env))))

(define (make-form n . args)
    (letrec*
        ((defn (cons n args))
         (f2 (memoize-lambda (lambda args (apply f args)) defn))
         (f (memoize-lambda (apply (vector-ref forms (inexact->exact n))
                                   (cons f2 args))
                            defn)))
        f))

(define (sclis exp ctenv fixups)
    (if (null? exp)
        (make-form send-value '())
        (make-form sclis0
                   (scan-aux (car exp) ctenv fixups)
                   (sclis (cdr exp) ctenv fixups))))

(define (scseq exp ctenv fixups)
    (if (null? exp)
        (make-form send-value '())
        (let ((first (scan-aux (car exp) ctenv fixups)))
            (if (null? (cdr exp))
                first
                (make-form scseq0 first (scseq (cdr exp) ctenv fixups))))))

(define (error-lookup-not-found name)
    (error "lookup" "symbol not found" name))

(define (error-set!-not-found name)
    (error "set!" "symbol not found" name))

(define (make-undefined-lookup name ctenv fixups)
    (let ((f '()))
        (define (fixup)
            (let ((i (ctenv-index ctenv name)))
                (if i
                    (begin (set! f (make-form symbol-lookup i)) '())
                    name)))
        (set-car! fixups (cons fixup (car fixups)))
        (lambda args (apply f args))))

(define (make-undefined-set! name scanned ctenv fixups)
    (let ((f '()))
        (define (fixup)
            (let ((i (ctenv-index ctenv name)))
                (if i
                    (begin (set! f (make-form define0 name i scanned)) '())
                    (error-set!-not-found name))))
        (set-car! fixups (cons fixup (car fixups)))
        (lambda args (apply f args))))

(define (scan-aux exp ctenv fixups)
    (cond ((symbol? exp)
           (let ((i (ctenv-index ctenv exp)))
               (if i
                   (make-form symbol-lookup i)
                   (let ((g (lookup-global exp #f)))
                       (if (procedure? g)
                           (make-form send-value g)
                           (make-undefined-lookup exp ctenv fixups))))))
          ((pair? exp)
           (case (car exp)
               ((quote)
                (make-form send-value (cadr exp)))
               ((if)
                (let ((scan0 (scan-aux (cadr exp) ctenv fixups))
                      (scan1 (scan-aux (caddr exp) ctenv fixups))
                      (scan2 (scan-aux (if (pair? (cdddr exp)) (cadddr exp) '())
                                       ctenv
                                       fixups)))
                    (make-form if0 scan0 scan1 scan2)))
               ((lambda)
                (let ((params (cadr exp)))
                    (if (improper? params)
                        (let ((scanned
                               (scseq (cddr exp)
                                      (extend-ctenv ctenv (->proper params))
                                      fixups)))
                            (make-form improper-lambda0 params scanned))
                        (let ((scanned
                               (scseq (cddr exp)
                                      (extend-ctenv ctenv params)
                                      fixups)))
                            (make-form lambda0 params scanned)))))
               ((let/cc)
                (let* ((name (cadr exp))
                       (scanned (scseq (cddr exp)
                                       (extend-ctenv ctenv (list name))
                                       fixups)))
                    (make-form let/cc0 name scanned)))
               ((set!)
                (let* ((name (cadr exp))
                       (i (ctenv-index ctenv name))
                       (scanned (scseq (cddr exp) ctenv fixups)))
                    (if i
                        (make-form define0 name i scanned)
                        (make-undefined-set! name scanned ctenv fixups))))
               ((mce-define)
                (let* ((name (cadr exp))
                       (i (putin-ctenv! ctenv name))
                       (scanned (scseq (cddr exp) ctenv fixups)))
                    (make-form define0 name i scanned)))
               ((begin)
                (scseq (cdr exp) ctenv fixups))
               (else
                (let ((scanned (sclis exp ctenv fixups)))
                    (make-form application0 scanned)))))
          (else
           (make-form send-value exp))))

(define (scan exp global-ctenv)
    (let* ((toplevels (car exp))
           (fixups (list '()))
           (r (scan-aux (cadr exp) global-ctenv fixups)))
        (let loop ((fixups (car fixups)))
            (if (not (null? fixups))
                (let* ((fixup (car fixups))
                       (name (fixup)))
                    (if (symbol? name)
                        (let ((toplevel (assoc name toplevels)))
                            (if (not toplevel)
                                (error-lookup-not-found name))
                            (let* ((i (putin-ctenv! global-ctenv name))
                                   (tlexp (list toplevels (cadr toplevel)))
                                   (scanned (scan tlexp global-ctenv))
                                   (form1 (make-form define0 name i scanned))
                                   (form2 (make-form scseq0 form1 r)))
                                (if (symbol? (fixup))
                                    (error-lookup-not-found name))
                                (set! r form2))))
                    (loop (cdr fixups)))))
        r))

(define (step state)
    ((car state) (cdr state)))

(define (result v)
    (vector 'MCE-RESULT v))

(define (result? exp)
    (and (vector? exp)
         (= (vector-length exp) 2)
         (equal? (vector-ref exp 0) 'MCE-RESULT)))

(define (result-val exp)
    (vector-ref exp 1))

(define (run state)
    (let loop ((state state))
        (if (result? state)
            (result-val state)
            (loop (step state)))))

(define (evalx k exp env)
    (let ((scanned (scan exp (make-global-ctenv))))
        (send (make-form evalx-initial k env scanned) '())))

(define (applyx k env fn args)
    (apply fn (make-step-contn k (make-env-args env args))))

(define (handle-lambda args params fn env extend-env)
    (if (step-contn? args)
        (let ((sca (step-contn-args args)))
            (fn (step-contn-k args)
                (extend-env env params (env-args-args sca))))
        (run (fn (lookup-global 'result)
                 (extend-env env params (env-args-args args))))))

(define (handle-contn-lambda args k)
    (cond ((transfer? args)
           (apply k (env-args-args (transfer-args args))))
          ((step-contn? args)
           (apply k (env-args-args (step-contn-args args))))
          (else
           (run (apply k (env-args-args args))))))

(define global-table (make-equal-table))

(define (globalize x args cf)
    (if (procedure? x)
        (letrec* ((defn (list constructed-function args cf))
                  (f2 (memoize-lambda (lambda args (apply f args)) defn))
                  (f (memoize-lambda (wrap-global-lambda x f2) defn)))
            f)
        x))

(define (handle-global-lambda args fn cf)
    (cond ((transfer? args)
           (apply fn args))
          ((step-contn? args)
           (let* ((sck (step-contn-k args))
                  (sca (step-contn-args args))
                  (eaa (env-args-args sca)))
               (send sck (globalize (apply fn eaa) eaa cf))))
          (else
           (let ((eaa (env-args-args args)))
            (globalize (apply fn eaa) eaa cf)))))

(define (handle-global-lambda-kenv args fn)
    (if (step-contn? args)
        (let ((sca (step-contn-args args)))
            (apply fn (cons (step-contn-k args)
                            (cons (env-args-env sca)
                                  (env-args-args sca)))))
        (run (apply fn (cons (lookup-global 'result)
                             (cons (env-args-env args)
                                   (env-args-args args)))))))

(define (wrap-global-lambda fn cf)
    (if (table-ref kenvfn-table fn)
        (lambda args (handle-global-lambda-kenv args fn))
        (lambda args (handle-global-lambda args fn cf))))

(define (transfer? exp) (and (pair? exp) (equal? (car exp) 'MCE-TRANSFER)))
(define (transfer-args exp) (cdr exp))

(define (transfer k env fn . args)
    (apply fn (cons 'MCE-TRANSFER args)))

(define (find-global sym #!optional (err #t))
    (let ((r (table-ref global-table sym)))
        (if r
            (ref-value r)
            (if err
                (error-lookup-not-found sym)
                r))))

(define (lookup-global sym #!optional (err #t))
    (let ((r (find-global sym err)))
        (if (procedure? r)
            (letrec*
                ((defn (list global-lambda sym))
                 (f2 (memoize-lambda (lambda args (apply f args)) defn))
                 (f (memoize-lambda (wrap-global-lambda r f2) defn)))
                f)
            r)))

(define (print . args)
    (for-each display args)
    (newline))

(define (eprint . args)
    (for-each (lambda (v) (display v (current-error-port))) args)
    (newline (current-error-port)))

(table-set! global-table 'result result)
(table-set! global-table 'print print)
(table-set! global-table 'eprint eprint)
(table-set! global-table '< <)
(table-set! global-table '> >)
(table-set! global-table '+ +)
(table-set! global-table '- -)
(table-set! global-table '* *)
(table-set! global-table '/ /)
(table-set! global-table 'eq? eq?)
(table-set! global-table '= =)
(table-set! global-table 'not not)
(table-set! global-table 'procedure? procedure?)
(table-set! global-table 'save mce-save)
(table-set! global-table 'restore mce-restore)
(table-set! global-table 'write write)
(table-set! global-table 'newline newline)
(table-set! global-table 'transfer transfer) 
(table-set! global-table 'car car)
(table-set! global-table 'cdr cdr)
(table-set! global-table 'set-car! set-car!)
(table-set! global-table 'set-cdr! set-cdr!)
(table-set! global-table 'apply applyx)
(table-set! global-table 'unmemoize unmemoize)
(table-set! global-table 'serialize serialize)
(table-set! global-table 'getpid getpid)
(table-set! global-table 'null? null?)
(table-set! global-table 'string? string?)
(table-set! global-table 'pair? pair?)
(table-set! global-table 'string=? string=?)
(table-set! global-table 'vector? vector?)
(table-set! global-table 'vector-length vector-length)
(table-set! global-table 'vector-ref vector-ref)
(table-set! global-table 'cons cons)
(table-set! global-table 'list->vector list->vector)
(table-set! global-table 'get-config get-config)

(define (get-global-function name)
    (ref-value (table-ref global-table name)))

(define (register-global-function! name f)
    (table-set! global-table name f))

(define (unregister-global-function! name)
    (table-delete! global-table name))

(define kenvfn-table (make-eq-table))

(table-set! kenvfn-table transfer #t)
(table-set! kenvfn-table applyx #t)

(define (register-kenv-function! f)
    (table-set! kenvfn-table f #t))

(define config-table (make-equal-table))

(define (set-config! k v)
    (table-set! config-table k v))

(define (get-config k)
    (ref-value (table-ref config-table k)))

(define (cmap f l tab set-entry!)
   (let ((ref (table-ref tab l)))
       (if ref
           (ref-value ref)
           (let ((entry (set-entry! tab l (cons '() '()))))
               (set-car! entry (f (car l)))
               (set-cdr! entry (f (cdr l)))
               entry))))

(define (vector-cmap f vec tab set-entry!)
    (let ((ref (table-ref tab vec)))
        (if ref
            (ref-value ref)
            (let* ((len (vector-length vec))
                   (entry (set-entry! tab vec (make-vector len))))
                (let loop ((i 0))
                    (if (= i len)
                        entry
                        (begin (vector-set! entry i (f (vector-ref vec i)))
                               (loop (+ i 1)))))))))

(define (unmemoized? v)
    (and (= (vector-length v) 2)
         (equal? (vector-ref v 0) 'MCE-UNMEMOIZED)))

(define (unmemoized-repexp v)
    (vector-ref v 1))

(define (memoize-aux exp tab fn)
    (cond ((pair? exp)
           (cmap fn exp tab table-set!))
          ((vector? exp)
           (if (unmemoized? exp)
               (let ((ref (table-ref tab exp)))
                   (if ref
                       (ref-value ref)
                       (letrec*
                           ((repexp (unmemoized-repexp exp))
                            (entry (table-set! tab exp
                                (memoize-lambda (lambda args (apply f args))
                                                (lambda () r))))
                            (r (fn repexp))
                            (f (lambda args
                                   (set! f (apply make-form r))
                                   (apply f args))))
                           entry)))
               (vector-cmap fn exp tab table-set!)))
          (else exp)))

(define (memoize exp)
    (letrec ((tab (make-eq-table))
             (fn (lambda (x) (memoize-aux x tab fn))))
        (fn exp)))

(define (unmemoize-aux exp tab fn)
    (cond ((pair? exp)
           (cmap fn exp tab table-set!))
          ((vector? exp)
           (vector-cmap fn exp tab table-set!))
          ((procedure? exp)
           (let ((ref (table-ref tab exp)))
               (if ref
                   (ref-value ref)
                   (let ((entry (table-set! tab exp (make-vector 2))))
                       (vector-set! entry 0 'MCE-UNMEMOIZED)
                       (vector-set! entry 1 (fn (get-procedure-defn exp)))
                       entry))))
          (else exp)))

(define (unmemoize exp)
    (letrec ((tab (make-eq-table))
             (fn (lambda (x) (unmemoize-aux x tab fn))))
        (fn exp)))

(define (make-serialized n)
    (vector 'MCE-SERIALIZED n))

(define (serialized? v)
    (and (= (vector-length v) 2)
         (equal? (vector-ref v 0) 'MCE-SERIALIZED)))

(define (serialized-n v)
    (vector-ref v 1))

(define (serialize-aux exp tab fn set-entry!)
    (cond ((pair? exp) (cmap fn exp tab set-entry!))
          ((vector? exp) (vector-cmap fn exp tab set-entry!))
          (else exp)))

(define (serialize exp)
    (letrec ((counter 0)
             (tab (make-eq-table))
             (set-entry!
              (lambda (tab v entry)
                  (table-set! tab v (make-serialized counter))
                  (set! counter (+ counter 1))
                  entry))
             (fn (lambda (x) (serialize-aux x tab fn set-entry!))))
        (fn exp)))

(define (deserialize-aux exp tab fn set-entry!)
    (cond ((pair? exp) (cmap fn exp tab set-entry!))
          ((vector? exp)
           (if (serialized? exp)
               (ref-value (table-ref tab (inexact->exact (serialized-n exp))))
               (vector-cmap fn exp tab set-entry!)))
          (else exp)))

(define (deserialize exp)
    (letrec ((counter 0)
             (tab (make-eq-table))
             (set-entry!
              (lambda (tab v entry)
                  (table-set! tab counter entry)
                  (set! counter (+ counter 1))
                  entry))
             (fn (lambda (x) (deserialize-aux x tab fn set-entry!))))
        (fn exp)))

(define null-code    "a")
(define boolean-code "b")
(define number-code  "c")
(define char-code    "d")
(define string-code  "e")
(define symbol-code  "f")
(define pair-code    "g")
(define vector-code  "h")

(define (pickle-aux exp)
   (cond ((null? exp)
          (list null-code))
         ((boolean? exp)
          (list boolean-code (if exp "t" "f")))
         ((number? exp)
          (list number-code exp))
         ((char? exp)
          (list char-code (string exp)))
         ((string? exp)
          (list string-code exp))
         ((symbol? exp)
          (list symbol-code (symbol->string exp)))
         ((pair? exp)
          (list pair-code (pickle-aux (car exp)) (pickle-aux (cdr exp))))
         ((vector? exp)
          (list vector-code (pickle-aux (vector->list exp))))
         (else
          exp)))

(define (pickle exp)
    (let ((port (open-output-string)))
        (json-write (pickle-aux exp) port)
        (get-output-string port)))

(define (unpickle-aux exp)
    (let ((code (car exp)))
        (cond ((equal? code null-code)
               '())
              ((equal? code boolean-code)
               (if (equal? (cadr exp) "t") #t #f))
              ((equal? code number-code)
               (cadr exp))
              ((equal? code char-code)
               (string-ref (cadr exp) 0))
              ((equal? code string-code)
               (cadr exp))
              ((equal? code symbol-code)
               (string->symbol (cadr exp)))
              ((equal? code pair-code)
               (cons (unpickle-aux (cadr exp)) (unpickle-aux (caddr exp))))
              ((equal? code vector-code)
               (list->vector (unpickle-aux (cadr exp))))
              (else
               exp))))

(define (unpickle s)
    (unpickle-aux (json-read (open-input-string s))))

(define (mce-save exp)
    (pickle (serialize (unmemoize exp))))

(define (mce-restore s)
    (memoize (deserialize (unpickle s))))

(define (mce-eval exp)
    (evalx (lookup-global 'result) exp (make-global-env)))

(define (mce-run exp . env)
    (run (mce-eval exp)))

(define (start-parsed is_scan v args)
    (if is_scan
        (write (mce-save (mce-eval v)))
        (if (string? v)
            (let ((r (mce-restore v)))
                (if (procedure? r)
                    (apply r args)
                    (run r)))
            (mce-run v))))

(define (start-stream stream #!key (is_scan #f) (args '()))
    (start-parsed is_scan (read stream) args))

(define (start-string s #!key (is_scan #f) (args '()))
    (start-parsed is_scan s args))

(define (start argv)
    (let ((is_scan (string-suffix? "scan" (car argv)))
          (execute #t)
          (s ""))
        (args-parse (cdr argv)
            ((("-h" "--help"))
             (args-parse-usage #f)
             (set! execute #f))
            (("--run" ?state (help "CPS form or state to run"))
             (set! s state))
            (("--config" ?kv (help "Set configuration"))
             (let ((pos (string-char-index kv #\= )))
                 (set-config! (substring kv 0 pos) (substring kv (+ pos 1)))))
            (else
             (error "start" "Invalid argument " else)))
        (if execute
            (if (string-null? s)
                (start-stream (current-input-port) :is_scan is_scan)
                (start-parsed is_scan (json-read (open-input-string s)) '())))))

(let ((runtime-ops (make-runtime-ops)))
    (runtime-ops-get-config-set! runtime-ops get-config)
    (runtime-ops-set-config!-set! runtime-ops set-config!)
    (runtime-ops-get-global-function-set! runtime-ops get-global-function)
    (runtime-ops-register-global-function!-set! runtime-ops register-global-function!)
    (runtime-ops-unregister-global-function!-set! runtime-ops unregister-global-function!)
    (runtime-ops-register-kenv-function!-set! runtime-ops register-kenv-function!)
    (runtime-ops-start-stream-set! runtime-ops start-stream)
    (runtime-ops-start-string-set! runtime-ops start-string)
    (runtime-ops-start-set! runtime-ops start)
    (runtime-ops-send-set! runtime-ops send)
    runtime-ops)

)

(define (get-config runtime k)
    ((runtime-ops-get-config runtime) k))

(define (set-config! runtime k v)
    ((runtime-ops-set-config! runtime) k v))

(define (get-global-function runtime name)
    ((runtime-ops-get-global-function runtime) name))

(define (register-global-function! runtime name f)
    ((runtime-ops-register-global-function! runtime) name f))

(define (unregister-global-function! runtime name)
    ((runtime-ops-unregister-global-function! runtime) name))

(define (register-kenv-function! runtime f)
    ((runtime-ops-register-kenv-function! runtime) f))

(define (start-stream runtime stream #!key (is_scan #f) (args '()))
    ((runtime-ops-start-stream runtime) stream is_scan: is_scan args: args))

(define (start-string runtime s #!key (is_scan #f) (args '()))
    ((runtime-ops-start-string runtime) s is_scan: is_scan args: args))

(define (start argv)
    ((runtime-ops-start (make-runtime)) argv))

(define (send runtime k v)
    ((runtime-ops-send runtime) k v))
