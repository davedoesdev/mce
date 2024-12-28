(module mce
    (library packrat)
    (export 
        (make-runtime)
        (set-config! runtime k v)
        (get-global-function runtime name)
        (register-global-function! runtime name f)
        (unregister-global-function! runtime name)
        (register-kenv-function! runtime f)
        (start-stream runtime stream #!key (is_scan #f) (args '()))
        (start-string runtime s #!key (is_scan #f) (args '()))
        (start argv)
        (send runtime k . v)))

(define-struct runtime-ops
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
                        (vector i1 i2)
                        (loop2 (+ i2 1) (cdr e2))))))))

(define (putin-ctenv! ctenv sym)
    (let ((i (ctenv-index ctenv sym)))
        (if i
            i
            (let ((e (car ctenv)))
                (set-car! ctenv (append e (list sym)))
                (vector 0 (length e))))))

(define (vlist-ref vl i)
    (if (= i 0)
        (vector-ref vl 0)
        (vlist-ref (vector-ref vl 1) (- i 1))))

(define (rtenv-lookup i env)
    (let* ((senv (vlist-ref env (vector-ref i 0)))
           (index (vector-ref i 1)))
        (if (< index (vector-length senv))
            (vector-ref senv index)
            '#())))

(define (extend-vector vec index val)
    (let ((len (vector-length vec))
          (newvec (make-vector (+ index 1))))
        (let loop ((i 0))
            (if (= i index)
                (vector-set! newvec i val)
                (begin (vector-set! newvec i
                           (if (< i len) (vector-ref vec i) '#()))
                       (loop (+ i 1)))))
        newvec))

(define (vlist-set! vl i v)
    (if (= i 0)
        (vector-set! vl 0 v)
        (vlist-set! vl (- i 1) v)))

(define (rtenv-setvar! i val env)
    (let* ((sindex (vector-ref i 0))
           (senv (vlist-ref env sindex))
           (index (vector-ref i 1)))
        (if (< index (vector-length senv))
            (vector-set! senv index val)
            (vlist-set! env sindex (extend-vector senv index val))))
    val)

(define (list->vlist l)
    (let ((r '()) (pr '()))
        (let loop ((l l))
            (if (pair? l)
                (let ((nr (vector (car l) '#())))
                    (if (null? r)
                        (begin (set! pr nr) (set! r nr))
                        (begin (vector-set! pr 1 nr) (set! pr nr)))
                    (loop (cdr l)))
                (let ((v (if (null? l) '#() l)))
                    (if (null? r)
                        (set! r v)
                        (vector-set! pr 1 v))
                    r)))))

(define (vlist->list vl)
    (let ((r '()) (pr '()))
        (let loop ((vl vl))
            (if (and (vector? vl) (= (vector-length vl) 2))
                (let ((nr (cons (vector-ref vl 0) '())))
                    (if (null? r)
                        (begin (set! pr nr) (set! r nr))
                        (begin (set-cdr! pr nr) (set! pr nr)))
                    (loop (vector-ref vl 1)))
                (if (and (vector? vl) (= (vector-length vl) 0))
                    r
                    (if (null? r)
                        (list vl)
                        (begin (set-cdr! pr (list vl)) r)))))))

(define (applyvl f vl)
    (apply f (vlist->list vl)))

(define (extend-rtenv env len values)
    (vector (list->vector values) env))

(define (improper-extend-rtenv env len values)
    (let ((v (make-vector len '#())))
        (let loop ((i 0) (values values))
            (if (and (< i len) (not (null? values)))
                (if (= i (- len 1))
                    (vector-set! v i (list->vlist values))
                    (begin (vector-set! v i (car values))
                           (loop (+ i 1) (cdr values))))))
        (vector v env)))

(define mark-prefix (string-append marker-code "MCE-"))

(define (mark type)
    (string-append mark-prefix type))

(define (marked? s)
    (substring-at? s mark-prefix 0))

(define step-contn-mark (mark "STEP-CONTN"))

(define (make-step-contn k env args)
    (cons step-contn-mark (cons k (cons env args))))

(define (step-contn? exp)
    (and (pair? exp) (equal? (car exp) step-contn-mark)))

(define (step-contn-k exp)
    (cadr exp))

(define (step-contn-env exp)
    (caddr exp))

(define (step-contn-args exp)
    (cdddr exp))

(define (sendl k args)
    (vector k (list->vlist args)))

(define (send k . v)
    (sendl k v))

(define (make-global-rtenv) (vector '#() '#()))

(define yield-defn-mark (mark "YIELD-DEFINITION"))

(define (yield-defn? args)
    (and (not (null? args)) (equal? (car args) yield-defn-mark)))

(define (get-procedure-defn proc)
    (apply proc (list yield-defn-mark)))

(define (memoize-lambda proc defn)
    (lambda args
        (if (yield-defn? args)
            (if (procedure? defn) (defn) defn)
            (apply proc args))))

(define (->proper l)
    (let ((r '()) (pr '()))
        (let loop ((l l))
            (cond ((null? l) #f)
                  ((pair? l)
                   (let ((nr (cons (car l) '())))
                       (if (null? r)
                           (begin (set! pr nr) (set! r nr))
                           (begin (set-cdr! pr nr) (set! pr nr)))
                       (loop (cdr l))))
                  (else
                   (let ((nr (cons l '())))
                       (if (null? r)
                           (set! r nr)
                           (set-cdr! pr nr))
                       r))))))

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
            (send k (rtenv-lookup i env)))))

(define-form send-value
    (lambda (this exp)
        (lambda (k env)
            (send k exp))))

(define-form constructed-function0
    (lambda (this args cf)
        (lambda args2
            (applyx (make-form constructed-function1 args2)
                    (make-global-rtenv) cf args))))

(define-form constructed-function1
    (lambda (this args)
        (lambda (f)
            (sendl f args))))

(define-form global-lambda
    (lambda (this defn)
        (wrap-global-lambda (find-global defn) this)))

(define-form if0
    (lambda (this scan0 scan1 scan2)
        (lambda (k env)
            (send scan0 (make-form if1 k env scan1 scan2) env))))

(define-form if1
    (lambda (this k env scan1 scan2)
        (lambda (v)
            (if v
                (send scan1 k env)
                (send scan2 k env)))))

(define-form sclis0
    (lambda (this first rest)
        (lambda (k env)
            (send first (make-form sclis1 k env rest) env))))

(define-form sclis1
    (lambda (this k env rest)
        (lambda (v)
            (send rest (make-form sclis2 k v) env))))

(define-form sclis2
    (lambda (this k v)
        (lambda (w)
            (send k (vector v w)))))

(define-form scseq0
    (lambda (this first rest)
        (lambda (k env)
            (send first (make-form scseq1 k env rest) env))))

(define-form scseq1
    (lambda (this k env rest)
        (lambda (v) (send rest k env))))

(define-form lambda0
    (lambda (this len scanned)
        (lambda (k env)
            (send k (make-form lambda1 len scanned env)))))

(define-form lambda1
    (lambda (this len scanned env)
        (lambda args
            (handle-lambda args len scanned env extend-rtenv))))

(define-form improper-lambda0
    (lambda (this len scanned)
        (lambda (k env)
            (send k (make-form improper-lambda1 len scanned env)))))

(define-form improper-lambda1
    (lambda (this len scanned env)
        (lambda args
            (handle-lambda args len scanned env improper-extend-rtenv))))

(define-form let/cc0
    (lambda (this scanned)
        (lambda (k env)
            (send scanned k (extend-rtenv env 1 (list (make-form let/cc1 k)))))))

(define-form let/cc1
    (lambda (this k)
        (lambda args
            (handle-contn-lambda args k))))

(define-form define0
    (lambda (this i scanned)
        (lambda (k env)
            (send scanned (make-form define1 k env i) env))))

(define-form define1
    (lambda (this k env i)
        (lambda (v)
            (send k (rtenv-setvar! i v env)))))

(define-form application0
    (lambda (this scanned)
        (lambda (k env)
            (send scanned (make-form application1 k env) env))))

(define-form application1
    (lambda (this k env)
        (lambda (v)
            (applyx k env (vector-ref v 0) (vector-ref v 1)))))

(define-form evalx-initial
    (lambda (this k env scanned)
        (lambda (v)
            (send scanned k env))))

(define (make-form n . args)
    (letrec*
        ((defn (list->vlist (cons n args)))
         (f2 (memoize-lambda (lambda args (apply f args)) defn))
         (f (memoize-lambda (apply (vector-ref forms n)
                                   (cons f2 args))
                            defn)))
        f))

(define (sclis exp ctenv fixups)
    (if (null? exp)
        (make-form send-value '#())
        (make-form sclis0
                   (scan-aux (car exp) ctenv fixups)
                   (sclis (cdr exp) ctenv fixups))))

(define (scseq exp ctenv fixups)
    (if (null? exp)
        (make-form send-value '#())
        (let ((first (scan-aux (car exp) ctenv fixups)))
            (if (null? (cdr exp))
                first
                (make-form scseq0 first (scseq (cdr exp) ctenv fixups))))))

(define (error-lookup-not-found name)
    (error "lookup" "symbol not found" name))

(define (error-set!-not-found name)
    (error "set!" "symbol not found" name))

(define (error-core-global-not-found n)
    (error "lookup" "core global not found" n))

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
                    (begin (set! f (make-form define0 i scanned)) '())
                    (error-set!-not-found name))))
        (set-car! fixups (cons fixup (car fixups)))
        (lambda args (apply f args))))

(define (scan-aux exp ctenv fixups)
    (cond ((symbol? exp)
           (let ((i (ctenv-index ctenv exp)))
               (if i
                   (make-form symbol-lookup i)
                   (let ((g (lookup-global exp #f (cadr fixups))))
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
                      (scan2 (scan-aux (if (pair? (cdddr exp)) (cadddr exp) #f)
                                       ctenv
                                       fixups)))
                    (make-form if0 scan0 scan1 scan2)))
               ((lambda)
                (let* ((params (cadr exp))
                       (proper-params (->proper params)))
                    (if proper-params
                        (let ((scanned
                               (scseq (cddr exp)
                                      (extend-ctenv ctenv proper-params)
                                      fixups)))
                            (make-form improper-lambda0 (length proper-params) scanned))
                        (let ((scanned
                               (scseq (cddr exp)
                                      (extend-ctenv ctenv params)
                                      fixups)))
                            (make-form lambda0 (length params) scanned)))))
               ((let/cc)
                (let* ((name (cadr exp))
                       (scanned (scseq (cddr exp)
                                       (extend-ctenv ctenv (list name))
                                       fixups)))
                    (make-form let/cc0 scanned)))
               ((set!)
                (let* ((name (cadr exp))
                       (i (ctenv-index ctenv name))
                       (scanned (scseq (cddr exp) ctenv fixups)))
                    (if i
                        (make-form define0 i scanned)
                        (make-undefined-set! name scanned ctenv fixups))))
               ((mce-define)
                (let* ((name (cadr exp))
                       (i (putin-ctenv! ctenv name))
                       (scanned (scseq (cddr exp) ctenv fixups)))
                    (make-form define0 i scanned)))
               ((begin)
                (scseq (cdr exp) ctenv fixups))
               (else
                (let ((scanned (sclis exp ctenv fixups)))
                    (make-form application0 scanned)))))
          (else
           (make-form send-value exp))))

(define (scan exp global-ctenv)
    (let* ((toplevels (car exp))
           (fixups-holder (list '() #f))
           (r (scan-aux (cadr exp) global-ctenv fixups-holder))
           (f (lambda args (apply r args)))
           (fixups (car fixups-holder))
           (fixups-len (length fixups)))
        (let loop ((fixups fixups) (count 0))
            (if (not (null? fixups))
                (let* ((fixup (car fixups))
                       (name (fixup)))
                    (if (symbol? name)
                        (let ((entry (assoc name toplevels)))
                            (if (not entry)
                                (error-lookup-not-found name))
                            (let ((helper? (equal? (cadr entry) 'helper)))
                                (if (and helper? (< count fixups-len))
                                    (error-lookup-not-found name))
                                (let* ((i (putin-ctenv! global-ctenv name))
                                       (fixups2 (list '() #t))
                                       (toplevel (if helper? (cddr entry) (cdr entry)))
                                       (initialize? (equal? (car toplevel) 'initialize))
                                       (scanned (scan-aux (if initialize?
                                                              (cadr toplevel)
                                                              (car toplevel))
                                                          global-ctenv
                                                          fixups2))
                                       (form1 (make-form define0 i scanned))
                                       (form2 (make-form scseq0 form1 (if initialize? r f))))
                                    (if (symbol? (fixup))
                                        (error-lookup-not-found name))
                                    (set-cdr! fixups (append (cdr fixups) (car fixups2)))
                                    (if initialize?
                                        (set! r form2)
                                        (set! f form2))))))
                    (loop (cdr fixups) (+ count 1)))))
        f))

(define (step state)
    (applyvl (vector-ref state 0) (vector-ref state 1)))

(define result-mark (mark "RESULT"))

(define (result v)
    (vector result-mark v))

(define (result? exp)
    (and (vector? exp)
         (= (vector-length exp) 2)
         (equal? (vector-ref exp 0) result-mark)))

(define (result-val exp)
    (vector-ref exp 1))

(define (run state)
    (let loop ((state state))
        (if (result? state)
            (result-val state)
            (loop (step state)))))

(define (evalx k exp env)
    (let ((scanned (scan exp (make-global-ctenv))))
        (send (make-form evalx-initial k env scanned) '#())))

(define (applyx k env fn args)
    (sendl fn (make-step-contn k env args)))

(define (handle-lambda args len fn env extend-rtenv)
    (if (step-contn? args)
        (send fn (step-contn-k args)
                 (extend-rtenv env len (step-contn-args args)))
        (run (send fn (lookup-global 'result)
                      (extend-rtenv env len args)))))

(define (handle-contn-lambda args k)
    (cond ((transfer? args)
           (sendl k (transfer-args args)))
          ((step-contn? args)
           (sendl k (step-contn-args args)))
          (else
           (run (sendl k args)))))

(define global-table (make-equal-table))

(define (globalize x args cf)
    (if (procedure? x)
        (letrec* ((defn (list->vlist (list constructed-function0 (list->vlist args) cf)))
                  (f2 (memoize-lambda (lambda args (apply f args)) defn))
                  (f (memoize-lambda (wrap-global-lambda x f2) defn)))
            f)
        x))

(define (handle-global-lambda args fn cf)
    (cond ((transfer? args)
           (apply fn (transfer-args args)))
          ((step-contn? args)
           (let ((sck (step-contn-k args))
                 (sca (step-contn-args args)))
               (send sck (globalize (apply fn sca) sca cf))))
          (else
           (globalize (apply fn args) args cf))))

(define (handle-global-lambda-kenv args fn)
    (if (step-contn? args)
        (apply fn (cons (step-contn-k args)
                        (cons (step-contn-env args)
                              (step-contn-args args))))
        (run (apply fn (cons (lookup-global 'result)
                             (cons (make-global-rtenv)
                                   args))))))

(define (wrap-global-lambda fn cf)
    (if (table-ref kenvfn-table fn)
        (lambda args (handle-global-lambda-kenv args fn))
        (lambda args (handle-global-lambda args fn cf))))

(define transfer-mark (mark "TRANSFER"))

(define (transfer k env fn . args)
    (sendl fn (cons transfer-mark args)))

(define (transfer? exp)
    (and (pair? exp) (equal? (car exp) transfer-mark)))

(define (transfer-args exp)
    (cdr exp))

(define (find-global sym #!optional (err #t) (helpers #f))
    (if (number? sym)
        (let ((r (if (< sym (vector-length core-globals))
                     (vector-ref core-globals sym)
                     #f)))
            (if r
                r
                (if err
                    (error-core-global-not-found sym)
                    #f)))
        (begin (if (and (string? sym)
                        (not (equal? (substring sym 0 1) symbol-code)))
                   (error-lookup-not-found sym))
               (let ((r (table-ref global-table
                                   (if (string? sym)
                                       (string->symbol (substring sym 1))
                                       sym))))
                   (if (and r
                            (or helpers
                                (< (vector-index (ref-value r) helper-globals 0) 0)))
                       (ref-value r)
                       (if err
                           (error-lookup-not-found sym)
                           #f))))))

(define (vector-index v vec i)
    (if (< i (vector-length vec))
        (if (eq? (vector-ref vec i) v)
            i
            (vector-index v vec (+ i 1)))
        -1))

(define (lookup-global sym #!optional (err #t) (helpers #f))
    (let ((r (find-global sym err helpers)))
        (if (procedure? r)
            (letrec*
                ((i (vector-index r core-globals 0))
                 (defn (list->vlist (list global-lambda (if (< i 0) sym i))))
                 (f2 (memoize-lambda (lambda args (apply f args)) defn))
                 (f (memoize-lambda (wrap-global-lambda r f2) defn)))
                f)
            r)))

(define (cf-test n x)
    (if (= n 0)
        (lambda (n2) (cf-test n2 x))
        (+ x n)))

(define (transfer-test k . args)
    (applyx (lookup-global 'result) (make-global-rtenv) k args))

(define (gvector-set! vec i v)
    (vector-set! vec i v)
    '#())

(define (make-binary n)
    (make-string n #\x00))

(define binary? string?)

(define binary-length string-length)

(define (binary-ref b i)
    (char->integer (string-ref b i)))

(define (binary-set! b i v)
    (string-set! b i (integer->char (inexact->exact v))))

(define (output-binary-to-stdout b start end)
    (display-substring b start end (current-output-port)))

(define (output-binary-to-stderr b start end)
    (display-substring b start end (current-error-port)))

(table-set! global-table 'result result)
(table-set! global-table 'boolean? boolean?)
(table-set! global-table 'number? number?)
(table-set! global-table '< <)
(table-set! global-table '> >)
(table-set! global-table 'add +)
(table-set! global-table 'subtract -)
(table-set! global-table 'multiply *)
(table-set! global-table 'divide /)
(table-set! global-table 'same-object? eq?)
(table-set! global-table '= =)
(table-set! global-table 'floor floor)
(table-set! global-table 'procedure? procedure?)
(table-set! global-table 'save mce-save)
(table-set! global-table 'restore grestore)
(table-set! global-table 'transfer transfer) 
(table-set! global-table 'apply applyx)
(table-set! global-table 'getpid getpid)
(table-set! global-table 'make-vector make-vector)
(table-set! global-table 'vector? vector?)
(table-set! global-table 'vector-length vector-length)
(table-set! global-table 'vector-ref vector-ref)
(table-set! global-table 'vector-set! gvector-set!)
(table-set! global-table 'get-config get-config)
(table-set! global-table 'cf-test cf-test)
(table-set! global-table 'transfer-test transfer-test)
(table-set! global-table 'set-gc-callback! (lambda (v) '#()))
(table-set! global-table 'make-binary make-binary)
(table-set! global-table 'binary? binary?)
(table-set! global-table 'binary-length binary-length)
(table-set! global-table 'binary-ref binary-ref)
(table-set! global-table 'binary-set! binary-set!)
(table-set! global-table 'output-binary-to-stdout output-binary-to-stdout)
(table-set! global-table 'output-binary-to-stderr output-binary-to-stderr)
(table-set! global-table 'error error)

(define helper-globals (vector
    ; arithmetic operations only visible at toplevel
    +
    -
    *
    /
))

(define core-globals (vector
    ; result needs to be accessible from core for when we run a
    ; lambda to its conclusion
    result

    ; apply is something only core can do
    applyx

    ; we're keeping booleans in core
    boolean?

    ; we're keeping numbers in core
    number?
    <
    >
    + ; add
    - ; subtract
    * ; multiply
    / ; divide
    =
    floor

    ; we're keeping vectors in core
    make-vector
    vector?
    vector-length
    vector-ref
    gvector-set!

    ; we're keeping lambdas in core
    procedure?

    ; we're using byte arrays (strings in Scheme) for everything else
    make-binary
    binary?
    binary-length
    binary-ref
    binary-set!

    ; errors are thrown by runtime
    error

    ; eq? needs to know if objects are the same
    eq? ; same-object?

    ; transfer can only be done in core but it might not be useful
    ; enough to keep - maybe make it optional
    transfer
    transfer-test
))

(define (get-global-function name)
    (ref-value (table-ref global-table name)))

(define (register-global-function! name f)
    (table-set! global-table name f))

(define (unregister-global-function! name)
    (table-delete! global-table name))

(define kenvfn-table (make-eq-table))

(table-set! kenvfn-table transfer #t)
(table-set! kenvfn-table applyx #t)
(table-set! kenvfn-table grestore #t)

(define (register-kenv-function! f)
    (table-set! kenvfn-table f #t))

(define config-table (make-equal-table))

(define (set-config! k v)
    (table-set! config-table k v))

(define (get-config k)
    (ref-value (table-ref config-table (substring k 1))))

(define (vector-cmap f vec tab set-entry!)
    ; note: in compiled program, two '#() instance compare eq? true
    ; other engines may modify empty vector for efficiency (setvar)
    ; so if we serialize then all empty vectors will end up being modified
    (if (= (vector-length vec) 0)
        vec
        (let ((ref (table-ref tab vec)))
            (if ref
                (ref-value ref)
                (let* ((len (vector-length vec))
                       (entry (set-entry! tab vec (make-vector len))))
                    (let loop ((i 0))
                        (if (= i len)
                            entry
                            (begin (vector-set! entry i (f (vector-ref vec i)))
                                   (loop (+ i 1))))))))))

(define unmemoized-mark (mark "UNMEMOIZED"))

(define (unmemoized? v)
    (and (= (vector-length v) 2)
         (equal? (vector-ref v 0) unmemoized-mark)))

(define (unmemoized-repexp v)
    (vector-ref v 1))

(define (memoize-aux exp tab fn)
    (if (vector? exp)
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
                                (set! f (applyvl make-form r))
                                (apply f args))))
                        entry)))
            (vector-cmap fn exp tab table-set!))
        exp))

(define (memoize exp)
    (letrec ((tab (make-eq-table))
             (fn (lambda (x) (memoize-aux x tab fn))))
        (fn exp)))

(define (unmemoize-aux exp tab fn)
    (cond ((vector? exp)
           (vector-cmap fn exp tab table-set!))
          ((procedure? exp)
           (let ((ref (table-ref tab exp)))
               (if ref
                   (ref-value ref)
                   (let ((entry (table-set! tab exp (make-vector 2))))
                       (vector-set! entry 0 unmemoized-mark)
                       (vector-set! entry 1 (fn (get-procedure-defn exp)))
                       entry))))
          (else exp)))

(define (unmemoize exp)
    (letrec ((tab (make-eq-table))
             (fn (lambda (x) (unmemoize-aux x tab fn))))
        (fn exp)))

(define serialized-mark (mark "SERIALIZED"))

(define (make-serialized n)
    (vector serialized-mark n))

(define (serialized? v)
    (and (= (vector-length v) 2)
         (equal? (vector-ref v 0) serialized-mark)))

(define (serialized-n v)
    (vector-ref v 1))

(define (serialize-aux exp tab fn set-entry!)
    (if (vector? exp)
        (vector-cmap fn exp tab set-entry!)
        exp))

(define (serialize exp)
    (letrec ((counter 0)
             (tab (make-eq-table))
             (set-entry!
              (lambda (tab v entry)
                  (table-set! tab v (make-serialized counter))
                  (set! counter (+ counter 1))
                  entry))
             (fn (lambda (x) (serialize-aux x tab fn set-entry!))))
        (let ((serialized (fn exp)))
            (vector serialized counter))))

(define (deserialize-aux exp tab fn set-entry!)
    (if (vector? exp)
        (if (serialized? exp)
            (ref-value (table-ref tab (serialized-n exp)))
            (vector-cmap fn exp tab set-entry!))
        exp))

(define (deserialize exp)
    (letrec ((counter 0)
             (tab (make-eq-table))
             (set-entry!
              (lambda (tab v entry)
                  (table-set! tab counter entry)
                  (set! counter (+ counter 1))
                  entry))
             (fn (lambda (x) (deserialize-aux x tab fn set-entry!))))
        (fn (vector-ref exp 0))))

(define true-code   "0")
(define false-code  "1")
(define number-code "2")
(define vector-code "3")
; binary codes; probably best to stick with A-Z so result of
; save (json) can't be confused: json will start with 0-9, 
; 'n' for null, '[', '{]', '"', whitespace, 't' for true,
; 'f' for false
(define marker-code "A")
; these are only valid when scanning
(define char-code   "B")
(define string-code "C")
(define symbol-code "D")

(define (pickle-aux exp #!key (is_scan #f))
   (cond ((and (null? exp) is_scan)
          (list vector-code 0))
         ((boolean? exp)
          (list (if exp true-code false-code)))
         ((number? exp)
          (list number-code exp))
         ((vector? exp)
          (cons vector-code
                (cons (vector-length exp)
                      (map (lambda (v) (pickle-aux v :is_scan is_scan))
                           (vector->list exp)))))
         ((and (pair? exp) is_scan)
          (cons vector-code
                (cons 2
                      (list (pickle-aux (car exp) :is_scan is_scan)
                            (pickle-aux (cdr exp) :is_scan is_scan)))))
         ((and (char? exp) is_scan)
          (base64-encode (string-append char-code (string exp))))
         ((and (symbol? exp) is_scan)
          (base64-encode (string-append symbol-code (symbol->string exp))))
         ((string? exp)
          (base64-encode (if (and is_scan (not (marked? exp)))
                             (string-append string-code exp)
                             exp)))
         (else
          (error "pickle" "unknown expression" exp))))

(define (pickle exp #!key (is_scan #f))
    (let ((port (open-output-string)))
        (json-write (pickle-aux exp :is_scan is_scan) port)
        (get-output-string port)))

(define (unpickle-aux exp)
    (cond ((pair? exp)
           (let ((code (car exp)))
               (cond ((equal? code true-code)
                      #t)
                     ((equal? code false-code)
                      #f)
                     ((equal? code number-code)
                      (let* ((n (cadr exp))
                             (en (inexact->exact n)))
                          (if (= n en) en n)))
                     ((equal? code vector-code)
                      (list->vector (map unpickle-aux (cddr exp))))
                     (else
                      (error "unpickle" "unknown expression" exp)))))
          ((string? exp)
           (base64-decode exp))
          (else
           (error "unpickle" "unknown expression" exp))))

(define (unpickle s)
    (unpickle-aux (json-read (open-input-string s))))

(define (mce-save exp #!key (is_scan #f))
    (pickle (serialize (unmemoize exp)) :is_scan is_scan))

(define (mce-restore s)
    (memoize (deserialize (unpickle s))))

(define (grestore k env s)
    ; We have to make restore a kenvfn because otherwise globalize will be
    ; called on the result of mce-restore which means constructed-function0
    ; won't get k and env to pass onto constructed-function1
    (send k (mce-restore s)))

(define (mce-eval exp)
    (evalx (lookup-global 'result) exp (make-global-rtenv)))

(define (mce-run exp)
    (run (mce-eval exp)))

(define (start-string s #!key (is_scan #f) (args '()))
    (if is_scan
        (display (mce-save (mce-eval (read (open-input-string s))) :is_scan is_scan))
        (let ((r (mce-restore s)))
            (if (procedure? r)
                (apply r args)
                (run r)))))

(define (start-stream stream #!key (is_scan #f) (args '()))
    (start-string (read-string stream) :is_scan is_scan :args args))

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
                 (set-config! (substring kv 0 pos)
                              (mce-restore (substring kv (+ pos 1))))))
            (else
             (error "start" "Invalid argument " else)))
        (if execute
            (if (string-null? s)
                (start-stream (current-input-port) :is_scan is_scan)
                (start-string s :is_scan is_scan)))))

(let ((runtime-ops (make-runtime-ops)))
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

(define (send runtime k . v)
    (apply (runtime-ops-send runtime) (cons k v)))
