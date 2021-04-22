(begin
    (set-gc-callback! (lambda (stats)
        (print stats)
        (if (vector? stats)
            (let ((first (vector-ref stats 0))
                  (second (vector-ref stats 1))
                  (third (vector-ref stats 2)))
                (and (= (car second) 3)
                     (= (cdr second) 1)
                     (< (car third) 20000)
                     (< (cdr third) 10000)))
            (< stats 0))))
    (let loop ()
        (let ((p (cons 1 2)))
            (set-cdr! p p)
            (loop))))
; run with --gc-threshold=20000
; stats should show 3 vectors referenced from C++:
; - state in start()
; - state in run()
; - nil
; and 1 referenced lambda:
; - gc callback
