(let loop ()
    (let ((p (cons 1 2)))
        (set-cdr! p p)
        (loop)))
