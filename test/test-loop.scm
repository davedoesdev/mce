(let loop ((i 0))
    (if (< i 10)
        (begin (print i)
               (loop (+ i 1)))))
