(let loop ((i -20))
    (if (< i 20)
        (begin (print i)
               (loop (+ i 1)))))
