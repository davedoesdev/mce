(let/cc stop
    (let loop ((i 0))
        (eprint i)
        (if (= i 5)
            (begin (let/cc k
                       (eprint "save " (getpid))
                       (write (save (lambda () (k #t))))
                       (stop #f))
                    (print "restore " (getpid))))
        (if (< i 10)
            (loop (+ i 1)))))