(let ((l '(1 2 3)))
    (set-cdr! l l)
    (let ((k (let/cc k k)))
        (print k)
        (if (procedure? k)
            (begin ;(write (save k)) (newline)
                   (transfer (restore (save k)) 91)
                   (print "one")
                   (k 90)
                   (print "two")))))