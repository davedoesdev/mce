(let ((a 1))
    (define b 2)
    (set! b 3)
    (set! a 5)
    (print a b)
    (print (unmemoize (lambda () '()))))
