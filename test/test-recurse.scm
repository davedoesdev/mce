(begin (define (foo a)
           (if (= a 10)
               a
               (foo (+ a 1))))
       (print (foo 0)))
