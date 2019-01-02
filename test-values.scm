(begin (let-values (((a b) (values 1 2))
                    ((c d) (values 3 4)))
           (print (list a b c d)))
       (let-values (((a b . c) (values 1 2 3 4)))
           (print (list a b c)))
       (let ((a 'a) (b 'b) (x 'x) (y 'y))
           (let-values (((a b) (values x y))
                        ((x y) (values a b)))
               (print (list a b x y)))))

