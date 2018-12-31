(begin (print (case (* 2 3)
                 ((2 3 5 7) 'prime)
                 ((1 4 6 8 9) 'composite)))

       (print (case (car '(c d))
                 ((a) 'a)
                 ((b) 'b)))

       (print (case (car '(c d))
                 ((a e i o u) 'vowel)
                 ((w y) 'semivowel)
                 (else 'consonant))))
