(let loop ((i 0))
    (define (next form)
        (loop ((if (assoc "up" form) + -) i 1)))
    `(body form (@ action ,(get-config "url") method "post") ,i " "
        (input (@ type "hidden" name "state" value ,next))
        (input (@ type "submit" name "up" value "Up"))
        (input (@ type "submit" name "down" value "Down"))))
