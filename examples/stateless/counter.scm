(let loop ((i 0))
    (define (up)
        (loop (+ i 1)))
    ;`(body (form (a (@ (href ,(save up))) "up")))
    ;`(body form ,i " " a (@ href ,up) "up")
    `(body form (@ action ,(get-config "url") method "post") ,i " "
        (input (@ type "hidden" name "state" value ,up))
        input (@ type "submit" value "Up"))
    )
