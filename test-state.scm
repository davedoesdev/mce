(let ((k (let/cc k k)))
    (print (pickle (unmemoize k))))
