(print (cf-test 1 2) " "
       ((cf-test 0 10) 3) " "
       (((cf-test 0 20) 0) 4) " "
       ((restore (save cf-test)) 1 2) " "
       ((restore (save (cf-test 0 3))) 10) " "
       ((restore (save ((cf-test 0 20) 0))) 4)
       )
