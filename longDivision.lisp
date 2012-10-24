;given a polynomial of form ("+" (coeff variable power) (coeff variable power) ..)
;returns (maxPower coeffofhigestpower coeffofnexthighest ...)
;Sample input (getCoeff '("+" (4 x 3)) 0)
;Sample output (3 4 0 0 0)
(defun getCoeff (lst power)
    (if (stringp (first lst))
        (cond
            ((string= (first lst) "+") (cons (third (second lst)) (getCoeff (cdr lst) (third (second lst)))  ))
        )
        (cond
            ((= power -1) nil)
            ((= power (third (first lst))) (cons (car (car lst)) (getCoeff (cdr lst) (- power 1))))
            (t (cons 0 (getCoeff lst (- power 1))))
        )
    )
)
(defun test(lst)
    (third (first lst))
)


(defun LongDivision (numerator denominator)
 

    (setf numExp (getCoeff numerator 0))
    (setf denExp (getCoeffdenominator 0))
 
)
