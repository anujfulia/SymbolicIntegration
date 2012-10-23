;given a polynomial of form ("+" (coeff variable power) (coeff variable power) ..)
;returns (maxPower coeffofhigestpower coeffofnexthighest ...)
;Sample input (getCoeff '("+" (4 x 3)) 0)
;Sample output (3 4 0 0 0)
(defun getCoeff (lst power)
    (cond
        ((equal power 0) 0)
        ((string= (first lst) "+") (cons (third (second lst)) (getCoeff (cdr lst) (third (second lst)))  ))
        ((equal power (third (first lst))) (cons (first (car lst)) (getCoeff (cdr lst) (- power 1))))
        (t (cons 0  (getCoeff (cdr lst) (- power 1))))

    )
)
(defun test(lst)
    (string= (first lst) "+")
)


(defun LongDivision (numerator denominator)
 

    (setf numExp (getCoeff numerator 0))
    (setf denExp (getCoeffdenominator 0))
 
)
