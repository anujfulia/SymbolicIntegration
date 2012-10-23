(defun getCoeff (lst power)
    (cond
        ((equal (length lst) 0) nil)
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
