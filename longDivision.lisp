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

; Get polynomial from a coefficients



(defun test(lst)
    (third (first lst))
)

; Polynomial Integration
; Input: '("+" (4 x 3) (3 x 0.5))
; Output: '("+" (1 x 4) (2 x 1.5))

(defun polyIntegration (polyn)
    (if (stringp (first polyn))
        (cond
            ((string= (first polyn) "+") (cons "+" (polyIntegration (cdr polyn) )  ))
        )
        (cond
            ((= (length polyn) 0) nil)
            (t 
             (cons
             ; Get the new tuple (coeff/(power+1) x power+1)

                (list (/ (first (first polyn)) (+ (third (first polyn)) 1)) (second (first polyn)) (+ (third (first polyn)) 1))
                (polyIntegration (cdr polyn))
            )
            )
        )
    )
)



(defun LongDivision (numerator denominator)
 

    (setf numExp (getCoeff numerator 0))
    (setf denExp (getCoeffdenominator 0))
 
)
