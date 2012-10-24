;A general assumption on input. The polynomial shoule be written in decresaing order of power. i.e. 3x + 4x^3 should be input as '("+" (4 x 3) (3 x 1))

;given a polynomial of form ("+" (coeff variable power) (coeff variable power) ..)
;returns (maxPower coeffofhigestpower coeffofnexthighest ...)
;Sample input (getCoeff '("+" (4 x 3)) -1)
;Sample output (3 4 0 0 0)
;NOTE: Applicable only for integer powers
;Initially call with power = -1
(defun getCoeff (lst power)
    (if (stringp (first lst))
        (cond
            ((string= (first lst) "+") (cons (third (second lst)) (getCoeff (cdr lst) (third (second lst)))  ))
        )
        (cond
            ((= (length lst) 0) nil)
            ((= power (third (first lst))) (cons (car (car lst)) (getCoeff (cdr lst) (- power 1))))
            (t (cons 0 (getCoeff lst (- power 1))))
        )
    )
)

; Get polynomial from a coefficients
; Note status says if its the first time called or not
; Initially call with power = -1
; Will return a polynomial in x
(defun getPoly (coeff status power)
   (if (= status 1)
        (
         cons "+" (getPoly (cdr coeff) 0 (car coeff))
        )
       (if (= (length coeff) 0) nil
        (cons
         (list (first coeff) "x" power)
         (getPoly (cdr coeff) 0 (- power 1))

        )
       )
    )
)


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
    (setf denExp (getCoeff denominator 0))
 
)
