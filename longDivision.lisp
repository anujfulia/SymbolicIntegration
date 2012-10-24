;A general assumption on input. The polynomial should be written in decresaing order of power. i.e. 3x + 4x^3 should be input as '("+" (4 x 3) (3 x 1))

; Checks if its of the three memebr tuple form of polynomial
; Call with status = 1
(defun poly-p (poly status)
    (if (= (length poly) 0) t
        (if (= status 1) (and (string= (first poly) "+") (poly-p (rest poly) 0))
            (and (= (length (first poly)) 3) (numberp (first (first poly))) (numberp (third (first poly))) (poly-p (rest poly) 0))
        )
    )
)
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
; Initially call with power = -1,status = 1
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

;Call with status = 1
;Returns the new numerator after one iteration of division
;Mul is the factor by which denom should be multiplied
(defun newNum (num den mul status)
   (cond
        ((= (length den) 0) num)
        ( (= status 1) (cons (- (first num) 1) (newNum (cdr (cdr num)) (cdr (cdr den)) mul 0)) )
        (t (cons (- (first num) (* mul (first den))) (newNum (cdr num) (cdr den) mul 0)))
   )
)
;Performs division on two polynomials
;It takes the coefficients of numerator and denominator as the above mentioned format
;Initially call with status = 1
(defun Divide (num den status)
    (cond
    ( (< (first num) (first den)) (list num))
     ( (= status 1) (cons (- (first num) (first den)) (Divide num den 0)))
     (t (cons (/ (second num) (second den)) (Divide (newNum num den (/ (second num) (second den)) 1) den 0))) 
    )
)

;Check if a polynomial representation is equivalent to not a zero
;As usual status = 1 initially
(defun notZero (poly status)
    (if (= (length poly) 0) nil
        (if (= status 1) (notZero (cdr poly) 0)
            (cond
                ((not (= (car poly) 0)) t)
                (t (notZero (cdr poly) 0))
            )
        )
    )
)

;Checks if the remainder obtained is zero
(defun hasRemainder (lst)
    (cond
     ( (= (length lst) 0) nil)
     ( (listp (car lst)) (or (notZero (car lst) 1) (hasRemainder (cdr lst))))
     (t (hasRemainder (cdr lst)))

    )
)

;Get the kast element in a list
(defun getLast (lst)
    (if (= (length lst) 1) (car lst)
        (getLast (cdr lst))
    )
)

;Integrates certain standard functions based on hardcoded values
;For now the standard forms are c1/(ax+b) and c1/(ax2 + bx + c)
(defun standardIntegrate(num den)
    (setf c1 (getLast num))
    (if (= (car den) 1) (constLinearDiv (list "/" c1 (getPoly den 1 -1)))
        (constSquareDiv (list "/" c1 (getPoly den 1 -1))) 
    )
)

;StripLast- this strips the last element which is assumed to be a lst
(defun StripLast(lst)
    (if (listp (car lst)) nil
        (cons (car lst) (StripLast (cdr lst)))
    )

)

;Gets the last element in the list and assumes that to be the remainder
(defun getRemainder(lst)
     (if (not (listp (car lst))) (getRemainder (cdr lst))
        (car lst)
    )  

)
    
;Call the function with the numerator polynomial and  the denominator polynomial in the format mentioned above
(defun LongDivision (numerator denominator)
    (setf numExp (getCoeff numerator 0))
    (setf denExp (getCoeff denominator 0))
    
    ;(getPoly (Divide numExp denExp 1) 1 -1)
    (setf quotient (Divide numExp denExp 1))
    (setf remainder (getRemainder quotient))
    (if (hasRemainder quotient)
        (list "+" (getPoly (stripLast quotient) 1 -1) (standardIntegrate remainder denExp))
        (getPoly (stripLast quotient)1 -1)
        
    )


)

;Functions of the form '("/" c1 '("+" (a x 2) (b x 1) (c x 0)))
;Returns a standard T1(a,b,c,c1,x). Read it as 2*c1*atan((2ax+b)/sqrt(4ac-b^2))/sqrt(4ac-b^2) 
(defun constSquareDiv(poly)
    (list "T1(" (first (second (third poly))) "," (first (third (third poly))) "," (first (fourth (third poly))) "," (second poly) "," (second (second (third poly)))")")
)

;Functions of the form '("/" c '("+" (a x 1) (b x 0)))
;Returns a standard T2(a,b,c,x). Read it as c*ln(ax+b)/a 
(defun constLinearDiv(poly)
    (list "T2(" (first (second (third poly))) "," (first (third (third poly))) "," (second poly) "," (second (second (third poly)))")")
)
    
(defun driver(func)
    (cond 
        
        ( (poly-p func 1) (polyIntegration func))
        ( (string= (first func) "/")

            (LongDivision (second func) (third func))
        )
    )
)

(defun test(lst)
    (numberp (first '(sin)))
)
