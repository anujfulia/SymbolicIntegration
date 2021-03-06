(defun exp-p(input)
    (if (or (equal (car input) 'exp) (equal (car input) 'sinh)
            (equal (car input) 'cosh) (equal (car input) 'tanh)
            (equal (car input) 'cosech) (equal (car input) 'cot)
            (equal (car input) 'log)  (equal (car input) 'pow)
        ) t nil
    )
)

(defun IntegralExp(input)
	(cond
	( (equal (car input) 'pow)
        `(/ ,input (* ,(second (second (third input))) (log ,(second input))))
    )
    (
			(equal (car input) 'exp) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(exp x)
						)
						(
							(eq (car lst) '*) `(* ,(/ 1 (second lst)) (exp (* ,(second lst) x)))  
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `(exp (+ x ,(third lst)))
										)
										(
											(eq (car lst2) '*) `(* ,(/ 1 (second lst2)) (exp (+ (* ,(second lst2) x) ,(third lst))) )   
										)
									)
									
								)
						)
					)
				)
		)
		
				(
			(equal (car input) 'sinh) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(cosh x)
						)
						(
							(eq (car lst) '*) `(* ,(/ 1 (second lst)) (cosh (* ,(second lst) x)))  
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `(cosh (+ x ,(third lst)))
										)
										(
											(eq (car lst2) '*) `(* ,(/ 1 (second lst2)) (cosh (+ (* ,(second lst2) x) ,(third lst))) )   
										)
									)
									
								)
						)
					)
				)
		)
		(
			(equal (car input) 'cosh) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(sinh x)
						)
						(
							(eq (car lst) '*) `(* ,(/ 1 (second lst)) (sinh (* ,(second lst) x)))  
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `(sinh (+ x ,(third lst)))
										)
										(
											(eq (car lst2) '*) `(* ,(/ 1 (second lst2)) (sinh (+ (* ,(second lst2) x) ,(third lst))) )   
										)
									)
									
								)
						)
					)
				)
		)
		
		(
			(equal (car input) 'tanh) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(ln (cosh x))
						)
						(
							(eq (car lst) '*) `(* ,(/ 1 (second lst)) (ln (cosh (* ,(second lst) x)))  )
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `(ln (cosh (+ x ,(third lst))))
										)
										(
											(eq (car lst2) '*) `(* ,(/ 1 (second lst2)) (ln (cosh (+ (* ,(second lst2) x) ,(third lst)))) )   
										)
									)
									
								)
						)
					)
				)
		)
		
		(
			(equal (car input) 'csch) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(ln (tanh (/ x 2)))
						)
						(
							(eq (car lst) '*) `(* ,(/ 1 (second lst)) (ln (tanh (* ,(/ (second lst) 2) x) )) )
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `( ln (tanh (/ 2 (+ x ,(third lst)))))
										)
										(
											(eq (car lst2) '*) `(* ,(/ 1 (second lst2)) (ln (tanh (/ (+ (* ,(second lst2) x) ,(third lst)) 2 )  )))
										)
									)
									
								)
						)
					)
				)
		)
		
		(
			(equal (car input) 'coth) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(ln (sinh x))
						)
						(
							(eq (car lst) '*) `(* ,(/ 1 (second lst)) (ln (sinh (* ,(second lst) x) ) ))
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `( ln (sinh (+ x ,(third lst))))
										)
										(
											(eq (car lst2) '*) `(* ,(/ 1 (second lst2)) (ln (sinh (+ (* ,(second lst2) x) ,(third lst)) )  ))
										)
									)
									
								)
						)
					)
				)
		)
        
        (
			(equal (car input) 'log) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(* x (- (log x) 1))
						)
						(
							(eq (car lst) '*) `(* ,(/ 1 (second lst)) (* (* ,(second lst) x) (- (log (* ,(second lst) x)) 1)))  
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `(* (+ x ,(third lst)) (- (log (+ x ,(third lst))) 1))
										)
										(
											(eq (car lst2) '*) `(* ,(/ 1 (second lst2)) (* (+ (* ,(second lst2) x) ,(third lst)) (- (log (+ (* ,(second lst2) x) ,(third lst))) 1)) )   
										)
									)
									
								)
						)
					)
				)
		)



	)	
)



