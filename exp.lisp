(defun IntegralExp(input)
	(cond
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
	)	
)


