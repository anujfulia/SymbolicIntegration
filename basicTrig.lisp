(defun IntegrateBasicTrig(input)
	(cond
		(
			;if(first elem is sin)
			(equal (car input) 'sin) 
			;then do :
			
				;Let lst be the second element
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(- (cos x))
						)
						(
							(eq (car lst) '*) `(- ,(/ 1 (second lst)) cos (* ,(second lst) x)  )
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `(- (cos (+ x ,(third lst))))
										)
										(
											(eq (car lst2) '*) `(- ,(/ 1 (second lst2)) cos (+ (* ,(second lst2) x) ,(third lst))   )
										)
									)
									
								)
						)
					)
				)
		)
	)
)


