(defun DifferentiateBasicTrig(input)
	(cond
		(
			(equal (car input) 'sin) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(cos x)
						)
						(
							(eq (car lst) '*) `(* ,(second lst) (cos (* ,(second lst) x))  )
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `(cos (+ x ,(third lst)))
										)
										(
											(eq (car lst2) '*) `(* ,(second lst2) (cos (+ (* ,(second lst2) x) ,(third lst)))    )
										)
									)
									
								)
						)
					)
				)
		)
		
		
		(
			(equal (car input) 'cos) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(- (sin x))
						)
						(
							(eq (car lst) '*) `(- (* ,(second lst) (sin (* ,(second lst) x))   ))
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `(- ( sin (+ x ,(third lst))))
										)
										(
											(eq (car lst2) '*) `(- (* ,(second lst2) (sin (+ (* ,(second lst2) x) ,(third lst)))   ))
										)
									)
									
								)
						)
					)
				)
		)
		
		(
			(equal (car input) 'tan) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(sec x 2)
						)
						(
							(eq (car lst) '*) `(* ,(second lst) (sec (* ,(second lst) x) ) 2 )
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `(sec (+ x ,(third lst)) 2)
										)
										(
											(eq (car lst2) '*) `(* ,(second lst2) (sec (+ (* ,(second lst2) x) ,(third lst)) 2)  )
										)
									)
									
								)
						)
					)
				)
		)
		
		(
			(equal (car input) 'csc) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(- (* (csc x) (cot x)))
						)
						(
							(eq (car lst) '*) `(- (* ,(second lst) (* (csc (* ,(second lst)  x) ) (cot(* ,(second lst)  x) )) ))
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `(- (* (csc (+ x ,(third lst))) (cot (+ x ,(third lst)) )))
										)
										(
											(eq (car lst2) '*) `(- (* ,(second lst2) (* (csc (+ (* ,(second lst2) x) ,(third lst))  ) (cot (+ (* ,(second lst2) x) ,(third lst))  ))))
										)
									)
								)
						)
					)
				)
		)
		
		(
			(equal (car input) 'sec) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(* (sec x) (tan x))
						)
						(
							(eq (car lst) '*) `(* ,(second lst) (* (sec (* ,(second lst)  x) ) (tan (* ,(second lst)  x) )) )
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `(* (sec (+ x ,(third lst))) (tan (+ x ,(third lst)) ))
										)
										(
											(eq (car lst2) '*) `(* ,(second lst2) (* (sec (+ (* ,(second lst2) x) ,(third lst))  ) (tan (+ (* ,(second lst2) x) ,(third lst))  )))
										)
									)
								)
						)
					)
				)
		)
		
		(
			(equal (car input) 'cot) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(- (csc x 2))
						)
						(
							(eq (car lst) '*) `(- (* ,(second lst) (csc (* ,(second lst) x) ) 2 ))
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `(- (csc (+ x ,(third lst)) 2))
										)
										(
											(eq (car lst2) '*) `(- (* ,(second lst2) (csc (+ (* ,(second lst2) x) ,(third lst)) 2)  ))
										)
									)
									
								)
						)
					)
				)
		)
		
	)
)


