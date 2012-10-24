;----------------------------------------------------------------
; INPUT : (IntegrateBasicTrig '(trig (+ (* a x) b)))
; 		  where trig = basic trigonometric functions 
;----------------------------------------------------------------
(defun IntegrateBasicTrig(input)
	(cond
		(
			(equal (car input) 'sin) 
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
		
		
		(
			(equal (car input) 'cos) 
				(let* 
					((lst  (car (cdr input)) )) 
					;(second lst)
					(cond
						(
							(eq lst 'x)  '(sin x)
						)
						(
							(eq (car lst) '*) `(,(/ 1 (second lst)) sin (* ,(second lst) x)  )
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `( (sin (+ x ,(third lst))))
										)
										(
											(eq (car lst2) '*) `(,(/ 1 (second lst2)) sin (+ (* ,(second lst2) x) ,(third lst))   )
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
							(eq lst 'x)  '(ln (sec x))
						)
						(
							(eq (car lst) '*) `(,(/ 1 (second lst)) ln (sec (* ,(second lst) x) ) )
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `( (ln (sec (+ x ,(third lst)))))
										)
										(
											(eq (car lst2) '*) `(,(/ 1 (second lst2)) ln (sec (+ (* ,(second lst2) x) ,(third lst)) )  )
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
							(eq lst 'x)  '(ln (tan (/ 2 x)))
						)
						(
							(eq (car lst) '*) `(,(/ 1 (second lst)) ln (tan (* ,(/ 2 (second lst)) x) ) )
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `( (ln (tan (/ 2 (+ x ,(third lst))))))
										)
										(
											(eq (car lst2) '*) `(,(/ 1 (second lst2)) ln (tan (/ 2 (+ (* ,(second lst2) x) ,(third lst)) )  ))
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
							(eq lst 'x)  '(ln (+ (sec x) (tan x)))
						)
						(
							(eq (car lst) '*) `(,(/ 1 (second lst)) ln (+ (sec (* ,(second lst) x) ) (tan (* ,(second lst) x) )) )
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `( (ln (+ (sec (+ x ,(third lst))) (tan (+ x ,(third lst))))))
										)
										(
											(eq (car lst2) '*) `(,(/ 1 (second lst2)) ln (+ (tan (+ (* ,(second lst2) x) ,(third lst)) ) (sec (+ (* ,(second lst2) x) ,(third lst)) ))  )
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
							(eq lst 'x)  '(ln (sin x))
						)
						(
							(eq (car lst) '*) `(,(/ 1 (second lst)) ln (sin (* ,(second lst) x) ) )
						)
						(
							(eq (car lst) '+) 
								(let*
									((lst2 (car (cdr lst)) ))
									(cond
										(
											(eq lst2 'x) `( (ln (sin (+ x ,(third lst)))))
										)
										(
											(eq (car lst2) '*) `(,(/ 1 (second lst2)) ln (sin (+ (* ,(second lst2) x) ,(third lst)) )  )
										)
									)
									
								)
						)
					)
				)
		)
		
	)
)


