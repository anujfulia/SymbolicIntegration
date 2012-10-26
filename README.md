Paradigms of Programming
===================
Contains the Symbolic integration in Common LISP.

Features Supported
===================

1) Simple polynomial integration. Assumption on the nature of input ("+" (a1 x n) (a2 x n-1) .. )

2) Multiplication of two polynomials ("*" ("+" (4 x 3)) ("+" (3 x 2)))

3) Rational Polynomials of the form p1(x)/p2(x) where p1(X) is divisible by p2(x) or leaves a remainder of the form c1/(ax+b) or c2/(ax^2 +bx +c)

4) Basic Trignometric Forms

5) Basic Logarithmic,Exponential, Power and hyperbolic functions

6) Integration by parts on polynomials, trignometric and exponential functions. (Note functions after u' * int(v) should also be one of the forms supported).

7) Addition of expressions (e.g. xsinx + cosx + e^x)

