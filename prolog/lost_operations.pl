pow(_, 0, 1):-!. 
pow(X, N, _) :- X > 10, N > 8, !, fail. 
pow(X, N, Res) :- N > 0, N1 = N - 1, pow(X, N1, Res1), Res = X * Res1. 
pow(X, N, Res) :- N < 0, N1 = N + 1, pow(X, N1, Res1), Res = X * Res1. 

operator("^", X, Y, Res) :- pow(X, Y, Res1), Res is Res1. 
operator("+", X, Y, Res) :- Res is X + Y. 
operator("-", X, Y, Res) :- Res is X - Y. 
operator("mod", X, Y, Res) :- Res is X mod Y. 
operator("div", X, Y, Res) :- Res is X div Y. 
operator("*", X, Y, Res) :- Res is X * Y. 

calculate(operation(X, Y, Op), Res) :- operator(Op, X, Y, Res). 

is_ordered([]). 
is_ordered([_]). 
is_ordered([A, B | C]) :- 
% write([A,B | C]), 
(member(B, ["^", "*", "mod", "div"]) -> 
\+ (member(A, ["-", "+"])) 
; 
true 
), 
is_ordered([B | C]), !. 

result(X1, O1, X2, O2, X3, O3, X4, R) :- 
calculate(operation(X1, X2, O1), R1), 
calculate(operation(R1, X3, O2), R2), 
calculate(operation(R2, X4, O3), R), 
is_ordered([O1, O2, O3]).