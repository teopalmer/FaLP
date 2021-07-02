delEll([_|Teil], Res, I, N):- 
I1 = I + 1, 
I1 = N + 1, 
delEll(Teil, Res, I1, N),!. 
delEll([Head|Teil], [Head|Tail1], I, N):- 
I1 = I + 1, 
delEll(Teil, Tail1, I1, N),!. 
delEll([],[],_,_). 

includes([H|_], H, I, I). 
includes([_|T], T1, I1, I):- 
I2 = I1 + 1, 
includes(T, T1, I2, I). 

result([], _, []). 
result([H|T], List, [H|T1]):- 
includes(List, H, 0, I), 
delEll(List, Res, 0, I), 
result(T, Res, T1),!. 
result([_|T], List, T1):- 
result(T, List, T1).