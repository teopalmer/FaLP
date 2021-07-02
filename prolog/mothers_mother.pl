domains 
name = symbol
predicates
mother(name, name).
father(name, name).
family(name, name, name).
grandMF(name, name).
grandFF(name, name).
grandMM(name, name).
grandFM(name, name).
allGrandM(name, name).
allGrandF(name, name).
allGrandP(name, name).
grandPM(name, name).

relative(name, integer, name).
getMax(name, integer, integer).
get(name, integer).
clauses
mother(olga, tomara).
mother(tomara, elena).
mother(elena, denis).
mother(elena, vlad).
father(smth, tomara).
father(gena, elena).
father(oleg, denis).
father(oleg, vlad).

family(Name, Father, Mother) :- 
mother(Mother, Name),
father(Father, Name).

grandMF(X, Y) :- mother(X, Z), father(Z, Y).
grandFF(X, Y) :- father(X, Z), father(Z, Y).
grandMM(X, Y) :- mother(X, Z), mother(Z, Y).
grandFM(X, Y) :- father(X, Z), mother(Z, Y).

allGrandM(X, Y) :- grandMM(X, Y).
allGrandM(X, Y) :- grandMF(X, Y).

allGrandF(X, Y) :- grandFM(X, Y).
allGrandF(X, Y) :- grandFF(X, Y).

allGrandP(X, Y) :- allGrandF(X, Y).
allGrandP(X, Y) :- allGrandM(X, Y).

grandPM(X, Y) :- grandFM(X, Y).
grandPM(X, Y) :- grandMM(X, Y).

% predok n-go poryadka
relative(Name, 0, RezName) :- family(Name, _, RezName).

relative(Name, Count, RezName) :-
Count > 0,
Count_new = Count - 1,
family(Name, _, Mother),
relative(Mother, Count_new, RezName).

getMax(Name, Count, RezCount) :- 
Count_new = Count + 1,
family(Name, Father, Mother),
getMax(Mother, Count_new, RezCount), !.
getMax(Name, Count, Count).


get(Name, RezCount) :- getMax(Name, -1, RezCount).


goal
%grandMM(X, denis).
%grandFM(X, denis).
%relative(denis, 2, RezName).
get(denis, RezCount).