ruta(lujan, baires, 60).
ruta(baires, laPlata, 70).
ruta(laPlata, lujan, 140).



ej1(M, Cs) :- withoutRepeated(M), withoutRepeated(CS), routesIncluded(M, Cs), citiesIncluded(Cs, M).


 withoutRepeated([]).
 withoutRepeated([X|XS]) :- not(member(X, XS)), withoutRepeated(XS).

 routesIncluded([], _).
 routesIncluded([ruta(X,Y,Z)|MX], CS) :- member(X, CS), member(Y,CS), routesIncluded(MX, CS).

 citiesIncluded([], _).
 citiesIncluded([C|CS], [ruta(C,X,Y)|MS]) :- citiesIncluded(CS, [ruta(C,X,Y)|MS]).
 citiesIncluded([C|CS], [ruta(X,C,Y)|MS]) :- citiesIncluded(CS, [ruta(C,X,Y)|MS]).


/* ej2 */
ej2(M, C, [X|Xs]) :- setOf(P, esVecino(M, C, X), L), M =:= L, ej2(M, C, Xs).

esVecino(M,X,Y) :- member(ruta(Y,X,Z), M).
esVecino(M,X,Y) :- member(ruta(X,Y,Z), M).

 


/*
isPresent(X, CS) :- member(X, CS).
*/




/*				ejemplo boludo
append([], L, L).
append([X|XS], L, [X|R]) :- append(XS, L, R).

equals([], []).
equals([X|XS], [Y|YS]) :- X = Y, equals(XS, YS).

sameSize([], []).
sameSize([M|MS], [C|CS]) :- sameSize(MS, CS).
*/


