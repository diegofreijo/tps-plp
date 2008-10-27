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

/* ej3 */

ej3([ruta(C1,C2,Z)|MS],C1,C2,N) :- (Z = N).
ej3([ruta(C2,C1,Z)|MS],C1,C2,N) :- (Z = N).
ej3([ruta(_,_,Z)|MS],C1,C2,N)   :- (1 = 0);ej3(MS,C1,C2,N).

/* ej4 */
/* Agregar without repeated cuando este andando*/
ej4(M,O,D,[])      :- (O=D).
ej4(M,O,D,[C|Cs])  :- hayCamino(M,O,C),ej4(M,C,D,Cs).

hayCamino(M,O,D) :- esVecino(M,O,D);(O=D).

/* ej5 */
ej5([ruta(O,D,N)|Ms]) = noReflexivo(

/*
isPresent(X, CS) :- member(X, CS).
*/




/*                              ejemplo boludo
append([], L, L).
append([X|XS], L, [X|R]) :- append(XS, L, R).

equals([], []).
equals([X|XS], [Y|YS]) :- X = Y, equals(XS, YS).

sameSize([], []).
sameSize([M|MS], [C|CS]) :- sameSize(MS, CS).
*/