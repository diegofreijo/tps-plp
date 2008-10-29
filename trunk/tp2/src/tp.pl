ruta(lujan, baires, 60).
ruta(baires, laPlata, 70).
ruta(laPlata, lujan, 140).

/* ej1 */
ciudades([], []).
ciudades([M|Ms], [X,Y|Cs]) :-   ciudades(Ms, Cs), ruta(X,Y,Z), ruta(X,Y,Z) = M, not(member(X,Cs)), not(member(Y,Cs)).
ciudades([M|Ms], [X|Cs])    :-   ciudades(Ms, Cs), ruta(X,Y,Z), ruta(X,Y,Z) = M, not(member(X,Cs)), member(Y,Cs).
ciudades([M|Ms], [Y|Cs])    :-   ciudades(Ms, Cs), ruta(X,Y,Z), ruta(X,Y,Z) = M, member(X,Cs), not(member(Y,Cs)).
ciudades([M|Ms], Cs)         :-   ciudades(Ms, Cs), ruta(X,Y,Z), ruta(X,Y,Z) = M, member(X,Cs), member(Y,Cs).

withoutRepeated([]).
withoutRepeated([X|XS]) :- not(member(X, XS)), withoutRepeated(XS).

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

ej4(M,O,D,[C|Cs])  :- (O=C),ej4Aux(M,O,D,Cs),withoutRepeated([C|Cs]).

ej4Aux(M,O,D,[])     :- (D=O).
ej4Aux(M,O,D,[C|Cs]) :- ej4Aux(M,C,D,Cs),hayCamino(M,O,C).

hayCamino(M,O,D) :- esVecino(M,O,D).

/* ej5 */
ej5(M) :- ciudades(M,J),todasAlcanzables(J,M,J),noReflexiva(J,M,M),noSimetria(M,M).
/*ej5(M) :- noSimetria(M,M).*/

noSimetria([],_) :- (1 = 1).
noSimetria([ruta(C1,C2,_)|Ms],M) :- noRuta(C2,C1,M),noSimetria(Ms,M).

noRuta(C1,C2,[ruta(A,B,_)|Ms])   :- not(A = C1);not(B = C2),noRuta(C1,C2,Ms).

noReflexiva([],_,M)                    :- (1 = 1).
noReflexiva([C|Cs],[],M)               :- (1 = 1),noReflexiva(Cs,M,M).
noReflexiva([C|Cs],[ruta(C,C,_)|Ms],M) :- (1 = 0),noReflexiva([C|Cs],Ms,M).
noReflexiva([C|Cs],[ruta(A,B,_)|Ms],M) :- not(A = B),(1 = 1),noReflexiva([C|Cs],Ms,M).

todasAlcanzables([],M,Css)     :- (1 = 1).
todasAlcanzables([C|Cs],M,Css) :- todasAlcanzables(Cs,M,Css),alcanzable(C,Css,M).

alcanzable(C1,[],M) :- (1 = 1).
alcanzable(C1,[C|CS],M) :- alcanzable(C1,CS,M),ej4(M,C,C1,X).

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