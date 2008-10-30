ruta(lujan, baires, 60).
ruta(baires, laPlata, 70).
ruta(laPlata, lujan, 140).

ruta(zarate, bsas, 30).
ruta(bahia, bsas,10).
ruta(bahia, zarate, 20)

/* [ruta(zarate, bsas, 30), ruta(bahia, bsas,10), ruta(bahia, zarate, 20)]  */

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

ej4(M,O,D,[C|Cs]) :- (O=C),ej4Aux(M,O,D,Cs,M),withoutRepeated([C|Cs]).

ej4Aux(_,D,D,[],_)  :- (O=D),!.
ej4Aux([],O,D,Cs,M) :- (1=0),!.
ej4Aux([ruta(O,C,_)|Ms],O,D,[C|Cs],M) :- select(ruta(O,C,_),M,MR),ej4Aux(MR,C,D,Cs,M).
ej4Aux([ruta(C,O,_)|Ms],O,D,[C|Cs],M) :- select(ruta(C,O,_),M,MR),ej4Aux(MR,C,D,Cs,M).
ej4Aux([ruta(A,B,_)|Ms],O,D,[C|Cs],M) :- ej4Aux(Ms,O,D,[C|Cs],M).


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


/* ej 6 */
caminoHamiltoniano(M, O, D, Cs) :-  ciudades(M, Xs),ej4(M, O, D, Cs),           /* Cs tiene que ser un camino desde O hasta D */
                                    esPermutacion(Xs, Cs).

esPermutacion([C | Cs], Ds) :-  select(C, Ds, Dss),
                                esPermutacion(Cs, Dss).
								esPermutacion([], []).



/* ej 7 */
caminosHamiltonianos([M|Ms], Cs) :- ruta(X,Y,Z), 
									M = ruta(X,Y,Z),
                                    caminoHamiltoniano( [M|Ms], X, Q, Cs).

/* ej 8 : ( Existe un camino (C1) O-hasta-D tq no Existe otro camino (C2) O-hasta-D tq length(C2) < length(C1) ) */
caminoMinimo(M, O, D, Cs, L) :- ej4(M, O, D, Cs),
                                distanciaCamino(M, Cs, L),
                                not( (ej4(M, O, D, Xs), distanciaCamino(M, Xs, R), R < L ) ). 


distanciaCamino(M, [], 0).				/* no es un caso valido, pero asumimos como resultado posible 0 */
distanciaCamino(M, [C|[]], 0). 
distanciaCamino(M, [O,C|Cs], X) :- 	( member(ruta(O,C,Z), M) ; member(ruta(C,O,Z), M) ),
                                  	distanciaCamino(M, [C|Cs], W), X is W+Z.


/* ej 10*/

caminosEulerianos([M|Ms],Cs) :- ruta(X,Y,Z),
                                M = ruta(X,Y,Z),
                                caminoEuleriano( [M|Ms], X, Q, Cs).
