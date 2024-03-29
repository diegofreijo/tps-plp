% Mapas de ejemplo

mapaEjemplo([
  ruta(zarate, bsas, 30),
  ruta(bahia, bsas, 10),
  ruta(bahia, zarate, 20)]).

mapa1([ruta(a, b, 20), ruta(b, c, 10), ruta(c, a, 20)]).

mapa2([
  ruta(a, b, 10),
  ruta(a, c, 20),
  ruta(b, d, 30),
  ruta(b, e, 20),
  ruta(c, d, 10),
  ruta(e, a, 40)]).
  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% ej 1
% ciudades(+M, -Cs)
%
% En este predicado se quiere ver si dado una lista de ciudades, estas corresponden a las ciudades
% contenidas en el mapa pasado tambien como parametro.
% El predicado se definio en forma recursiva sobre el mapa, donde sabiendo que cada elemento del
% mismo es de la forma :
%                 ruta(X,Y,_)
% en cada paso recursivo, solo puede haber 3 casos:
% -       Ninguno de los dos esta y entonces hay que agregarlos.
% -       X esta pero Y no entonces hay que agregar Y
% -       Idem anterior pero para Y
% -       X e Y estan en la lista y por ende no hay que agregarlos
% El caso base es un mapa vacio con una lista de ciudades vacias.
% De este modo, queda armada por construccion una lista de ciudades sin repetidos donde
% cada una es una ciudad del mapa, y estan todas las de este contenidas en dicha lista.

ciudades([], []).
ciudades([ruta(X,Y,_)|Ms], [X,Y|Cs]) :-   ciudades(Ms, Cs), not(member(X,Cs)), not(member(Y,Cs)).
ciudades([ruta(X,Y,_)|Ms], [X|Cs])   :-   ciudades(Ms, Cs), not(member(X,Cs)), member(Y,Cs).
ciudades([ruta(X,Y,_)|Ms], [Y|Cs])   :-   ciudades(Ms, Cs), member(X,Cs), not(member(Y,Cs)).
ciudades([ruta(X,Y,_)|Ms], Cs)       :-   ciudades(Ms, Cs), member(X,Cs), member(Y,Cs).



% ej 2
% ciudadesVecinas(+M, +C, -Cs)
% esVecino(+M, +X, +Y)
%
% Verificar que Vecinos es la lista de vecinos de Ciudad equivale a ver si cada ciudad
% que es vecina esta en la lista. Para verificar si una ciudad es vecina de otra solo
% hay que verificar que exista alguna ruta que contenga a alguna como origen y a la
% otra como destino.

ciudadesVecinas(Mapa, Ciudad, Vecinos) :- setof(Vecino, esVecino(Mapa, Ciudad, Vecino), Vecinos).

esVecino(M, X, Y) :- member(ruta(X, Y, _), M).
esVecino(M, X, Y) :- member(ruta(Y, X, _), M).



% ej 3
% distanciaVecinas(+M, +C1, +C2, -N)
%
% Busca recursivamente la ruta deseada y verifica si la longitud es igual al numero
% dado, si no la encuentra, sigue iterando hasta encontrarla.

distanciaVecinas([ruta(C1, C2, Z) | MS], C1, C2, Z).
distanciaVecinas([ruta(C2, C1, Z) | MS], C1, C2, Z).
distanciaVecinas([ruta(_, _, Z) | MS], C1, C2, N)      :- distanciaVecinas(MS, C1, C2, N).



% ej 4
% caminoSimple(+M, +D, +H, -Cs)
%
% Este predicado recursivo determina si existe un camino en el mapa M que vaya desde
%  O hasta D. Para esto nos valemos de la
% transitividad de caminos y en cada paso recursivo unicamente checkeamos que
% exista un camino desde O hasta X. Luego llamamos
% recursivamente pero en ves de pasar O se pasa como parametro X. O sea :
%     O RelacionadoCon X y X RelacionadoCon Y y ... y Z RelacionadoConD => O
%     RelacionadoCon D => hay camino desde O hasta D

caminoSimple(M, D, D, [D]).
caminoSimple(M, O, D, [O|Cs]) :-  E = ruta(O,X,_), member(E, M),
                            select(E,M,Ms),
                            caminoSimple(Ms, X, D, Cs).
caminoSimple(M, O, D, [O|Cs]) :-  E = ruta(X,O,_), member(E, M),
                            select(E,M,Ms),
                            caminoSimple(Ms, X, D, Cs).


% ej 5
% mapa(+M)
%
% En el ej5, se realizan las siguientes validaciones:
% noSimetria: verifica que no haya ruta de A a B y de B a A.
% noReflexiva: verifica que no existe la ruta de A a A.
% todasAlcanzables: Verifica si hay camino desde todas las ciudades a todas.
% Todos estos predicados requieren los parametros instanciados

mapa(M) :- ciudades(M,J), todasAlcanzables(J,M,J), noReflexiva(J,M,M), noSimetria(M,M).

noSimetria([],_).
noSimetria([ruta(C1,C2,_)|Ms], M) :- noRuta(C2,C1,M), noSimetria(Ms,M).

noRuta(_, _, []).
noRuta(C1, C2, [ruta(A,B,_)|Ms])   :- not( (A = C1 , B = C2) ) , noRuta(C1,C2,Ms).

noReflexiva([],_,M).
noReflexiva([C|Cs],[],M)               :- noReflexiva(Cs,M,M).
noReflexiva([C|Cs],[ruta(A,B,_)|Ms],M) :- not(A = B), noReflexiva([C|Cs],Ms,M).

todasAlcanzables([],M,Css).
todasAlcanzables([C|Cs],M,Css) :- alcanzable(C,M), todasAlcanzables(Cs,M,Css).

alcanzable(Ciudad,M) :- setof(C, Camino^caminoSimple(M, C, Ciudad, Camino), _).



% ej 6
% caminoHamiltoniano(+M, +O, +D, -Cs)
%
% Un camino Hamiltoniano es un camino que va desde O hasta D pasando por todas las ciudades
% una unica vez, de modo que es un camino que cumple una condicion especial, esta condicion
% es que la lista de ciudades presentes en el camino es una permutacion de la lista de
% ciudades del grafo.
% Sabiendo eso y valiendonos del ejericio 4 (un predicado que lo satisfacen caminos simples),
% encontrar un camino hamiltoniano es equivalente a encotrar un camino que cumpla con el
% predicado del ejercicio 4 y la condicion antes mencionada.

caminoHamiltoniano(M, O, D, Cs) :-  ciudades(M, Xs),
                                    caminoSimple(M, O, D, Cs),
                                    permutation(Xs, Cs).
                                    
                                    

% ej 7
% caminosHamiltonianos(+M, -Cs)
%
% Aqu� dado un mapa se pretende encontrar un predicado que se satisfaga para todo camino
% que pase por todas las ciudades del mapa. Habiendo hecho el ejericio 6, esto se reduce
% a verificar que la lista de ciudades pasada como parametro (Cs) cumple que para todo
% par de elementos distintos (O, D) de la lista de ciudades del mapa (se usa el predicado
% del ejercicio 1), Cs es un camino hamiltoniano (se usa el predicado del ejercicio 6)
% desde O hasta D.

caminosHamiltonianos(M, Cs) :-  ciudades(M, Ds),
                                 member(O,Ds),
                                 member(D,Ds),
                                 not(O=D),
                                 caminoHamiltoniano( M, O, D, Cs ).
                                 

% ej 8
% caminoMinimo(+M, +O, +D, -Cs, -L)
%
% En este ejercicio hay que encontrar un camino simple minimo (o sea, el camino y su longitud).
% Que una lista de ciudades sea camino simple y minimo equivale a decir que dicha lista
% cumple con el predicado del ejericio 4 y ademas que no existe otro predicado que
% tambien cumple con el ejercicio 4 y ademas tiene una longitud menor. Exactamente
% esto fue lo que describimos en nuestro predicado caminoMinimo.
% Longitud de un camino  es un predicado auxiliar que usamos que describe lo que seria
% la longitud de un camino, esto es, recursivamente sobre la lista de ciudades, ir
% sumando las distancias que figuran en el mapa.

caminoMinimo(M, O, D, Cs, L) :- caminoSimple(M, O, D, Cs),
                                distanciaCamino(M, Cs, L),
                                not( (caminoSimple(M, O, D, Xs),
                                distanciaCamino(M, Xs, R), R < L ) ).

distanciaCamino(M, [], 0).  /* no es un caso valido, pero asumimos como resultado posible 0 */
distanciaCamino(M, [C|[]], 0).
distanciaCamino(M, [O,C|Cs], X) :-  member(ruta(O,C,Z), M),
                                    distanciaCamino(M, [C|Cs], W), X is W+Z.
distanciaCamino(M, [O,C|Cs], X) :-  member(ruta(C,O,Z), M),
                                    distanciaCamino(M, [C|Cs], W), X is W+Z.



% ej 9
% caminoEuleriano(+M, +D, +H, -Cs)
%
% En este ejercicio se pide un predicado que sea verdadero cuando la lista pasada
% como parametro sea un camino euleriano. Un camino euleriano
% es un camino que cumple con la condicion de que todas las rutas estan presentes
% en el. A partir de esta condicion podemos inferir que si
% todas las rutas tienen que estar presentes entonces para cada elemento del mapa,
%  o sea la ruta con su respectivo origen y destino, tiene que
% estar presente en el camino. Pero decir que cierta condicion tiene que ser cierta
% para todo elemento, equivale a decir que no puede existir
% un elemento que no lo cumpla.
% Sabiendo esto nuestro predicado lo unico que describe es que exista un camino de
%  O a D (notar que este camino puede admitir ciudades repetidas)
% y ademas que no pasa que exista un elemento del mapa para el cual el origen o el
%  destino de este no esta presente en nuestro camino (que como
% dijimos antes, equivale a decir que todas las rutas del mapa estan en nuestro
% camino).
% Hay un detalle mas sobre las condiciones que tiene que cumplir la lista de
% ciudades para que sea un camino euleriano. Esto tiene que ver con la
% cantidad de elementos. Sabemos que cada ruta representa un elemento en el mapa,
% de modo que la cantidad de elementos del camino resultante
% tiene que tener al menos una cantidad de elementos equivalentes a los del mapa.
% Si bien lo que se penso sobre la cantidad de elementos es cierto, uno podria
% pensar que cada elemento del mapa involucra dos ciudades y estas
% dos ciudades estan agregadas a la lista resultante, de modo que se podria pensar
%  que la cantidad de elementos de la lista resultante es
% el doble que los del mapa. Pero esto no es cierto. Si vemos en detalle, cada
% elemento del mapa ( E1 = ruta(X,Y,_) ) relaciona dos ciudades, que
% se incluyen en la lista resultante, pero cuando se considere otro elemento del
% mapa ( E2 = ruta(A,B,_) ) va a suceder que A=X o A=Y (o lo mismo para B).
% Por que pasa esto? porque al ser un camino, si se llego hasta esa ruta (E2),
% es porque existe una relacion entre E1 y E2 (pues de otra forma no
% seria camino). De modo que en realidad, no se van a estar agregando 2 elementos
%  (origen y destino de la ruta) por cada ruta sino uno solo, a
% excepcion del primer elemento que se agregue al camino y el ultimo (que podria
%  estar agregando el origen para el caso que se pida un camino
% desde X hasta X, como es el caso del ejemplo del enunciado).
% Este predicado se vale del uso del predicado camino ya que es condicion
% necesaria para nuestro proposito, que exista un camino desde O hasta D.

caminoEuleriano(M,O,D,Cs) :-    caminoSimple(M,O,D,Cs),
                                length(Cs,K),
                                length(M,L),
                                L =< K,
                                not( (E = ruta(X,Y,_), member(E,M), not(member(X,Cs))) ).
caminoEuleriano(M,O,D,Cs) :-    caminoSimple(M,O,D,Cs),
                                length(Cs,K),
                                length(M,L),
                                L =< K,
                                not( (E = ruta(X,Y,_), member(E,M), member(Y,Cs)) ).



% ej 10
% caminosEulerianos(+M, -Cs)
%
% Similarmente al ejercicio 7, aqui se van listando todos los caminos eulerianos
% para cada origen y destino posible.

caminosEulerianos(M, Cs) :- ciudades(M, Ds),
                             member(O,Ds),
                             member(D,Ds),
                             not(O = D),
                             caminoEuleriano( M, O, D, Cs ).
