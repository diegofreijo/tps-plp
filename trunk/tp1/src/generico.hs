module Generico where

type Sentence s = s -> s
type Combinator s = Sentence s -> Sentence s -> Sentence s
type Predicate s = s -> Bool

-- Ejercicio 1
(>->) :: Combinator s 	
s >-> y = y.s 					-- Como en esta funcion, al parametro primero hay q aplicarle s y despues y, es utiliza el operador "."
								-- que primero aplica al parametro la funcion de la derecha y luego al resultado de esto, la de la izquierda.

-- Ejercicio 2
(<-<) :: Combinator s 	
(<-<) = flip (>->) 					-- Similar al anterior, pero esta vez se ejecuta primero el segundo combinador y luego el primero. Se utilizo
									-- flip aplicado al ejercicio 1 para reutilizar codigo, ya que flip invierte los parametros dados, haciendo que
									-- "a -> b -> c" se transforme en "b -> a -> c"

-- Ejercicio 3
secAbs :: Combinator s -> [Sentence s] -> Sentence s
secAbs c xs = foldr c identity xs	-- Esta funcion lo que hace es aplicar a un parametro dado, una sucesion de sentencias de acuerdo con el combinador 
									-- dado. Al utilizas foldr, se va aplicando el combinador a la lista de sentencias recursivamente, para finalmente 
									-- aplicarlo sobre el parametro.

-- Ejercicio 4
sec :: [Sentence s] -> Sentence s	
sec = secAbs (>->) 					-- Similar al anterior, solo que en este caso, el combinador esta "hardcodeado" en >-> para ejecucion secuncial.


-- Ejercicio 5
(><) :: Combinator s -> Sentence s -> Combinator s 
c >< s = (\ s1 s2 -> (s >-> s) >-> c s1 s2)
									-- En este caso, el combinador dado ejecuta dos veces la sentencia s antes de aplicar las dos sentencias s1 y s2
									-- en secuencia.


-- Ejercicio 6
bifurcacion :: Predicate r -> Combinator r
bifurcacion p = (\ s1 s2 a -> if (p a) then s1 a else s2 a)
									-- Se utilizo una funcion lambda para claridad del codigo. Dependiendo de la variable booleana resultante del
									-- predicado a, se realiza una sentencia especifica. Si a es "True", entonces se ejecuta la primer sentencia
									-- pasada como parametro. En cambio, si es "False", se ejecuta la segunda.

									
-- Sentencias utilizadas para probar los operadores genericos
identity :: Sentence s 
identity x = x				-- Identidad

doble :: Sentence Int
doble x = x * 2				-- Multiplica por dos el parametro

triple :: Sentence Int	
triple x = x * 3			-- Multiplica por tres el parametro

restaUno :: Sentence Int	
restaUno x = x - 1			-- Resta uno al parametro

trueSentence :: Predicate s 
trueSentence r = True 		-- Predicado True

falseSentence :: Predicate s
falseSentence r = False		-- Predicado False


{- Ejemplos :
	
	- (restaUno >-> doble) 3 = 4
	- (restaUno <-< doble) 3 = 5
	- ( secAbs (<-<) [doble, restaUno] ) 2 = 2
	- (sec [doble, restaUno]) 2 = 3
	- ((<-<) >< doble) restaUno doble 5	 = 39
	
-}
