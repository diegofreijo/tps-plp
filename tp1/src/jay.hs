module Jay where
import Generico

type JayState = [(String, Int)]
type JayExpression t = JayState -> t
type JaySentence = Sentence JayState

con :: Int -> JayExpression Int
con c s = c

var :: String -> JayExpression Int
var varName s = maybe 0 id (lookup varName s)

-- Ejercicio 11
{--
	- No se puede usar bifurcacion porque bifurcacion toma dos Sentences, cada una de ellas toma el mismo parametro que toma bifurcacion y lo 
	devuelve transformado. El problema es que a bifurcacion necesitamos pasarle una 3-upla (key, value, estado), pero no podemos hacer una Sentece
	que devuelva eso porque segun el tipo de asig esta tiene que devolver un estado => no se puede usar bifurcacion porq no dan los tipos
--}
asig :: String -> JayExpression Int -> JaySentence
--asig nombre valor estado = [(nombre, valor estado)] ++ estado 
--asig nombre valor estado = bifurcacion varPredicate (\t -> [(fst t, fst.snd t snd.snd t)] ++ snd.snd t) (nombre,(valor,estado)) (\t -> concatMap () )
asig nombre valor estado = 	if (var nombre estado == 0) then
								[(nombre, valor estado)] ++ estado 
							else
								{-- FIXME
									- La linea correcta para el else es "1", pero no anda ( la "2" es un ejemplo simplificado )
									  Lo que quiero hacer es recorrer la lista, para el elemento que coincida, le cambio la segunda
									  coordenada a la tupla por el value que le paso, que representa el nuevo que se le esta asignando.
									  No entiendo por q no anda la puta madre....
									NOTE : como esta ahora, el comportamiento es:
										- si no esta la variable en el estado, lo agrego
										- si esta, no le cambio el valor, se queda con el que estaba
								--1 concatMap (\key value elem -> if (key == fst elem) then (key, value) else elem) estado
								--2 concatMap (\x -> x) estado
								--}
								estado		-- puse esto para que compile



{--
pruebaBif :: String -> JaySentence -> JayExpression Int 
pruebaBif nombre valor estado = ( bifurcacion (\tup -> var (fst tup) (snd tup) == 0) (nombre,estado) ) 5 6 
--}


{--
varPredicate :: Predicate (String,(Int,JayState))
varPredicate tup = var (fst tup) (snd.snd tup) == 0
--}

{--
isIn :: Predicate (String, JaySentence)
isIn tup = (\tup -> var (fst tup) (snd tup) == 0) 
--}

test :: String -> JayExpression Int 
test s j = (\tup -> var (fst tup) (snd tup) ) (s,j)


{--
pruebaBif :: String -> JayExpression Int -> JaySentence -> Int
pruebaBif nombre valor estado = bifurcacion ((\tup -> (var (fst tup) (snd tup) == 0)) (nombre,estado)) 5 6-}
--}

-- Ejercicio 12
op :: (a -> b -> c) -> JayExpression a -> JayExpression b -> JayExpression c
op operador op1 op2 estado = operador (op1 estado) (op2 estado)


-- Ejercicio 13
rIf :: JayExpression Bool -> [JaySentence] -> [JaySentence] -> JaySentence
rIf condicion rama_then rama_else = bifurcacion condicion (sec rama_then) (sec rama_else)


-- Ejercicio 14
rWhile :: JayExpression Bool -> [JaySentence] -> JaySentence
rWhile condicion cuerpo estado = 	head (reverse 									-- 7. Y lo que termino devolviendo es el ultimo estado.
									(
										fst (break ([("",0)] ==)					-- 6. Pero solo quiero la lista de resultados intermedios hasta antes que comienze a dar nulos
										(
											iterate 								-- 4. Todo esto es repetido infinitas veces
											(
												bifurcacion condicion				-- 1. Mientras se cumpla la guarda...
												(sec cuerpo)						-- 2. ejecuto el cuerpo del bucle.
												(\s -> [("",0)])					-- 3. Y cuando no se cumpla mas, comienzo a devolver estados nulos
											) estado								-- 5. Comenzando por el estado inicial (previo al bucle)
										))										
									))
							

{-
Ejemplos

	Ejercicio 12:
		op (==) (var "foo") (var "bar") [("foo",28),("bar",42),("quux",1)]		->	false
		op (==) (var "foo") (con 28) [("foo",28),("bar",42),("quux",1)]			-> 	true

	Ejercicio 13:
		rIf (op (>) (var "x") (var "y")) [\s -> s++[("max",var "x" s)]] [\s -> s++[("max",var "y" s)]] [("x",15),("y",12)]	->	[("x",15),("y",12),("max",15)]

	Ejercicio 14:
		rWhile (op (<) (var "hola") (con 15)) [\s -> s ++ [("hola",16)]] [("chau",20)]		-> 	[("chau",20),("hola",16)]
		
-}
