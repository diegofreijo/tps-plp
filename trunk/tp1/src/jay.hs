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
{-asig :: String -> JayExpression Int -> JaySentence
asig nombre valor estado = [(nombre, valor estado)] ++ estado

pruebaBif :: String -> JayExpression Int -> JaySentence -> Int
pruebaBif nombre valor estado = bifurcacion ((\tup -> (var (fst tup) (snd tup) == 0)) (nombre,estado)) 5 6-}


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
