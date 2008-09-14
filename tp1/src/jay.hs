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