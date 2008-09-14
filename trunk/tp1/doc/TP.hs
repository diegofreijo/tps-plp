import Char

--- Definición de tipo para sentencia
type Sentence s = s -> s

--- Definición de tipo para el Combinator
type Combinator s = Sentence s -> Sentence s -> Sentence s

----------------------------------------------------------------------
-- Parte 1: OPERADORES GENERICOS
----------------------------------------------------------------------

-- ! Combinador de sentencias
(>->) :: Combinator s

-- ! Combinador de sentencias inverso
(<-<) :: Combinator s

-- ! Regla de la secuencia ahora usando un combinador de secuencia
secAbs :: Combinator s -> [Sentence s] -> Sentence s

-- ! Regla de la secuencia estandar, que usa el combinador (>->)
sec :: [Sentence s] -> Sentence s

-- ! Generador de combinadores
(><) :: Combinator s -> Sentence s -> Combinator s

-- ! Combinator de bifurcación
bifurcacion :: (s -> Bool) -> Combinator s

----------------------------------------------------------------------
-- Parte 2: Lenguaje PP
----------------------------------------------------------------------

--- Definición de una sentencia PP
type PPState = [Char]
type PPSentence = Sentence PPState

-- ! Regla shift right
shr :: PPSentence

-- ! Regla shift left
shl :: PPSentence

-- ! Regla duplicate
dup :: PPSentence

-- ! Sentencia del for para iterar n veces un bloque de código
forN :: Int -> [PPSentence] -> PPSentence

----------------------------------------------------------------------
-- Ejemplos PP
----------------------------------------------------------------------

-- Estos son algunos ejemplos, no necesariamente cubren todos los casos
-- ¡No se puede asumir que alcanza con que funcionen estos ejemplos!

progPP1 = [shr, dup, shl]
testPP1 = sec progPP1 "ba"
progPP2 = [shr, dup, shr, forN 3 [shl], shl, forN 2 [reduce,shl,shr]]
testPP2 = sec progPP2 "hola"

----------------------------------------------------------------------
-- Parte 3: Lenguaje Jay
----------------------------------------------------------------------

--- Definición de sentencias y expresiones Jay
type JayState = [(String, Int)]
type JayExpression t = JayState -> t
type JaySentence = Sentence JayState

--- Variables
var :: String -> JayExpression Int
var varName s = maybe 0 id (lookup varName s)

--- Constantes
con :: Int -> JayExpression Int
con c s = c

-- ! Regla de asignación
asig :: String -> JayExpression Int -> JaySentence

-- ! Evaluador de operadores
op :: (a -> b -> c) -> JayExpression a -> JayExpression b -> JayExpression c

-- ! Regla del if
rIf :: JayExpression Bool -> [JaySentence] -> [JaySentence] -> JaySentence

-- ! Regla del while
rWhile :: JayExpression Bool -> [JaySentence] -> JaySentence

----------------------------------------------------------------------
-- Ejemplos Jay
----------------------------------------------------------------------

-- Estos son algunos ejemplos, no necesariamente cubren todos los casos
-- ¡No se puede asumir que alcanza con que funcionen estos ejemplos!

progJay1 = [asig "n" (con 5),
            asig "res" (con 1), 
            rWhile (op (>) (var "n") (con 1)) [
              asig "res" (op (*) (var "res") (var "n")),
              asig "n" (op (-) (var "n")  (con 1))
            ]
           ]

progJay2 = [asig "x" (con 1),
            rIf (op (>) (var "x") (con 10))
            [asig "x" (con 3)]
            [asig "x" (op (+) (var "x") (con 1))]
           ]

testJay1 = sec progJay1 []

