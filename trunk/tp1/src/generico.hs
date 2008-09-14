module Generico where

type Sentence s = s -> s
type Combinator s = Sentence s -> Sentence s -> Sentence s
type Predicate s = s -> Bool

-- generics operators
-- Ejercicio 1
(>->) :: Combinator s 	
s >-> y = y.s 					-- s es algo q toma un parametro. Lo mismo para y


-- Ejercicio 2
(<-<) :: Combinator s 	
(<-<) = flip (>->) 					-- s es algo q toma un parametro. Lo mismo para y


-- Ejercicio 3
secAbs :: Combinator s -> [Sentence s] -> Sentence s
secAbs c xs = foldr c identity xs


-- Ejercicio 4
sec :: [Sentence s] -> Sentence s
sec xs = secAbs (>->) xs


-- Ejercicio 5
(><) :: Combinator s -> Sentence s -> Combinator s
c >< s = (\ s1 s2 -> (s >-> s) >-> c s1 s2)


-- Ejercicio 6
bifurcacion :: Predicate r -> Combinator r
bifurcacion p = (\ s1 s2 str -> if (p str) then s1 str else s2 str)

--especific sentences	
identity :: Sentence s 
identity x = x

doble :: Sentence Int
doble x = x * 2

triple :: Sentence Int
triple x = x * 3

restaUno :: Sentence Int
restaUno x = x - 1

trueSentence :: Predicate s
trueSentence r = True 

falseSentence :: Predicate s
falseSentence r = False


{- Ejemplos :
	
	- (restaUno >-> doble) 3 = 4
	- (restaUno <-< doble) 3 = 5
	- ( secAbs (>->) [doble, restaUno] ) 2
	- (sec [doble, restaUno]) 2
	- ((<-<) >< doble) restaUno doble 5	
	
-}
