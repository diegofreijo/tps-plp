
type Sentence s = s -> s
type Combinator s = Sentence s -> Sentence s -> Sentence s
type Predicate s = s -> Bool

-- generics operators
-- Ejercicio 1
(>->) :: Combinator s 	
s >-> y = y.s 					-- s es algo q toma un parametro. Lo mismo para y


-- Ejercicio 2
(<-<) :: Combinator s 	
s <-< y = s.y 					-- s es algo q toma un parametro. Lo mismo para y


-- Ejercicio 3
secAbs :: Combinator s -> [Sentence s] -> Sentence s
secAbs c [] 	= identity
secAbs c (x:xs) = c x (secAbs c xs)


-- Ejercicio 4
sec :: [Sentence s] -> Sentence s
sec [] 		= identity
sec (x:xs)	= (x >-> sec xs)

-- Ejercicio 5
(><) :: Combinator s -> Sentence s -> Combinator s
c >< s = auxiliar c s

auxiliar :: Combinator s -> Sentence s -> Sentence s -> Sentence s -> Sentence s
auxiliar c s1 s2 s3 = c s2 s3.c s1 s1			-- s1 es la que hay que anteponer


-- Ejercicio 6
bifurcacion :: Predicate r -> Combinator s
bifurcacion p = bifAux p

bifAux :: Predicate r -> Sentence s -> Sentence s -> Sentence s			-- NO ANDA!!
bifAux p s1 s2 = if (p) then
					s1
				 else
				 	s2

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
	- ((<-<) >< doble) restaUno doble 5	
	
-}


