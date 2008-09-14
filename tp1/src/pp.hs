module PP where
import Generico

type PPState = [Char]
type PPSentence = Sentence PPState

-- Ejercicio 7
shl :: PPSentence
shl s = tail (s ++ [(head s)])

-- Ejercicio 8
shr :: PPSentence
shr s = sec (take (length s - 1) (repeat shl)) s

-- Ejercicio 9
dup :: PPSentence
dup = foldr (\x xs -> [x] ++ [x] ++ xs) []

-- Ejercicio 10
forN :: Int -> [PPSentence] -> PPSentence
forN n xs = sec (flatten (take n (repeat xs)))

flatten :: [[a]] -> [a]
flatten = concatMap (\x -> x)



----------------------------------------------------
-- Ejemplos
{-
	shl "abc"  				->  "bca"
	shr "abcd"  			->  "dabc"
	dup "abc"				->  "aabbcc"
	forN 2 [shl, dup] "abc" -> "bbccccaaaabb"

-}