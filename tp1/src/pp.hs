module PP where
import Generico

type PPState = [Char]
type PPSentence = Sentence PPState

-- Ejercicio 7
shl :: PPSentence
shl s = tail (s ++ [(head s)])						-- shl s equivale a apendear el head(s) y quedarse con el tail de la secuencia

-- Ejercicio 8
shr :: PPSentence
shr s = sec (take (length s - 1) (repeat shl)) s	-- con la idea de reutilizar codigo, pensamos shr s = (shl s) repeteidas len s - 1 veces

-- Ejercicio 9
dup :: PPSentence
dup = foldr (\x xs -> [x] ++ [x] ++ xs) []			-- en cada iteracion, duplicamos el elemento actual (head de la lista) y se apendea a la lista resultante

-- Ejercicio 10
forN :: Int -> [PPSentence] -> PPSentence
forN n xs = sec (flatten (take n (repeat xs)))		-- primero aplanamos la lista de listas y luego reutilizamos la funcion 'sec'

flatten :: [[a]] -> [a]
flatten = concatMap (\x -> x)						-- aplana una lista de listas a una lista



----------------------------------------------------
-- Ejemplos
{-
	shl "abc"  				->  "bca"
	shr "abcd"  			->  "dabc"
	dup "abc"				->  "aabbcc"
	forN 2 [shl, dup] "abc" -> "bbccccaaaabb"

-}
