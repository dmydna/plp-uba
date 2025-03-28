import Prelude hiding (subtract,sum,elem)

-- Ejercicio 1

--max2 :: Ord a => (a,a) -> a
max2 (x,y) | x >= y = x
            | otherwise = y


--max2Curry :: Ord a => a -> a -> (a -> a -> b) 
max2Curry = \x -> \y -> max2 (x,y)

--normaVectorial :: Floating a => (a, a) -> b
normaVectorial (x, y) = sqrt (x^2 + y^2)

--normaVectorialCurry :: Floating a => a -> a -> (a -> a -> b)
normaVectorialCurry = \x -> \y -> normaVectorial (x, y) 


-- funcion currificada
--subtract :: Num a => a -> a -> a 
subtract = flip (-)


-- funcion currificada
--predecedor :: Num a => a -> a
predecedor n = subtract 1 n

-- funcion currificada
--evaluarEnCero :: Num a => (a->b) -> b
evaluarEnCero = \f -> f 0

--dosVeces :: Num a => a -> a
dosVeces = \f -> f * f


-- flip :: (a -> b -> c) -> b -> a -> c
-- recibe una funcion y su argumentos intercambiados, devuelve c

-- map :: (a -> b) -> [a] -> [b]
-- recibe una funcion y una lista, devuelve c


-- flipAll :: [(a->b->c)] -> [(b -> a -> c)]
-- recibe una lista de funciones con arg a y b y los swapea 
flipAll = map flip


--flipRaro :: (b -> a -> c) -> (a -> b -> c)
--flipRaro f a b = flip (flip f) a b
--recibe una funcion con flip aplicado y la desflipea?
flipRaro = flip flip



-- Ejercicio 2

curry :: ((a,b) -> c) -> (a -> b -> c)
curry f = \x -> \y -> f (x,y)


uncurry :: (a->b->c) -> (a,b)-> c
uncurry f = \(x, y) -> f x y


-- esta mal
curryN :: [((a,b) -> c)] -> [(a -> b -> c)]
curry f = \[x] -> \[y] -> f (x, y)




-- Ejercicio 3

--
--foldr funcion valor_inicial lista 

sum :: (Num a) => [a] -> a
sum xs = foldr (+) 0 xs

-- revisar
-- elem 3 [1, 2, 3, 4]
-- (3 == 1) || ( (3 == 2) || ( (3 == 3) || ( (3 == 4) || False ) ) )


elem :: (Eq a) => a -> [a] -> Bool
elem x xs = foldr  (\y -> \ys -> (x == y) || ys)  False xs
-- ys es una lista acumulador

(++) :: [a] -> [a] -> [a]
(++) xs ys = foldr (:) ys xs 


-- foldr (:) [3,4] [1,2]
-- 1 -> 1 : (foldr (:) [3,4] [2]) 
-- 2 -> 2 : (foldr (:) [3,4] [])  
--   -> 2 : [3,4]                
--   -> 1 : [2,3,4]              



filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\y -> \ys -> if f y then y : ys else ys) []
-- ys es una lista acumulador


-- foldr (\y a -> if even y then y : a else a) [] [1,2,3,4,5]
-- 1 -> if even 1 then 1 : acc else acc  --> acc = []
-- 2 -> if even 2 then 2 : acc else acc  --> acc = [2]
-- 3 -> if even 3 then 3 : acc else acc  --> acc = [2]
-- 4 -> if even 4 then 4 : acc else acc  --> acc = [4,2]
-- 5 -> if even 5 then 5 : acc else acc  --> acc = [4,2]



map :: (a -> b) -> [a] -> [b]
map = foldr (\y -> ys -> f y : ys) []
-- ys es una lista acumulador

