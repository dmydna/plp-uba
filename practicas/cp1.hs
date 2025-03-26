curry :: ((a,b) -> c) -> (a -> b -> c)
curry f x y = f (x,y)
curry f = \x -> \y -> f (x,y)
curry f = \x -> f (x,y)


uncurry :: (a->b->c) -> (a,b)-> c
uncurry f = \ (x,y) -> f x y

doble x = (prod 2) x
doble = prod 2

triple :: Float -> Float
triple = (*3)

esMayorDeEdad :: Int -> Bool
esMayorDeEdad = (>) 17
esMayorDeEdad = (17<)


(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g c = f (g x)
(.) f g = \x -> f (g x)

flip :: (a -> b -> c) -> (b -> a -> c)
flip f = \x -> \y -> f y x
flip f = \x y -> f y x
flip f x y = f y x


f (g (h x))
f $ g $ h x


($)::(a -> b) -> a -> b
($) f = f


const :: a -> (b -> a)
const x _ = x
const x = \_ -> x
const = \x -> \_-> x

flip ($) 0 f
    = flip ($) f 0
    = f 0

(flip ($)) 0


((==0)).(flip `mod` 2)) 6
   = (==0)((flipmod 2) 6)
   = (==0)(mod 6 2)
   = True

maximo :: Ord a => [a] -> a
maximo [x] = x
maximo (x:xs) = if x > maximo xs then x else maximo xs

listaMasCorta :: [[a]] -> [a]
listaMasCorta [xs] = xs
listaMasCorta (xs:xss) = if length xs < length (listaMasCorta xss) then xs else listaMasCorta xss


mejorSegun :: (a -> b -> Bool) -> [a] -> a
mejorSegun _ [x] = x
mejorSegun p (x:xs) = if f x (mejorSegun p xs) then x else mejorSegun p xss

maximo:: Ord a => [a] -> a
maximo = mejorSegun (<)

listaMasCorta :: [[a]] -> [a]
listaMasCorta = mejorSegun (\x y -> length x < length y)


filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs


deLongitudN :: Int -> [[a]] -> [[a]]
deLongitudN n = filter (\x -> length x == n)


soloPuntosFijosEnN :: Int -> [Int -> Int] -> [Int -> Int]
soloPuntosFijosEnN = filter (if -> f n == n)

map :: (a->b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

reverseAnidados :: [[Char]] -> [[Char]]
reverseAnidados xs = reverse (map reverse xs)
reverseAnidados = \xs -> reverse (map reverse xs)
reverseAnidados = reverse (map reverse)

paresCuadrados :: [Int] -> [Int]
paresCuadrados xs = map (\x -> if even x then x*x else x)

foldr :: (a->b->c) -> b -> [a] -> b
foldr _ z [] = z
foldr z (x:xs) = f x (foldr f x xs)


suma :: [Int] -> Int
suma [] = 0
suma (x:xs) = x + suma xs


-- no recuerdo que era rec  
sumar = foldr (\x rec -> x + rec) 0
sumar = foldr (+) 0

-- no recuerdo que era rec  
filter :: (a->Bool) -> [a] -> [a]
filter p = foldr (\x rec -> if p x then x : rec else rec) []

-- se puso a explicar y no escribio la solucion.
listaComp:: (a->Bool) -> (a->b) -> [a] -> [b]
listaComp p f xs = [f x |x -> xs p x]




