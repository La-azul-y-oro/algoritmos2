module Practica0 where

import Data.List

{-
1) Los siguientes códigos tienen errores, cargar el archivo 20.Practica.0.hs en el interprete de Haskell
GHCi, leer los mensajes de error y corregirlos hasta que el archivo se cargue correctamente.
-}

-- a)
regla b = case b of
    True -> "Quedate en Casa"
    False -> "Qudate en Casa"

regla' b = if b then "Quedate en Casa" else "Qudate en Casa"

-- b)
case' [x] = []
case' (x:y:xs) = y : case' (x:xs)
case' [] = []
-- estaba usando palabra reservada (case)

-- c)
map' f []        =  []
map' f (x:xs)     =  f x : map' f xs
--Ambigüedad entre funciones, al importar Data.List, ya trae la funcion map, por lo cual aqui se cambia el nombre

-- d)
listNumeros' = '1' : '2' : 'a' : []
listNumeros_bis = ('1':['2']) ++ 'a' : []
--Sacar parentesis y convertir todo a tipo char
-- el operador cons ( : ) requiere un elemento (a la izq) y una lista a la derecha 

-- e)
[]     ++! ys = ys
(x:xs) ++! ys = x : xs ++! ys
-- no tiene error, tal vez se podria cuestionar como se denomina el operador usando el !
-- en efecto realiza lo mismo que usar la funcion de concatenacion ++

-- f)
addToTail x xs = map (+x) (tail xs)
-- requiere los parentesis para que las operaciones queden bien definidas, es decir, 
--por un lado la generacion de la lista de cola (tail) y por otro el separar la operacion a aplicar sobre estos elementos, para el caso: (+x) 

-- -- g)
listmin xs = head (sort xs)
listmin' xs = (head . sort) xs
-- head . sort xs espera dos funcines a cada lasdo del "." y sort xs es una lista ordenada 

-- h) (*)
smap f [] = []
smap f [x] = [f x]
smap f (x:xs) = f x : smap f xs
-- la funcion smap espera una f y una lista, es incorrecto pasarle smap (smap f) xs

-- 2. Definir las siguientes funciones y determinar su tipo:

-- a) five, que dado cualquier valor, devuelve 5
five :: a -> Int
five _ = 5

-- b) apply, que toma una función y un valor, y devuelve el resultado de
-- aplicar la función al valor dado
apply f = f -- esta seria currificada
apply' f x = f x

-- c) identidad, la función identidad
identidad:: a -> a
identidad x = x 

-- d) first, que toma un par ordenado, y devuelve su primera componente
first :: (a,b) -> a
first (x, _) = x

-- e) derive, que aproxima la derivada de una función dada en un punto dado
derive :: Fractional a => (a -> a) -> a -> a
derive f x = (f (x+h) - f x) / h
              where h = 0.0000001

-- f) sign, la función signo
sign:: Int -> Int
sign 0 = 1
sign x = if x > 0 then 1
                  else -1

-- g) vabs, la función valor absoluto (usando sign y sin usarla)
vabs :: Int -> Int
vabs x | x < 0 = x*(-1)
       | otherwise = x

vabs' :: Int -> Int
vabs' x = (sign x) * x

-- h) pot, que toma un entero y un número, y devuelve el resultado de
-- elevar el segundo a la potencia dada por el primero
pot :: (Integral b, Num a) => b -> a -> a
pot x y = y^x
-- pot x y = y**x 
  -- ** en caso de operar con flotantes, se debe utilizar ** para realizar la potencia

-- i) xor, el operador de disyunción exclusiva
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _ = False

-- j) max3, que toma tres números enteros y devuelve el máximo entre llos
max3 :: Int -> Int -> Int -> Int
max3 x y z | x > y && x > z = x
           | y > z = y 
           | otherwise = z

-- k) swap, que toma un par y devuelve el par con sus componentes invertidas
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- {-
-- 3) Definir una función que determine si un año es bisiesto o no, de
-- acuerdo a la siguiente definición:

-- año bisiesto 1. m. El que tiene un día más que el año común, añadido al mes de febrero. Se repite
-- cada cuatro años, a excepción del último de cada siglo cuyo número de centenas no sea múltiplo
-- de cuatro. (Diccionario de la Real Academia Espaola, 22ª ed.)

-- ¿Cuál es el tipo de la función definida?
-- -}

--kk a = (mod a 400 == 0) || (mod a 400 == 0) && (not(mod a 100 == 0))

esBisiesto :: Int -> Bool
esBisiesto anio = (esMultiploDe 4 anio) && ((not (esMultiploDe 100 anio)) || (esMultiploDe 400 anio))

esMultiploDe :: Int -> Int -> Bool
esMultiploDe divisor dividendo = (dividendo `mod` divisor) == 0

-- {-
-- 4) Dar al menos dos ejemplos de funciones que tengan cada uno de los siguientes tipos:
-- a) (Int -> Int) -> Int

-- increment es auxiliar para probar las funciones siguientes
increment :: Int -> Int 
increment x = x + 1

applyAndSum :: (Int -> Int) -> Int
applyAndSum f = f 3

applyToOneAndAddTen :: (Int -> Int) -> Int
applyToOneAndAddTen f = f 1 + 10

-- b) Int -> (Int -> Int)
-- Haskell generaliza las funciones cada vez que puede

-- Analizar este caso y ver como usarlo, en firma OK, en uso ni idea
-- ejemplo2:: Int -> (Int -> Int)
-- ejemplo2 x = (\y -> x * y)
restaCurr :: Int -> (Int -> Int)
restaCurr x = (x-)

divCurr :: Int -> (Int -> Int)
divCurr x = (`div` x)

-- c) (Int -> Int) -> (Int -> Int)
ejemploC :: (Int -> Int) -> (Int -> Int)
ejemploC f = \x -> f (f x)

incrementarResultado :: (Int -> Int) -> (Int -> Int)
incrementarResultado f x = (f x) + 1

-- d) Int -> Bool
isZero :: Int -> Bool
isZero x = x == 0

isEven :: Int -> Bool
isEven x = x `mod` 2 == 0

-- e) Bool -> (Bool -> Bool)
boolEjem :: Bool -> (Bool -> Bool)
boolEjem x = (\y -> y && x)

boolCurr :: Bool -> (Bool -> Bool)
boolCurr x = (x ||) 

-- f) (Int,Char) -> Bool
parIntChar :: (Int, Char) -> Bool
parIntChar (x, y) = x == 0 && y == 'a'

parIntChar' :: (Int, Char) -> Bool
parIntChar' (x, y) = if y == 'a' then even x else even (x+1) 

-- g) (Int,Int) -> Int
parIntInt :: (Int, Int) -> Int
parIntInt (x, y) = x `div` y

parIntInt' :: (Int, Int) -> Int
parIntInt' (x, y) = x + y

-- h) Int -> (Int,Int)
div2 :: Int -> (Int, Int)
div2 x = (div x 2, mod x 2)

div2' :: Int -> (Int, Int)
div2' x = (div x x,x)

-- i) a -> Bool
esNulo :: Eq a => a -> Bool
esNulo x = x == x

alwaysTrue :: p -> Bool
alwaysTrue _ = True

-- j) a -> a

myIdentidad :: a -> a
myIdentidad x = x

-- -}


-- {-
-- 5) Definir las siguientes funciones usando listas por comprensión:

-- a) 'divisors', que dado un entero positivo 'x' devuelve la
-- lista de los divisores de 'x' (o la lista vacía si el entero no es positivo)
divisors :: Int -> [Int]
divisors x | x <= 0 = []
           | otherwise = [a | a <- [1..x], x `mod` a == 0]

-- b) 'matches', que dados un entero 'x' y una lista de enteros descarta
-- de la lista los elementos distintos a 'x'
matches :: Int -> [Int] -> [Int]
matches x xs = [y | y <- xs, x==y]

-- c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
-- '(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
-- donde 0 <= a, b, c, d <= 'n'
cuadrupla :: Int -> [(Int, Int, Int, Int)]
cuadrupla n = [(a, b, c, d) | a <- [0..n], b <- [0..n], c <- [0..n], d <- [0..n], a^2 + b^2 == c^2 + d^2]

-- (d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
-- 'xs' sin elementos repetidos
-- unique :: [Int] -> [Int]
unique :: [Int] -> [Int]
unique xs = [x | (x, i) <- zip xs [0..], x `notElem` (take i xs)]
-- -}

-- [5,5,3,4,5,1]

-- 5 [] - 5
-- 5 [5] - x
-- 3 [5 5] - 3
-- 4 [5 5 3 ] -4
-- 5 [5 5 3 4 ] -x 
-- 1 [ 5 5 3 4 5] - 1

-- {-
-- 6) El producto escalar de dos listas de enteros de igual longitud
-- es la suma de los productos de los elementos sucesivos (misma
-- posición) de ambas listas.  Definir una función 'scalarProduct' que
-- devuelva el producto escalar de dos listas.

-- Sugerencia: Usar las funciones 'zip' y 'sum'. -}
scalarProduct :: [Int] -> [Int] -> Int
scalarProduct xs ys = sum [ x*y | (x,y) <- zip xs ys]

-- {-
-- 7) Sin usar funciones definidas en el
-- preludio, defina recursivamente las siguientes funciones y
-- determine su tipo más general:

-- a) 'suma', que suma todos los elementos de una lista de números
suma :: Num a => [a] -> a
suma [] = 0
suma (x:xs) = x + suma xs 

-- b) 'alguno', que devuelve True si algún elemento de una
-- lista de valores booleanos es True, y False en caso
-- contrario

alguno :: [Bool] -> Bool
alguno [] = False
alguno (x:xs) = x || alguno xs

-- c) 'todos', que devuelve True si todos los elementos de
-- una lista de valores booleanos son True, y False en caso
-- contrario
todos :: [Bool] -> Bool
todos [] = True
todos (x:xs) = x && todos xs

-- d) 'codes', que dada una lista de caracteres, devuelve la
-- lista de sus ordinales
--- TOMADO de la resolucion del profesor
code c = buscar c (zip (['a'..'m']++['ñ']++['o'..'z']) [1..])
buscar c [] = error "El caracter no tiene ordinal"
buscar c ((x,i):xs) = if x == c then i else buscar c xs


codes:: [Char] -> [Int]
codes [] = []
codes (x:xs) = code x : codes xs

-- e) 'restos', que calcula la lista de los restos de la
-- división de los elementos de una lista de números dada por otro
-- número dado

restos :: [Int] -> Int -> [Int]
restos [] _ = []
restos (x:xs) n = (x `mod` n) : restos xs n

-- f) 'cuadrados', que dada una lista de números, devuelva la
-- lista de sus cuadrados
cuadrados :: Num a => [a] -> [a]
cuadrados [] = []
cuadrados (x:xs) = x^2 : cuadrados xs

-- g) 'longitudes', que dada una lista de listas, devuelve la
-- lista de sus longitudes

longitudes :: [[a]] -> [Int]
longitudes [] = []
longitudes (x:xs) = length x : longitudes xs


-- h) 'orden', que dada una lista de pares de números, devuelve
-- la lista de aquellos pares en los que la primera componente es
-- menor que el triple de la segunda
orden :: (Num a, Ord a) => [(a,a)] -> [(a,a)] 
orden [] = []
orden ((x,y) : xs) | x < y*3 = (x,y) : orden xs
                   | otherwise = orden xs
                   
-- i) 'pares', que dada una lista de enteros, devuelve la lista
-- de los elementos pares

pares :: [Int] -> [Int]
pares [] = []
pares (x:xs) | x `mod` 2 == 0 = x : pares xs
             | otherwise = pares xs

-- j) 'letras', que dada una lista de caracteres, devuelve la
-- lista de aquellos que son letras (minúsculas o mayúsculas)
--- TOMADO de la resolucion del profesor
letras::[Char] -> [Char]
letras [] = []
letras (x:xs) = if x `elem` ['a'..'z'] then x: letras xs
                                       else letras xs


-- k) 'masDe', que dada una lista de listas 'xss' y un
-- número 'n', devuelve la lista de aquellas listas de 'xss'
-- con longitud mayor que 'n' -}
masDe :: [[a]] -> Int -> [[a]]
masDe [] _ = []
masDe (xs:xss) n | length xs >= n = xs : masDe xss n
                 | otherwise = masDe xss n

-- {-
-- 8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
-- ver su definición en https://hoogle.haskell.org/
-- -}
-- a) 'suma', que suma todos los elementos de una lista de números
suma' :: Num a => [a] -> a
suma' [] = 0
suma' (x:xs) = foldr (+) x xs 

-- b) 'alguno', que devuelve True si algún elemento de una
-- lista de valores booleanos es True, y False en caso
-- contrario

alguno' :: [Bool] -> Bool
alguno' [] = False
alguno' (x:xs) = foldr (||) x xs

-- -- c) 'todos', que devuelve True si todos los elementos de
-- -- una lista de valores booleanos son True, y False en caso
-- -- contrario
todos' :: [Bool] -> Bool
todos' [] = True
todos' (x:xs) = foldr (&&) x xs

-- -- d) 'codes', que dada una lista de caracteres, devuelve la
-- -- lista de sus ordinales
-- --- TOMADO de la resolucion del profesor
-- code c = buscar c (zip (['a'..'m']++['ñ']++['o'..'z']) [1..])
-- buscar c [] = error "El caracter no tiene ordinal"
-- buscar c ((x,i):xs) = if x == c then i else buscar c xs


-- codes:: [Char] -> [Int]
-- codes [] = []
-- codes (x:xs) = code x : codes xs

-- e) 'restos', que calcula la lista de los restos de la
-- división de los elementos de una lista de números dada por otro
-- número dado

restos' :: [Int] -> Int -> [Int]
restos' [] _ = []
restos' xs n = map (`mod` n) xs

-- -- f) 'cuadrados', que dada una lista de números, devuelva la
-- -- lista de sus cuadrados
cuadrados' :: Num a => [a] -> [a]
cuadrados' [] = []
cuadrados' xs = map (^2) xs

-- g) 'longitudes', que dada una lista de listas, devuelve la
-- lista de sus longitudes

longitudes' :: [[a]] -> [Int]
longitudes' [] = []
longitudes' xs = map length xs

-- -- h) 'orden', que dada una lista de pares de números, devuelve
-- -- la lista de aquellos pares en los que la primera componente es
-- -- menor que el triple de la segunda
orden' :: (Num a, Ord a) => [(a,a)] -> [(a,a)] 
orden' [] = []
-- orden' xs = filter isMinor xs
--             where isMinor (x,y) = x < y*3
orden' xs = filter (\(x,y) -> x < y*3) xs
                   
-- i) 'pares', que dada una lista de enteros, devuelve la lista
-- de los elementos pares

pares' :: [Int] -> [Int]
pares' [] = []
pares' xs = filter (\x -> x `mod` 2 == 0) xs


-- -- j) 'letras', que dada una lista de caracteres, devuelve la
-- -- lista de aquellos que son letras (minúsculas o mayúsculas)
letras' :: [Char] -> [Char]
letras' [] = []
letras' xs = filter (\x -> x `elem` ['a'..'z']) xs

-- -- k) 'masDe', que dada una lista de listas 'xss' y un
-- -- número 'n', devuelve la lista de aquellas listas de 'xss'
-- -- con longitud mayor que 'n' -}
masDe' :: [[a]] -> Int -> [[a]]
masDe' [] _ = []
masDe' xss n = filter (\xs -> length xs >= n) xss


----- SOBRE FOLDL (left) y FOLDR (right)
{- Diferencias Clave:

Orden de Aplicación: foldr procesa la lista de derecha a izquierda, mientras que foldl lo hace de izquierda a derecha.
Función Binaria: En foldr, la función binaria toma un elemento de la lista y el acumulador. En foldl, toma el acumulador y un elemento de la lista.
Asociatividad: foldr es adecuado para funciones binarias que son asociativas hacia la derecha, y foldl para las que son asociativas hacia la izquierda.
Pereza: foldr puede trabajar con listas infinitas debido a su naturaleza perezosa. foldl no puede trabajar con listas infinitas ya que intenta evaluar la lista completa.

Ejemplos para Ilustrar la Diferencia
  foldr (:) [] [1, 2, 3]  -- Resultado: [1, 2, 3]
  foldr (\x acc -> acc ++ [x]) [] [1, 2, 3]  -- Resultado: [3, 2, 1]

  foldl (flip (:)) [] [1, 2, 3]  -- Resultado: [3, 2, 1]
  foldl (\acc x -> x : acc) [] [1, 2, 3]  -- Resultado: [3, 2, 1]

En resumen, la elección entre foldr y foldl depende de la naturaleza de la función binaria que estás utilizando y del comportamiento deseado en 
términos de orden y asociatividad. -}