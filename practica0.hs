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

-- b) 'matches', que dados un entero 'x' y una lista de enteros descarta
-- de la lista los elementos distintos a 'x'

-- c) 'cuadrupla', que dado un entero 'n', devuelve todas las cuadruplas
-- '(a,b,c,d)' que satisfacen a^2 + b^2 = c^2 + d^2,
-- donde 0 <= a, b, c, d <= 'n'

-- (d) 'unique', que dada una lista 'xs' de enteros, devuelve la lista
-- 'xs' sin elementos repetidos
-- unique :: [Int] -> [Int]
-- -}



-- {-
-- 6) El producto escalar de dos listas de enteros de igual longitud
-- es la suma de los productos de los elementos sucesivos (misma
-- posición) de ambas listas.  Definir una función 'scalarProduct' que
-- devuelva el producto escalar de dos listas.

-- Sugerencia: Usar las funciones 'zip' y 'sum'. -}

-- {-
-- 7) Sin usar funciones definidas en el
-- preludio, defina recursivamente las siguientes funciones y
-- determine su tipo más general:

-- a) 'suma', que suma todos los elementos de una lista de números

-- b) 'alguno', que devuelve True si algún elemento de una
-- lista de valores booleanos es True, y False en caso
-- contrario

-- c) 'todos', que devuelve True si todos los elementos de
-- una lista de valores booleanos son True, y False en caso
-- contrario

-- d) 'codes', que dada una lista de caracteres, devuelve la
-- lista de sus ordinales

-- e) 'restos', que calcula la lista de los restos de la
-- división de los elementos de una lista de números dada por otro
-- número dado

-- f) 'cuadrados', que dada una lista de números, devuelva la
-- lista de sus cuadrados

-- g) 'longitudes', que dada una lista de listas, devuelve la
-- lista de sus longitudes

-- h) 'orden', que dada una lista de pares de números, devuelve
-- la lista de aquellos pares en los que la primera componente es
-- menor que el triple de la segunda

-- i) 'pares', que dada una lista de enteros, devuelve la lista
-- de los elementos pares

-- j) 'letras', que dada una lista de caracteres, devuelve la
-- lista de aquellos que son letras (minúsculas o mayúsculas)

-- k) 'masDe', que dada una lista de listas 'xss' y un
-- número 'n', devuelve la lista de aquellas listas de 'xss'
-- con longitud mayor que 'n' -}

-- {-
-- 8) Redefinir las funciones del ejercicio anterior usando foldr, map y filter.
-- ver su definición en https://hoogle.haskell.org/
-- -}