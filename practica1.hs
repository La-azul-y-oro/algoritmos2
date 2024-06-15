-- 1. Definir las siguientes funciones en forma recursiva:
-- a) borrarUltimo que dada una lista borra el ultimo elemento de la lista. No utilizar reverse, ni tail.
borrarUltimo :: [a] -> [a]
borrarUltimo [] = []
borrarUltimo [_] = []
borrarUltimo (x:xs) = x : borrarUltimo xs

-- b) collect :: [(k, v)] → toma un lista de pares (clave, valor) y asocia cada clave unica con todos
-- los valores con los que estaba apareada originalmente. Por ejemplo: collect

-- --HECHO por Rabacho en clases
borrar h [] = []
borrar h ((a, e):xs) | h == a = borrar h xs 
                     | otherwise = (a,e): borrar h xs 

klave :: Eq t => t -> [(t, a)] -> [a]
klave h [] = []
klave h ((a, e) : xs) | h == a = e : klave h xs 
                      | otherwise = klave h xs 

collect :: Eq k => [(k , v)] -> [(k,[v])]
collect [] = []
collect ((a,e):xs) = (a,e : klave a xs) : collect (borrar a xs)

--Hecho por el cra Andres Grillo en clase
ins :: Eq k => (k,v) -> [(k, [v])] -> [(k, [v])]
ins (k,v) [] = [(k,[v])]
ins (k,v) ((k',vs):xs) | k == k' = (k', v : vs) : xs
                       | otherwise = (k', vs): ins (k,v) xs 

collect' [] = []
collect' ((k,v):xs) = ins (k,v) (collect' xs)


-- c) serie que se comporta de la siguiente manera: serie [1, 2, 3] = [[ ], [1], [1, 2], [1, 2, 3]] Dar su tipo más general.
serie :: [a] -> [[a]]
serie [] = [[]] 
serie xs = serie (init xs) ++ [xs]

-- lo que hace la funcion es....
--- serie [1,2] ++ [1,2,3]
--- serie [1] ++ [1,2], [1,2,3]
--- serie [] ++ [1]
--- serie [] ++ []

-- d) paresIguales :: Int → Int → Int → Int → Bool toma 4 numeros enteros y retorna True si de
-- dos en dos son iguales (en cualquier orden), en los demas casos retorna False. Por ejemplo:
-- paresIguales 3 1 1 2 = False paresIguales 3 1 3 1 = True paresIguales 3 3 1 1 = True
-- paresIguales 3 1 1 3 = True
paresIguales :: Int -> Int -> Int -> Int -> Bool
paresIguales a b c d = paresAux [a, b, c, d]

paresAux :: [Int] -> Bool
paresAux [] = False
paresAux (x:xs) | ocurre x xs == 2 = True
                | otherwise = paresAux xs

ocurre :: Int -> [Int] -> Int
ocurre _ [] = 0
ocurre n (x:xs) | n == x = 1 + ocurre n xs 
                | otherwise = ocurre n xs

-- e) isosceles :: Int → Int → Int → Bool que dadas la longitud de los lados de un triangulo nos dice si es un triangulo isosceles.
-- VER LUEGO 
isosceles :: Int -> Int -> Int -> Bool
isosceles a b c = hayDuplicados [a,b,c]

hayDuplicados :: (Eq a) => [a] -> Bool
hayDuplicados [] = False
hayDuplicados (x:xs) = elementoEnLista x xs || hayDuplicados xs

elementoEnLista :: (Eq a) => a -> [a] -> Bool
elementoEnLista _ [] = False
elementoEnLista y (z:zs) = y == z || elementoEnLista y zs


-- f) ror que dada una lista xs y un entero n, tal que n <= length xs, rota los primeros n elementos
-- de xs a la derecha: ror 3 [1, 2, 3, 4, 5] = [4, 5, 1, 2, 3]. Definir una version recursiva de ror ,
-- sin usar drop, take ni tail.

ror :: Int -> [a] -> [a]
ror n xs = ror' n xs xs

ror' _ _ [] = []
ror' 0 ys zs = zs 
ror' m (y:ys) (z:zs) = ror' (m - 1) ys (zs ++ [y])


---- 3 1:[2,3,4,5] 1:[2,3,4,5] = (3-1) [2, 3, 4, 5] [2, 3, 4, 5] ++ [1]
---- 2 2:[3,4,5] 2:[3,4,5,1] = (2-1) [3, 4, 5] [3, 4, 5, 1] ++ [2]
---- 1 3:[4,5] 3:[4,5,1,2] = (1-1) [4, 5] [4, 5, 1, 2] ++ [3]
---- 0 [4,5] [4,5,1,2,3] = [4,5,1,2,3]

-- Asi se podria resolver en una UNICA linea
rorSimple n xs = drop n xs ++ take n xs


-- g) upto :: Int → Int → [Int] que dado dos numeros enteros n y m devuelve la lista 
-- [n, n + 1, n + 2, ..., m ] en caso que n <= m y la lista [ ] en otro caso. No usar listas por comprension.
upto :: Int -> Int -> [Int]
upto n m | n > m = []
         | otherwise = n : upto (n+1) m 

-- h) eco que devuelve la cadena obtenida a partir de la cadena xs repitiendo cada elemento tantas
-- veces como indica su posicion. No usar listas por comprension.
-- Por ejemplo: eco "hola" = "hoolllaaaa"

eco:: [a] -> [a]
eco xs = replicar (zip [1..] xs)

replicar :: [(Int, a)] -> [a]
replicar [] = []
replicar (x:xs) = copia x ++ replicar xs

copia :: (Int, a) -> [a]
copia (x, c) = replicate x c

--- opcion simple que resuelve el replicar
--replicar = concatMap copia

--------------------
-- 2. Definir usando listas por comprension las funciones:
-- a) cambios : [a ] → [Int], que dada una lista, devuelve la lista de los ındices en que la lista
-- cambia. Es decir, dada la lista s retorna la lista con los i tal que si <= si+1
-- cambios [1, 1, 1, 3, 3, 1, 1] = [2, 4]

cambios :: Eq a => [a] -> [Int]
cambios [] = []
cambios xs = [i | (i, (x,y)) <- zip [1..] (zip xs (tail xs)), x/=y]


--[1, 1, 1, 3, 3, 1, 1] [1, 1, 3, 3, 1, 1]
--(zip xs (tail xs)) = [(1,1),(1,1),(1,3),(3,3),(3,1),(1,1)] --- primer zip
-- [ (1,(1,1)), (2,(1,1)), (3,(1,3)), (4,(3,3)), (5,(3,1)), (6,(1,1))] ---segundo zip
-- (1,(1,1)) -> 1 != 1 -> falso -> NO agrega
-- (2,(1,1)) -> 1 != 1 -> falso -> NO agrega
-- (3,(1,3)) -> 1 != 3 -> true -> agrega
-- (4,(3,3)) -> 3 != 3 -> falso -> NO agrega
-- (5,(3,1)) -> 3 != 1 -> true -> agrega
-- (6,(1,1)) -> 1 != 1 -> falso -> NO agrega

-- b) oblongoNumber :: [Int] que genera la lista de los numeros oblongos. Un numero es oblongo si es el producto de dos naturales consecutivos. Por ejemplo, los numeros [2, 6, 12, 20, ...]
oblongoNumber :: Int -> [Int]
oblongoNumber n = [i * (i + 1) | i <- [1..n]]

-- c) abundantes :: [Integer] que es la lista de todos los numeros abundantes. Un numero natural
-- n se denomina abundante si es menor que la suma de sus divisores propios. Por ejemplo, 12
-- y 30 son abundantes pero 5 y 28 no lo son. Por ejemplo abundates = [12, 18, 20, 24, 30, 36, ...

-- d) eco que devuelve la cadena obtenida a partir de la cadena xs repitiendo cada elemento tantas
-- veces como indica su posicion. No usar listas por comprension. Por ejemplo: eco "hola" =
-- "hoolllaaaa"

-- e) euler :: Int → Int tal que euler n es la suma de todos los multiplos de 3 o 5 menores que n.
-- Por ejemplo, euler 10 = 23. Puedes usar sin definir la funcion sum que suma los elementos
-- de una lista

-- f) expandir :: [Int] → [Int] que reemplace en una lista de numeros positivos cada numero n por
-- n copias de sı mismo:
-- Ejemplo: expandir [3, 4, 2] = [3, 3, 3, 4, 4, 4, 4, 2, 2]

-- 3. Dar dos ejemplos de funciones que tengan los siguientes tipos:
-- a) (Int → Int) → (Bool → Bool)
-- b) Bool → (Int → Bool)
-- c) Char → Char
-- d) Int → (Int → Bool) → [Int]
-- e) [a ] → (a → [b ]) → [b ]
-- f) [[a ]] → (a → Bool) → [a ]
-- g) (a, b, c) → Bool
-- h) (a, b, c) → Int → c
-- i) (a, a, a) → Int → a

-- 4. Dar el tipo de la siguiente funciones o expresiones:
-- a) foo1 p = if p then (p ∧) else (p ∧)
-- b) foo2 x y z = x (y z )
-- c) foo3 x y z = x y z
-- d) foo4 x y z = x y : z
-- e) foo5 x y z = x : y z
-- f) foo6 x y z = x ++ y z
-- g) foo7 a b = if b a then head a else [ ]
-- h) foo8 a b = if b a then a else [ ]
-- i) foo9 a b = if b a then head (:a) else (:[ ])


-- 5. Definir las siguientes funciones usando foldr:
-- a) map :: (a → b) → [a ] → [b ] que dada una funcion y una lista, aplica la funcion a cada
-- elemento de la lista.
-- b) filter :: (a → Bool) → [a ] → [a ] , que dado un predicado y una lista xs, devuelve una lista
-- con los elementos de xs que satisfacen el predicado.
-- c) unzip ::[(a, b)] → ([a ], [b ]), que dada una lista de tuplas xs retorna una tupla de listas donde
-- cada una corresponde a los primeros y secundos elementos de los pares respectivamente.
-- Ej. unzip [(’a’, 1),(’z’, 7),(’h’, 9)] = ("azh", [1, 7, 9])
-- d) pair2List ::(a, [b ]) → [(a, b)] que dado un par formado por un valor x y una lista xs convierta
-- a la lista xs en una lista de pares, formada con los elementos de xs y x .
-- Ej. pair2List (x , [y1 , y2 , y3 ]) = [(x , y1 ),(x , y2 ),(x , y3 )]
-- e) maxSec :: [(Int, Int)] → (Int, Int), que dada una lista de pares de naturales que represente a
-- una lista de segmentos de la recta, calcule el segmento mas largo de la misma.
-- Ej.maxSec [(1, 2),(0, 7),(4, 6)] = (0, 7)
-- Puede definir una funcion auxiliar maxL :: (Int, Int) → (Int, Int) → (Int, Int), que dados dos
-- pares de naturales que representan a dos segmentos de la recta, devuelva el segmento cuya
-- longitud sea maxima.
-- Ej.maxL (1, 2) (0, 7) = (0, 7)