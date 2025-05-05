mercaderia :: [String]
mercaderia = ["arroz","pollo","yogurt","pollo"]
stock:: [(String,Int)]
stock = [
  ("arroz",10),
  ("carne",15),
  ("leche",13)]

precios:: [(String,Int)]
precios = [
  ("arroz", 1200),
  ("carne", 7000),
  ("leche",2000)]

--ejercicio 1
generarStock :: [String]->[(String, Int)]
generarStock [] = []
generarStock (x:xs) = (x, contar x (x:xs)) : generarStock (eliminar x xs)

contar:: String -> [String] -> Int
contar _ [] = 0
contar p (y:ys)  | p==y = 1 + contar p ys
                 | otherwise = contar p ys 


eliminar :: String -> [String] -> [String]
eliminar _ [] = []
eliminar p (y:ys)  | p == y    = eliminar p ys
                   | otherwise = y : eliminar p ys

--ejercicio 2
stockDeProducto :: [(String, Int)] ->String ->Int
stockDeProducto [] _ = 0
stockDeProducto ((nombre,cant):xs) producto | producto == nombre = cant
                                         | otherwise = stockDeProducto xs producto


--ejercicio 3
dineroEnStock :: [(String,Int)] ->[(String,Int)]->Int
dineroEnStock [] [] = 0
dineroEnStock ((nombre1, cant):xs) ((nombre2,precio) : ys) | nombre1 == nombre2 = cant*precio + dineroEnStock  xs ys
                                                           | otherwise = dineroEnStock xs ys

--ejercicio 4
--aplicarOferta :: [(String, Int)]->[(String, Int)]->[(String,Float)]
--aplicarOferta [] [] = 0
--aplicarOferta ((nombre1,cant):xs) ((nombre2,precio):ys) |nombre1 == nombre2 && cant > 10 = ((nombre1, (precio * 0.8)): aplicarOferta xs ys)
 --                                                       | nombre1== nombre2 && cant < 10 = ((nombre1, fromIntegral (precio)): aplicarOferta xs ys)
  --                                                      | otherwise = aplicarOferta xs ys

--ejercicio 5
type Fila = [Int]
type Tablero = [Fila]
type Posicion = (Int, Int)
type Camino = [Posicion]

concatenarFilas:: Tablero -> Fila
concatenarFilas [] = []
concatenarFilas (fila:otrafila) = fila ++ concatenarFilas otrafila


maximo :: Fila -> Int
maximo [x] = x
maximo (x:y:xs)
  | x > y     = maximo (x:xs)
  | otherwise = maximo (y:xs)

maximoFilas:: Tablero -> Int
maximoFilas [] = 0
maximoFilas filas = maximo (concatenarFilas filas)



--ejercicio 6
masRepetido:: Tablero -> Int
masRepetido [] = 0
masRepetido filas = compararNum(concatenarFilas filas) 

compararNum :: [Int] -> Int
compararNum [] = 0
compararNum (x:y:xs) | contarNum x xs > contarNum y xs = x
                   | otherwise = compararNum  (y:xs)

contarNum::  Int -> [Int]  -> Int
contarNum _ []  = 0
contarNum p (y:ys)  | p==y = 1 + contarNum p ys
                    | otherwise = contarNum p ys 

--ejercicio 7
--ejercicio 8


--ejercicio 9
divisoresPropios:: Int -> [Int]  
divisoresPropios 0 = []
divisoresPropios 1 = [1]
divisoresPropios n =  buscarDivisores  n (n-1)

buscarDivisores:: Int-> Int-> [Int] 
buscarDivisores _ 0 = []
buscarDivisores n x | mod n x == 0 = x : buscarDivisores n  (x - 1)
                    | otherwise = buscarDivisores n (x - 1)


--ejercicio 10
sonAmigos :: Int ->Int ->Bool

sonAmigos x y = sumatoria(divisoresPropios x) == y && sumatoria(divisoresPropios y) == x 


sumatoria::[Int] -> Int
sumatoria [] = 0  
sumatoria (x:xs) =  x + sumatoria (xs)

--ejercicio 11
longitud :: [t] -> Int
longitud [] = 0
longitud (_:xs) = 1 + longitud xs

primerosNperfectos:: :: Int->[Int]
primerosNperfectos 0 = 0
primerosNperfectos n 