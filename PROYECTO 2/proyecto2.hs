import Test.QuickCheck
import Data.Array
import Data.List
import Cola
import System.IO
import System.Environment
import System.Directory
import Data.Char

-- -----------------------------------------------------------------------------
-- Programación Declarativa 2023/24
-- Grado de Ingeniería Informática - Tecnologías Informáticas y
-- Doble Grado en Matemáticas e Ingenigería Informática
-- 2ª Convocatoria                                           04 de Julio de 2024
-- -----------------------------------------------------------------------------
-- Apellidos:
-- Nombre:
-- UVUS:
-- Laboratorio/Puesto:
--
-- NO OLVIDES RELLENAR CON TUS DATOS LA CABECERA ANTERIOR
-- 
-- -----------------------------------------------------------------------------
-- INSTRUCCIONES PARA LA ENTREGA
-- 1. CAMBIA EL NOMBRE de este archivo por:       <uvus>.hs
--    donde "<uvus>" es tu UVUS. 
-- 2. COMENTA LAS LÍNEAS CON ERRORES hasta que se pueda cargar el fichero:
--    sin problemas. AQUELLOS ARCHIVOS DE CÓDIGO QUE NO CARGUEN POR ERRORES
--    DE CÓDIGO NO SE CORREGIRÁN.
-- 3. RELLENA los datos de la cabecera incluido el laboratorio y puesto.
-- 4. COMPRIME ESTE archivo en un único fichero llamado EXACTAMENTE:
--      ENTREGA-<uvus>.tar.gz      (o bien)       ENTREGA-<uvus>.tar.xz
--    donde "<uvus>" es tu UVUS. No te olvides del guión después de
--    ENTREGA, y NO lo comprimas en un fichero .zip.
-- 4. REINICIA el equipo. En el menú de selección del sistema (con fondo
--    blanco), HAZ CLICK SOBRE "Enviar examen" al lado de sistema Ubuntu.
-- 5. Pregunta al profesor si ha llegado tu correo correctamente, si es
--    así, ya puedes dejar tu puesto APAGANDO EL EQUIPO (si lo consieras
--    oportuno, vuelve al escritorio y borra el examen del equipo).
-- ----------------------------------------------------------------------
-- ORIENTACIONES
-- · Escribe la solución de cada ejercicio en el hueco reservado para
--   ello. NO ESCRIBAS FUERA DE DICHO HUECO. NO MODIFIQUES NADA DE LOS 
--   ENUNCIADOS.
-- · Se valorará el uso correcto de tipados para cada función definida.
-- · Puedes añadir tantas funciones auxiliares como necesites (incluyendo su 
--   signatura adecuadamente).
-- · Puedes usar otros módulos de Haskell que estén ya importados.
-- -----------------------------------------------------------------------------

-- ------------------------------------------------------------------------------
-- Ejercicio 1.1 (2 puntos)
-- ------------------------------------------------------------------------------

-- Define la función nubn que toma un entero n y una lista xs. La función devuelve
-- una nueva lista donde los elementos repetidos más de n veces son eliminados.

ejemplo1 :: [Int]
ejemplo1 = [1,2,3,1,0,2,2,3,2]

-- nubn 1 [0,0,0]  == [0]
-- nubn 2 [0,0,0]  == [0, 0]
-- nubn 1 ejemplo1 == [1,0,3,2]
-- nubn 2 ejemplo1 == [1,3,1,0,2,3,2]
-- nubn 1 [1..5]   == [1,2,3,4,5]
--
--
-- Definir la función mediante:
-- a) Recursión (R)
-- b) Recursión con acumulador (RA)
-- c) Plegado (F)
-- ------------------------------------------------------------------------------

nrep e xs = sum [if x == e then 1 else 0 | x <- xs]

nubnR :: Eq a => Int -> [a] -> [a]
nubnR _ [] = []
nubnR n (x:xs) 
    | nrep x xs > n-1 = nubnR n xs
    | otherwise = x:nubnR n xs

nubnRA :: Eq a => Int -> [a] -> [a]
nubnRA n = aux_AC n []

aux_AC :: Eq a => Int -> [a] -> [a] -> [a]
aux_AC n ac [] = ac
aux_AC n ac (x:xs) = if nrep x ac == n then aux_AC n ac xs else aux_AC n (ac++[x]) xs

nubnF :: (Eq a) => Int -> [a] -> [a]
nubnF n = foldr (\ x r -> if nrep x r > n-1 then r else x:r) []

-- ------------------------------------------------------------------------------
-- Ejercicio 1.2 (0.5 puntos)
-- ------------------------------------------------------------------------------
-- El objetivo es definir una propiedad llamada prop_nub1 que tome una lista xs
-- y verifique que la función nubn con n=1 produce el mismo conjunto de elementos
-- que la función estándar nub, sin importar el orden de los elementos. Esto 
-- significa que las dos listas resultantes deben contener los mismos elementos,
-- aunque puedan estar en diferente orden.
-- ------------------------------------------------------------------------------

prop_nub1 :: (Eq a, Ord a) =>  [a] -> Bool
prop_nub1 xs = (sort (nub xs)) == (sort (nubnR 1 xs))


-- ------------------------------------------------------------------------------
-- Ejercicio 2 (1.5 punto)
-- ------------------------------------------------------------------------------
-- Define, mediante el uso de ORDEN SUPERIOR, la función (incrustaFiltra ys p xsss), 
-- que toma una lista de listas de listas (xsss) y agrega cada elemento de la 
-- lista ys a la izquierda de cada elemento de xsss en el mismo orden. Luego, 
-- reduce el nivel de anidación de las listas (concatenando las sublistas) y 
-- selecciona los elementos que cumplan la propiedad p. 
--
-- Por ejemplo, dada la siguientes listas de ejemplo:
--

ejLista :: [[[Integer]]]
ejLista = [[[1,2],[],[2]],[[3,3],[4,5,6,7]],[[1,2,3,4,5],[0],[1,1]],[]]

ejLista2 :: [[[Char]]]
ejLista2 = [["o-lAK#~",[],"23a2"],[[]],["un","d","o"]]

-- > incrustaFiltra "h m" (\x -> (x == ' ') || (isLower x))  ejLista2
-- "hola mundo"
--
-- Traza: 
--   * Agrega cada elemento de la lista ys a la izquierda de cada 
--     elemento de xsss en el mismo orden:
--
--     > [["h","o-lAK#~","","23a2"],[" ",""],["m","un","d","o"]]
--
--   * Reduce el nivel de anidación de las listas:
--
--     > "ho-lAK#~23a2 mundo"
--
--   * Selecciona los elementos que cumplen p (minúsculas o espacios):
--
--     > "hola mundo"
--
-- 
-- > incrustaFiltra [1,2,3] even ejLista
-- [2,2,2,4,6,2,4,0]
--

-- > incrustaFiltra [-1] (<4) ejLista
-- [-1,1,2,2]
--
-- Para llevar a cabo las operaciones de concatenar la lista ys, concatenar las 
-- sublistas y filtrar según p, se debe utilizar la composición de funciones 
-- de orden superior.
--
-- ------------------------------------------------------------------------------

incrustaFiltra :: [a] -> (a -> Bool) -> [[[a]]] -> [a]
incrustaFiltra ys p xsss =
      ( (filter p). concat . (map concat) . (map (\(a,b) -> [a]:b))) (zip ys xsss)

-- ------------------------------------------------------------------------------
-- Ejercicio 3. (1 puntos)
-- ------------------------------------------------------------------------------
-- Un número x se considera cúbico si su raíz cúbica es exacta, es decir, existe un
-- número natural n tal que n^3 = x.
--
-- Genera la lista infinita (cubicosSuma11) de ternas (x, y, z) donde x e y son 
-- números cubicos consecutivos (sus raices son consecutivas) que satisfacen que 
-- su suma es z y que z es múltiplo de 11.
--
-- Por ejemplo: 5^3 == 125 y 6^3 == 216, 5 y 6 son consecutivos. Su suma es 341
-- la cual es multiplo de 11 (11 * 31 == 341), por tanto una ternal de la
-- lista infinita sería (125, 216, 341).
--
-- take 3 cubicosSuma11 == [(125,216,341),(4096,4913,9009),(19683,21952,41635)]
-- cubicosSuma11 !! 5   == (216000,226981,442981)
-- ------------------------------------------------------------------------------

cubicos :: Integer -> [Integer]
cubicos n = map (^3) [n..]


nroot n x = x ** (1 / fromIntegral n)

esCubico :: Integer -> Bool
esCubico n =  n == ((floor.(nroot 3)) (fromIntegral n))^3 
  where cs = cubicos 1

cubicosSuma11 :: [(Integer,Integer,Integer)]
cubicosSuma11 = 
    [(x1,x2,x1+x2) | (x1,x2) <- zip cs (tail cs), 
                                mod (x1 + x2) 11 == 0 ]
    where cs = cubicos 1


-- ------------------------------------------------------------------------------
-- Ejercicio 4.1 (1 punto)
-- ------------------------------------------------------------------------------
-- Se quiere implementar un sistema de recomendación con un árbol. Cada nodo
-- contendrá un atributo y un valor máximo de dicho atributo. En las hojas,
-- estarán las colas de recomendaciones. Por ejemplo, supongamos el siguiente 
-- árbol para recomendar una actividad en función de las condiciones climáticas:
--
--                               Raíz    
--                             /      \
--                            /        \
--                           /          \
--                 Lluvia, 0.5            Lluvia, 1.0
--                    /   \                   /    \
--                   /     \                 /      \
--                  /       \               /        \
--         Humedad, 0.6  Humedad, 1.0   Humedad 0.7 Humedad, 1.0
--               |             |             |          |
--           [Futbol,      [Pinpong,      [Cine,     [Ajedrez,
--            Tenis]        Padel]        Natacion]   Cartas] 
--
-- Dado que queremos promocionar todas las actividades posibles, las
-- actividades estarán guardadas en colas. De esta manera, cuando hay que 
-- recomendar una actividad, se toma la primera de la cola y se pasa al final de 
-- la misma cola.
--
-- Por ejemplo, supongamos que en el día de hoy hay una probabilidad de lluvia
-- del 0.1 y un nivel de humedad de 0.65. Según el árbol anterior, se seleccionaría
-- la cola [Pinpong, Padel] y se tomaría la cabeza de la cola, con lo que la 
-- recomendación sería Pinpong. Además, la cola se modificaría a [Padel, Pinpong].
--
--
-- Definir el tipo de datos Recomendador que se ajuste a esta definición. El
-- tipo de la cola debe ser genérico, es decir, la cola puede ser de tipo Char,
-- String, Int, Float, ...
--
-- El arbol anterior tendría la siguiente representación:
--
-- R [N ("Lluvia",0.5) 
--      [H ("Humedad",0.6) (C ["Tenis","Futbol"]),
--       H ("Humedad",1.0) (C ["Padel","Pinpong"])],
--    N ("Lluvia",1.0) 
--      [H ("Humedad",0.7) (C ["Natacion","Cine"]),
--       H ("Humedad",1.0) (C ["Cartas","Ajedrez"])]
--   ]
--
-- Una vez definido el tipo, crea el árbol recomendador anterior utilizando la
-- estructura de datos definida. La representación del árbol debe coincidir con
-- la descripción proporcionada anteriormente.
--
---------------------------------------------------------------------------------

data Recomendador a =  N (String, Float) [Recomendador a] 
                    | H (String, Float) (Cola a)
                    | R [Recomendador a] 
             deriving (Show, Eq)
             

cr1 = foldr inserta vacia ["Futbol", "Tenis"]
cr2 = foldr inserta vacia ["Pinpong", "Padel"]
cr3 = foldr inserta vacia ["Cine", "Natacion"]
cr4 = foldr inserta vacia ["Ajedrez", "Cartas"]

recomendador :: Recomendador String
recomendador = R
                     [N ("Lluvia", 0.5) [H ("Humedad", 0.6) cr1, 
                                         H ("Humedad", 1.0) cr2], 
                      N ("Lluvia", 1.0) [H ("Humedad", 0.7) cr3, 
                                         H ("Humedad", 1.0) cr4]]


-- ------------------------------------------------------------------------------
-- Ejercicio 4.2. (2.5 puntos)
-- ------------------------------------------------------------------------------
-- Implementa la función (recomendar r d) que toma un árbol recomendador r y un 
-- conjunto de datos d que incluyen etiquetas y valores de nodos. La función 
-- devuelve una tupla, donde el primer elemento es la cola rotada y el segundo 
-- elemento es la recomendación asociada de tipo Recomendacion. Por ejemplo:


data Recomendacion a = Ninguna | REC a deriving (Show, Eq)

datos ::  [(String, Float)]
datos = [("Humedad", 0.1), ("Lluvia", 0.6)]

-- > recomendar recomendador datos
-- > (C ["Cine","Natacion"],REC "Natacion")
--
-- datos = [("Humedad", 0.1), ("Lluvia", 0.1)]
-- > (C ["Futbol","Tenis"],REC "Tenis")
--
-- Ayuda para la implementación:
--
--   - Implementa una función que permita rotar una cola.
--
--   - Implementa una función que recupere un valor de la lista de datos dado el 
--     nombre del atributo.
--
-- > rota cr1
-- C ["Futbol","Tenis"]
-- rota cr4
-- C ["Ajedrez","Cartas"]
-- > coger_valor datos "Lluvia"
-- 0.6
--   
-- ------------------------------------------------------------------------------


rota :: Cola a -> Cola a
rota c = inserta p (resto c)
  where p = primero c

coger_valor :: [(String, Float)] -> String -> Float
coger_valor [] _ = -1
coger_valor ((a, v):xs) att
  | a == att = v
  | otherwise = coger_valor xs att
  

recomendar :: Recomendador a -> [(String, Float)] -> (Cola a, Recomendacion a)
recomendar (H (a,umbral) cola) datos = (rota cola, sol)     
  where r = primero cola
        v = coger_valor datos a
        sol = if umbral >= v then REC r else Ninguna


recomendar (R []) datos = (vacia, Ninguna)        


recomendar (R (r:rs)) datos = case (recomendar r datos) of 
               (cola, REC sol) -> (cola, REC sol)           
               _         -> recomendar (R rs) datos
  
recomendar (N (a,umbral) []) datos =  (vacia, Ninguna)

recomendar (N (a,umbral) (r:rs)) datos 
           | umbral >= v = (case (recomendar r datos) of 
                              (cola, REC sol) -> (cola, REC sol)           
                              _         -> recomendar (R rs) datos)
           | otherwise   = recomendar (N (a,umbral) rs) datos
       where v = coger_valor datos a


-- ------------------------------------------------------------------------------

-- ------------------------------------------------------------------------------
-- Ejercicio 5 (2.5 puntos)
-- ------------------------------------------------------------------------------
-- Toda matriz de más de una fila y una columna se puede separar en cuatro 
-- bloques de igual tamaño de la siguiente forma:
--
--     | A | B |
-- M = |---|---|
--     | C | D |
--
-- Definimos la traspuesta por bloques de la matriz anterior como:
--
--     | A | C |
-- M = |---|---|
--     | B | D |
--
-- Por ejemplo, para la matriz:
--
--      | 1  2  3  4 |
-- M1 = | 5  6  7  8 |
--      | 9 10 11 12 |
--
-- Sus bloques serían A = (1 2), B = (3 4), C = (9 10) y D = (11 12).
--
-- La matriz traspuesta por bloques de M1 sería:
--
--      | 1  2  9 10 |
--      | 3  4 11 12 |
--
-- En este caso, la fila 2 ha quedado descartada al tener la matriz 
-- original un número impar de filas.

type Matriz a = Array (Int, Int) a


m1 :: Matriz Integer
m1 = listArray ((1,1),(3,4)) [1..12]

m1t :: Matriz Integer
m1t = array ((1,1),(2,4)) [((1,1),1),((1,2),2),((1,3),9),((1,4),10),
            ((2,1),3),((2,2),4),((2,3),11),((2,4),12)]


m2 :: Matriz Integer
m2 = listArray ((1,1),(2,2)) [1..4]

m2t :: Matriz Integer
m2t = array ((1,1),(2,2)) [((1,1),1),((1,2),3),((2,1),2),((2,2),4)]

m3 :: Matriz Integer
m3 = listArray ((1,1),(4,4)) [1,2,0,0,3,4,1,1,0,0,5,6,1,1,7,8]

m4 :: Matriz Integer
m4 = listArray ((1,1),(3,3)) [1,0,0,0,1,0,0,0,1]

--
-- Se pide:
-- 
--     1. (1 punto) Definir el operador (||||) que lleve a cabo la concatenación de dos
--        matrices horizontalmente. Considere que ambas matrices tienen el mismo 
--        número de filas. Por ejemplo:
--
--             | 1  2  3  4 |              | 1 0 0 |
--        M1 = | 5  6  7  8 |         MS = | 0 1 0 | 
--             | 9 10 11 12 |              | 0 0 1 |
--         
--
--                     | 1  2  3  4  1  0  0 |
--        M1 |||| M2 = | 5  6  7  8  0  1  0 | 
--                     | 9 10 11 12  0  0  1 |
--
--       > m1 |||| m4
--       array ((1,1),(3,7)) [((1,1),1),((1,2),2),((1,3),3),((1,4),4),((1,5),1),
--       ((1,6),0),((1,7),0),((2,1),5),((2,2),6),((2,3),7),((2,4),8),((2,5),0),
--       ((2,6),1),((2,7),0),((3,1),9),((3,2),10),((3,3),11),((3,4),12),((3,5),0),
--       ((3,6),0),((3,7),1)]
--
--    2. (1.5 puntos). Define la función (traspuestaBloques m), que recibe una matriz
--       y lleva a cabo la traspuesta según la definición anterior:
--
--       > traspuestaBloques m1 == m1t
--       True
--       > traspuestaBloques m2 == m2t
--       True
--       > traspuestaBloques m3 == m3
--       True
-- 
--       Ayuda: será de utilidad definir una función auxiliar para extraer una
--       submatriz de una matriz dadas las filas y columnas iniciales y finales.
--       La función matrizLista de la práctica 8.1 también será de utilidad.
--
--
-- Práctica 8.1
separa :: Int -> [a] -> [[a]]
separa _ [] = []
separa n xs = take n xs : separa n (drop n xs)

-- Práctica 8.1
matrizLista :: Matriz a -> [[a]]
matrizLista p = separa cols (elems p)
  where cols = snd $ snd $ bounds p

--
-- ------------------------------------------------------------------------------

subMatriz :: Matriz a -> (Int, Int) -> (Int, Int) -> Matriz a
subMatriz a (xini, xend) (yini, yend) = listArray ((1,1), (xend-xini+1,yend-yini+1)) [a ! (i,j) | i <- [xini..xend], j <- [yini..yend]]

          
          
(||||) :: Matriz a -> Matriz a -> Matriz a
(||||) a b = listArray  ((xini, yini), (xend, yenda + yendb)) (concat [l1 ++ l2 | (l1, l2) <- zip (matrizLista a) (matrizLista b)])
  where ((xini, yini), (xend, yenda)) = bounds a
        ((_, _), (_, yendb)) = bounds b


traspuestaBloques :: Matriz a ->  Matriz a
traspuestaBloques m = listArray ((xini, yini), (w*2, h*2)) (elems (a |||| c) ++ elems (b |||| d))
  where ((xini, yini), (xend, yend)) = bounds m
        (w, h) = (div (xend-xini+1) 2, div (yend-yini+1) 2)
        (a, b, c, d) = (subMatriz m (xini, xini + w - 1) (yini, yini + h - 1), 
                            subMatriz m (xini, xini + w - 1) (yend - h + 1, yend),                          
                            subMatriz m (xend - w + 1, xend) (yini, yini + h - 1),
                            subMatriz m (xend - w + 1, xend)  (yend - h + 1, yend))

-- ------------------------------------------------------------------------------

-- ------------------------------------------------------------------------------
-- Ejercicio 6 (2.5 puntos)
-- ------------------------------------------------------------------------------
-- Los archivos message1.enc y message2.enc contienen mensajes codificados. Para
-- decodificarlos, es necesario leer los archivos, dividirlos en líneas y tomar
-- los primeros caracteres de cada línea. El código ASCII de estos caracteres 
-- apunta a la posición de dos caracteres del resto de la línea que forman
-- parte del mensaje.
--
-- Ejemplo:
--
--                            6 = 54
--     ---------------------------------------------------------   
--     |                           A == 65                     |
--    -|-------------------------------------------------------|----------- 
--    ||                                                       |          | 
--    A6ASDKJ"·$"JASDFADSK"··$")"S(AFSD"K"·$)"·$)SDFADSFKASF)"L)"·)$KSDFKASDFKA)"·$
--
--      0         1         2         3         4          5         6     
--      0         0         0         0         0          0         0
--
--
-- Implementa un programa que reciba el nombre de un archivo con un mensaje codificado,
-- lo decodifique, lo imprima por pantalla y además lo escriba en un archivo
-- llamado "mensaje.txt" con el texto "mensaje decodificado: <mensaje>".
-- 
-- > runHaskell examen.hs message1.enc
-- Hola mundo
-- > cat mensaje.txt
-- mensaje decodificado: Hola mundo
--
-- ------------------------------------------------------------------------------

decodifica_linea [] = []
decodifica_linea (c1:c2:cs) = [cs !! (ord c1), cs !! (ord c2)]

main :: IO ()
main = do
  args <- getArgs
  
  if length args == 0 then 
    putStrLn "Debe indicar el nombre del archivo con el mensaje codificado"
  else do
    exists <- doesFileExist (head args)
    
    if exists then do
      text <- readFile (head args)
      let lineas = lines text
      let mensaje =  ((concat.(map decodifica_linea)) lineas)
      putStrLn mensaje
      writeFile "mensaje.txt" ("mensaje decodificado: " ++ mensaje ++ "\n")

    else
      putStrLn ("Archivo " ++ (head args) ++ " no encontrado")


-- ------------------------------------------------------------------------------
