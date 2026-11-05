-- -----------------------------------------------------------------------------
-- Programación Declarativa 2022/23
-- Grado de Ingeniería Informática - Tecnologías Informáticas
-- -----------------------------------------------------------------------------

module Examen(
  elementoProp,
  vecinosProp,  
  vecinos2movProp,
  pos2movProp,
  laberinto2movesProp,
  sigPosProp,
  mueveteProp,
  buscarSalidaProp,
  buscaminasProp,
  caminosASalida,
  caminosASalidaLab1,
  caminosASalidaLab2,
  todasDirec,
  guarda_cola,
  historial,
  lee_configuracion,
  lab1,
  lab2
) where

import Test.QuickCheck
import Data.Char
import Data.List
import Data.Array
import Control.Exception (catch, SomeException)
import System.Environment (getArgs)
import System.Directory
import System.IO (stdin, hSetEcho, hSetBuffering)
import System.IO( BufferMode( NoBuffering ) )
import Utils.UtilidadesExamen
import Utils.ColaConListas
import Utils.Auxiliar


-- -----------------------------------------------------------------------------
-- Ejercicio 1 El objetivo de este ejercicio es conseguir una función
-- que busque la salida en un laberinto. Partimos de un laberinto representado 
-- mediante una cadena de caracteres (es una representación visual). Por ejemplo
-- el siguiente laberinto:


--                #################
--                #I  #   #       #
--                # ### # ##### # #
--                #     #     # # #
--                # ### ### # # ###
--                #   # # # # #  x#
--                # # # # ### # ###
--                # # #       #   #
--                #################

-- Donde el carácter # representa un muro u obstáculo, la I el punto de inicio y 
-- la 'x' la salida.
--
-- En este ejercicio, para facilitar su resolución se irá pidiendo la implementación 
-- de funciones que serán de utilidad para alcanzar el objetivo final de buscar 
-- una ruta de salida.  

laberinto0 = ["#######",
              "#I   x#",
              "#######"]

laberinto1 = ["#######",
              "#I#   #",
              "#   # #",
              "## ##x#",            
              "#######"]
              
laberinto2 = [
             "#################",
             "#I  #   #       #",             
             "# ### # # ### # #",
             "#     #     # # #",              
             "# ### ### # # ###",
             "#   # # # # #  x#",              
             "# # ### ### # ###",
             "# # #       #   #",              
             "#################"]

-- Con este ejercicio se evaluará el conocimiento de las nociones básicas
-- de haskell: cadenas y caracteres, uso de listas por comprensión, 
-- generadores, recursividad y patrones.               

-- Ejercicio 1.1 (0.4 puntos) Implementa dos funciones, una para obtener un elemento 
-- desde una lista de listas y otra función para obtener los vecinos (arriba, abajo,
-- izquierda y derecha) de una posición data. Ejemplos:
--
-- > elemento laberinto0 (0,0) 
-- #
-- > elemento laberinto0 (1,1) 
-- I
-- > elemento laberinto0 (1,5) 
-- x
-- > vecinos laberinto0 (1,1)
-- "### "
-- > vecinos laberinto0 (1,4)
-- "## x"
-- -----------------------------------------------------------------------------

elemento :: (Eq a) => [[a]] -> (Int, Int) -> a
elemento ls (i,j) = (ls !! i) !! j


vecinos :: [String] -> (Int, Int) -> [Char]
vecinos ls (i,j) = [elemento ls (i-1, j), elemento ls (i+1,j),
                    elemento ls (i,j-1), elemento ls (i,j+1)]

-- Propiedades
elementoProp :: (Int, Int) -> Property
elementoProp (x,y) = (i > 0) && (j > 0) ==> (elemento' ls (i,j)) == (elemento ls (i,j))
        where chars = ['a'..'z']
              (i,j) = (min (abs x) 1000, min (abs y) 1000)
              ls = [[chars !! ((ii+jj) `mod` ((length chars) -1)) | jj <- [0..(j+10)]] | ii <- [0..(i+10)]]

vecinosProp (x, y) = (i > 0) && (j > 0) ==> (vecinos' ls (i,j)) == (vecinos ls (i,j))
        where chars = ['a'..'z']
              (i,j) = (min (abs x) 1000, min (abs y) 1000)
              ls = [[chars !! ((ii+jj) `mod` ((length chars) -1)) | jj <- [0..(j+10)]] | ii <- [0..(i+10)]]


-- -----------------------------------------------------------------------------             
-- Ejercicio 1.2 (0.2 puntos) Implementa una función que dada una lista de vecinos
-- devuelva los movimientos ("AR", "AB", "IZ", "DE") válidos que se pueden realizar:
--
-- > vecinos2mov "####" 
-- []
-- > vecinos2mov "### " 
-- ["DE"]
-- > vecinos2mov "###x" 
-- ["DE"]
-- > vecinos2mov "I##x" 
-- ["AR", "DE"]
--
--
-- Nota: se aconseja usar la lista *moves* aportada
--
moves :: [String]
moves = ["AR", "AB", "IZ", "DE"]
--
-- -----------------------------------------------------------------------------

vecinos2mov vecinos = [m | (m, v) <- zip moves vecinos, (v /= '#')]

-- Propiedades
vecinos2movProp :: (Int, Int, Int, Int) -> Bool
vecinos2movProp (i,j,k,l) = (vecinos2mov vecinos) == (vecinos2mov' vecinos)
    where (ii,jj,kk,ll) = (mod (abs i) 4,mod (abs j) 4,mod (abs k) 4,mod (abs l) 4) 
          chrs = "#xI "
          vecinos = [chrs !! ii,chrs !! jj,chrs !! kk,chrs !! ll] 

-- -----------------------------------------------------------------------------             
-- Ejercicio 1.3 (0.3 puntos) Implementa una función que dado un laberinto y una
-- posición devuelva la lista de movimientos válidos que se puede realizar. En 
-- el caso de encontrar la 'x' en alguno de dichos movimientos se deberá 
-- añadir la cadena "END" a la lista de movimiento:
--
-- > pos2mov laberinto0 (1,1)
-- ["DE"]
-- > pos2mov laberinto0 (1,4)
-- ["IZ","DE"]
-- > pos2mov laberinto0 (1,5)
-- ["IZ","END"]
-- > pos2mov laberinto2 (3,9)
-- ["AR","AB","IZ","DE"]
-- -----------------------------------------------------------------------------

pos2mov ls (i,j)
     | c == '#' = [] 
     | c == 'x' = moves ++ ["END"]
     | otherwise = moves
   where c = elemento ls (i,j)
         moves = vecinos2mov (vecinos ls (i,j))


-- Propiedades
pos2movProp rs (x, y) = (pos2mov ls (i,j)) == (pos2mov' ls (i,j))
        where chars = "########      xxxx" 
              (i, j, ls) = fmaze chars rs (x,y)


-- -----------------------------------------------------------------------------  
-- Ejercicio 1.4 (0.3 puntos) Podemos representar el laberinto como una lista de 
-- lista de movimientos. Por ejemplo, el laberinto0 tendría la siguiente 
-- representación:
--
-- [[["DE"],["IZ","DE"],["IZ","DE"],["IZ","DE"],["IZ","END"]]]
--
-- dado que solo hay una fila (la primera y la última son paredes), solo nos 
-- podemos mover a izquierda y derecha. El laberinto1 tendría la siguiente 
-- representación: 
--
-- [
--  [["AB"],     [],              ["AB","DE"],["IZ","DE"],["AB","IZ"]],
--  [["AR","DE"],["AB","IZ","DE"],["AR","IZ"],[],         ["AR","AB"]],
--  [[],         ["AR"],          [],         [],         ["AR","END"]]
-- ]
--
-- como se puede observar se han excluido las filas iniciales y finales, así como
-- columnas iniciales y finales dado que se considera que siempre serán paredes.
-- Cada posición no indica que movimiento podemos realizar. En el caso de las 
-- posiciones donde hay un muro u obstáculo su lista de movimientos debe
-- ser vacía. 
--
-- Implementa una función para que dado un laberinto en modo texto se obtenga su 
-- representación como lista de movimientos. 
--
-- laberinto2moves laberinto0
-- > [[["DE"],["IZ","DE"],["IZ","DE"],["IZ","DE"],["IZ","END"]]]
-- > laberinto2moves laberinto1
-- > [[["AB"],[],["AB","DE"],["IZ","DE"],["AB","IZ"]],[["AR","DE"],["AB","IZ","DE"],
--   ["AR","IZ"],[],["AR","AB"]],[[],["AR"],[],[],["AR","END"]]]
-- -----------------------------------------------------------------------------  

laberinto2moves :: [String] -> [[[String]]]
laberinto2moves ls = [[pos2mov ls (i,j) | j <- [1..cols-2]] | i <- [1..rows-2]]
     where cols = length (head ls) 
           rows = length ls 

-- Propiedades
laberinto2movesProp rs (x, y) = (laberinto2moves ls) == (laberinto2moves' ls)
        where chars = "########      xxxx" 
              (i, j, ls) = fmaze chars rs (x,y)

-- -----------------------------------------------------------------------------  
-- Ejercicio 1.5 (0.3 puntos) Implementa dos funciones de utilidad para movernos
-- por el laberinto. 

-- La función *sigPos* determina cuál es la siguiente posición dado un movimiento 
-- y una posición:
--
-- > sigPos "IZ" (2,2)
-- (2,1)
-- > sigPos "IZ" (2,2)
-- (2,3)
-- > sigPos "AB" (4,5)
-- (5,5)
-- > sigPos "AB" (4,5)
-- (3,5)
--
-- La función *muevete* devuelve la siguiente posición y la lista de movimientos
-- que se pueden realizar desde la nueva posición. Esta función recibe como 
-- entrada un laberinto (representado como lista de movimientos), un movimiento
-- a aplicar y una posición actual.
-- 
-- > muevete (laberinto2moves laberinto0) "DE" (0,0)
-- (["IZ","DE"],(0,1))
-- > muevete (laberinto2moves laberinto2) "DE" (2,3)
-- (["AR","AB","IZ"],(2,4))
--
--
-- Notas: 
--   - En sigPos se valorará el uso de patrones.
--   - Tengan en cuenta que los índices en la presentación inicial y en la
--     representación por movimientos no coinciden ya que se han eliminado los
--     muros del contorno.
-- -----------------------------------------------------------------------------  

sigPos "IZ" (i,j) = (i,j-1)
sigPos "DE" (i,j) = (i,j+1)
sigPos "AR" (i,j) = (i-1,j)
sigPos "AB" (i,j) = (i+1,j)
sigPos "END" (i,j) = (i,j)


muevete ls mov (i,j) = (elemento ls pos, pos)
        where pos = sigPos mov (i,j)
   

-- Propiedades
sigPosProp k (i,j) = sigPos m (i,j) == sigPos' m (i,j)
    where m = (["IZ", "DE", "AR", "AB"] ++ ["END"]) !! (mod k 4)


mueveteProp rs k (x,y) = (muevete ls m (i,j)) == (muevete' ls m (i,j))
        where m = (["IZ", "DE", "AR", "AB"] ++ ["END"]) !! (mod k 4)
              chars = "########      xxxx"
              (i, j, ls) = fmaze chars rs (x,y)

-- -----------------------------------------------------------------------------  
-- Ejercicio 1.6 (1.5 puntos) Implementa una función que, dado un laberinto, 
-- representado como lista de movimientos (laberinto2moves), devuelva la secuencia 
-- de movimientos para encontrar la salida 'x'.
--
-- Ayuda:
--   - La implementación deberá ser recursiva, siendo el caso base que se 
--     encuentre "END" en los movimientos posibles desde la posición actual. 
--   - Será necesaria una función auxiliar que tenga en cuenta la posición 
--     actual, la posiciones ya visitadas y los movimientos restantes a 
--     comprobar de la posición actual.
--   - A la vuelta de la recursión se deberán ir anexando los movimientos realizados 
--     SI se encontró el final "END".
--   - Antes de llevar a cabo la llamada recursiva, se deberá 
--     comprobar si la siguiente posición ya fue visitada. En tal caso
--     se descarta dicho movimiento. Esto evita entrar en bucles infinitos
--     durante el recorrido por el laberinto (IZ-DE-IZ-DE-IZ-DE-......-IZ-DE....)
-- -----------------------------------------------------------------------------  


buscarSalida :: [[[String]]] -> [String]
buscarSalida ls = buscarSalidaAux ls (0, 0) [] ((head.head) ls)

buscarSalidaAux :: [[[String]]] -> (Int, Int) -> [(Int, Int)] -> [String] -> [String]
buscarSalidaAux _ _ _ [] = []
buscarSalidaAux _ _ _ ("END":ms) = ["END"]
buscarSalidaAux ls (i,j) visited cms@(m:rms) 
          | elem np visited = buscarSalidaAux ls (i,j) visited rms
          | elem "END" resultado = m:resultado
          | otherwise = buscarSalidaAux ls (i,j) ((i,j):visited) rms
        where (nm, np) = muevete ls m (i,j)
              resultado = buscarSalidaAux ls np (np:visited) nm
              

-- Propiedades   
buscarSalidaProp rs (w, h) = (buscarSalida ms) == (buscarSalida' ms)  
        where chars = "################      xxxx"
              (_, _, ls) = fmaze chars rs (min w 5, min h 5)
              ms = laberinto2moves' ls
          
 
-- -----------------------------------------------------------------------------  
-- Ejercicio 2.1 (0.5 puntos) Queremos representar las opciones o caminos 
-- posibles de un laberinto mediante un árbol, para ello definir un tipo de dato 
-- Laberinto. Cada uno de los nodos deben representar un movimiento posible
-- (IN, AB, AR, DE, IZ) desde la posición actual. El tipo de dato Laberinto se
-- debe componer como sigue:
--
--   - Un tipo de dato *Con* de constructores  que pueden ser los casos: IN , AB, AR, 
--     DE, IZ . IN para el comienzo del laberinto y el resto para representar los 
--     movimientos de este.
--
--   - El tipo Laberinto tendrá dos casos diferentes:
--      * uno con un constructor  N el dato Con y una lista de Laberintos hijos.
--      * otro que es el caso de llegar a la 'x' que se representará con el
--        constructor END.
--
-- Todo debe poder ser mostrable en el terminal y comparable por igualdad.
--
--------------------------------------------------------------------------------

-- Examen
{--
data Con' = NONE1
data Laberinto' = A | B | C deriving (Eq)
type Con = Con'
type Laberinto = Laberinto'

lab1 :: Laberinto
lab1 = undefined

lab2 :: Laberinto
lab2 = undefined

--}

-- Sol
data Con = IN | AB | AR | DE | IZ 
         deriving (Show, Eq)

data Laberinto = N Con [Laberinto]
               | END
         deriving (Show, Eq)



-- El tipo definido de ser válido para las
-- siguientes instancias del tipo:

lab1 = N IN [N DE [N DE [END], 
                   N AB [],  
                   N IZ [END]], 
             END]
             
--                 IN
--                /  \
--               DE  END
--             / | \    
--            DE AB IZ   
--           /        \ 
--         END       END                  

lab2 = N IN [N DE [N AR [N IZ [N IZ
                              [N AB [END]],
                               END]],
                   N AB [],
                  N IZ [END]],
             END]

-- -----------------------------------------------------------------------------  
-- Ejercicio 2.2 (1 puntos) Definir la función 
--
--            caminosASalida :: Laberinto -> [Laberinto]
-- 
-- tal que caminosASalida devuelva la lista de laberintos que llegan a la 'x' en 
-- formato Laberinto.
--
-- Por ejemplo,
--
-- > caminosASalida lab1 
-- [N IN [N DE [N DE [END]],N DE [N IZ [END]]],N IN [END]]
--
-- > caminosASalida lab2
-- [N IN [N DE [N AR [N IZ [N IZ [N AB [END]]],
--                    N IZ [END]]],
--        N DE [N IZ [END]]],
--  N IN [END]]
--
-- -----------------------------------------------------------------------------  

-- Examen
{--

caminosASalida :: Laberinto -> [Laberinto]
caminosASalida _ = undefined

caminosASalidaLab1 = undefined
caminosASalidaLab2 = undefined


--}

llegaSalida :: Laberinto -> Bool
llegaSalida END = True
llegaSalida (N _  xs) = any llegaSalida xs

caminosASalidaLab1 = [N IN [N DE [N DE [END]],N DE [N IZ [END]]],N IN [END]]
caminosASalidaLab2 = [N IN [N DE [N AR [N IZ [N IZ [N AB [END]]], N IZ [END]]],  N DE [N IZ [END]]], N IN [END]]

caminosASalida END  = [END]
caminosASalida (N a  []) = []
caminosASalida lab@(N a xs) =
  if llegaSalida lab
  then [N a (caminosASalida x) | x<- xs, llegaSalida x]
  else []

-- ----------------------------------------------------------------------------- 
-- Ejercicio 2.2 (1 puntos) Definir la función todasDirec :: Laberinto -> Bool
-- tal que todasDirec verifica si en un laberinto nos podemos mover en todas las
-- direcciones posibles (AB, AR, IZ, DE). Por ejemplo,
--
-- > todasDirec lab2
-- True
-- > todasDirec lab1
-- False
--
-- En lab1 solo hay movimientos a la IZ, DE y AB
-- ----------------------------------------------------------------------------- 

-- Examen
{--
todasDirec :: Laberinto -> Bool
todasDirec _ = undefined
--}

todasDirec lab = auxDirec lab moves

auxDirec :: Laberinto -> [String] -> Bool
auxDirec _   []       = True
auxDirec END _        = False
auxDirec (N _ []) _   = False
auxDirec (N IN xs) ac = any (`auxDirec` ac) xs
auxDirec (N AB xs) ac = any (`auxDirec` (filter (/= "AB") ac)) xs
auxDirec (N AR xs) ac = any (`auxDirec` (filter (/= "AR") ac)) xs
auxDirec (N DE xs) ac = any (`auxDirec` (filter (/= "DE") ac)) xs
auxDirec (N IZ xs) ac = any (`auxDirec` (filter (/= "IZ") ac)) xs



-- ----------------------------------------------------------------------------- 
-- Ejercicio 3 (2.5 puntos) Se puede definir el tipo de dato Matriz con 
-- coeficientes enteros de la siguiente forma:
--
type Matriz = Array (Int,Int) Int
--
-- Teniendo esto en cuenta, podemos representar un laberinto como en el juego 
-- del buscaminas, es decir según el número de posiciones posibles que te puedes 
-- desplazar a partir de una posición dada de la siguiente forma:
--
--   - Si la posición dada es # o uno de los bordes, dar 0 posiciones posibles.
--   - Si la posición es I devolver -1 caminos posibles
--   - Si la posición es X devolver 5 (así nos aseguramos que es el máximo).
--   - Cualquier otro caso, contar los caminos adyacentes posibles (los que no 
--     sean muro).
--
-- Por ejemplo, los laberintos:
--
-- laberinto0 = ["#######",             laberinto1 = ["#######",
--               "#I   x#",                           "#I#   #",
--               "#######"]                           "#   # #",
--                                                    "## ##x#",            
--                                                    "#######"]
-- Podemos representarlos con las matrices:
--
--  / 0 0  0 0 0 0 0 \                  / 0 0  0 0 0 0 0 \
--  | 0 -1 2 2 2 5 0 |                  | 0 -1 0 2 2 2 0 |
--  \ 0 0  0 0 0 0 0 /                  | 0 2  3 0 0 2 0 |
--                                      | 0 0  2 0 0 5 0 |
--                                      \ 0 0  0 0 0 0 0 /
--
--
-- Definir la función buscaminas :: [String] -> Matriz
-- tal que buscaminas lab devuelve la matriz en formato buscaminas
-- del laberinto lab. Por ejemplo:
--
-- > buscaminas laberinto0 
-- array ((1,1),(3,7)) [((1,1),0),((1,2),0),((1,3),0),((1,4),0),((1,5),0),((1,6),0),((1,7),0),
--                      ((2,1),0),((2,2),-1),((2,3),2),((2,4),2),((2,5),2),((2,6),1),((2,7),0),
--                      ((3,1),0),((3,2),0),((3,3),0),((3,4),0),((3,5),0),((3,6),0),((3,7),0)]
--
-- > buscaminas laberinto1
-- array ((1,1),(5,7)) [((1,1),0),((1,2),0),((1,3),0),((1,4),0),((1,5),0),((1,6),0),((1,7),0),
--                      ((2,1),0),((2,2),-1),((2,3),0),((2,4),2),((2,5),2),((2,6),2),((2,7),0),
--                      ((3,1),0),((3,2),2),((3,3),3),((3,4),2),((3,5),0),((3,6),2),((3,7),0),
--                      ((4,1),0),((4,2),0),((4,3),1),((4,4),0),((4,5),0),((4,6),1),((4,7),0),
--                      ((5,1),0),((5,2),0),((5,3),0),((5,4),0),((5,5),0),((5,6),0),((5,7),0)]
--
-- ----------------------------------------------------------------------------- 


buscaminas :: [String] -> Matriz
buscaminas lab = 
      array ((1,1),(f,c)) [( (i,j) , posCam i j ) | i<-[1..f], j <-[1..c]]
      where f = length lab 
            c = length (head lab) 
            posCam i j 
                  | (lab !! (i-1))!! (j-1) == '#' ||i == 1 || j == 1 || i == f || j == c = 0
                  | (lab !! (i-1))!! (j-1)  == 'X' = 5
                  | (lab !! (i-1))!! (j-1)  == 'I' = -1
                  | otherwise = length [ (lab !! a) !! b | 
                                         (a,b) <- [(i-2,j-1),(i,j-1),(i-1,j-2),(i-1,j)], 
                                         (lab !! a) !! b /= '#']


-- Propiedades
buscaminasProp rs (w, h) = (buscaminas ls) == (buscaminas' ls)  
        where chars = "################      xxxx"
              (_, _, ls) = fmaze chars rs (min w 5, min h 5)

-- -----------------------------------------------------------------------------   
-- Ejercicio 4. En este ejercicio se propone implementar un juego simple en el
-- que el usuario tiene que encontrar la salida de un laberinto. 
--
-- En este ejercicio de evaluará la competencias del alumno para implementar
-- programas interactivos con haskell, el manejo de E/S, así como el uso de 
-- colas.
--
-- Ejercicio 4.1 (0.5 puntos). Implementa una función que dada una cola de cadenas
-- escriba toda la cola a un archivo de texto especificado. 
--   
historial :: Cola String
historial = foldr inserta vacia ["AR", "AR", "AB", "DE", "DE", "AB", "IZ", "IZ"]
--
-- > guarda_cola historial "historial.log"
-- > :q
-- Leaving GHCi.
-- $ ls historial.log 
-- cola.log
-- $ cat historial.log 
-- IZ
-- IZ
-- AB
-- DE
-- DE
-- AB
-- AR
--
-- Notas:
--   - NO se debe implementar un tipo para las colas. Ya se aporta la implementación
--     de ColasConListas y está importado.
--   - Cada cadena de la cola debe ser almacenada en una línea en el archivo.
-- -----------------------------------------------------------------------------    


-- Examen
{--
guarda_cola = undefined 
--}

-- Sol
cola2Lista :: Cola a -> [a]
cola2Lista c
    | esVacia c = []
    | otherwise = pc : cola2Lista rc
    where pc = primero c
          rc = resto c

guarda_cola :: (Cola String) -> FilePath -> IO()
guarda_cola cola file = do
    let cadena = intercalate "\n" (cola2Lista cola)
    writeFile file cadena

    
-- -----------------------------------------------------------------------------             
-- Ejercicio 4.2 (0.5 puntos). Implementa una función que lea el archivo de 
-- configuración y devuelva una tupla de 5 elementos (dos enteros, una cadena
-- y otros dos enteros) con los valores de las configuraciones tal y como
-- aparece en config.lab. 
--
-- Este archivo contiene el número de filas y columnas que el usuario P ve desde 
-- su posición actual, además del carácter usado para los muros, así como el número 
-- de filas y columnas del terminal.
--
-- Esta función debe recibir un parámetro con el nombre del archivo de 
-- configuración. No es necesario comprobar la existencia del archivo.
--
-- > lee_configuracion "config.lab"
-- (10,10,"/",30,184)
-- > lee_configuracion "config2.lab"
-- (5,5,"#",20,100)
-- 
-- -----------------------------------------------------------------------------             


-- Examen
--lee_configuracion = undefined 


lee_configuracion :: FilePath -> IO (Int, Int, String, Int, Int)
lee_configuracion configFile = do
  config <- readFile configFile
  let values = [(head.tail.words) f | f <- (lines config)]
  return (read (values!!0)::Int, read (values!!1)::Int, values!!2, 
          read (values!!3)::Int, read (values!!4)::Int) 

-- -----------------------------------------------------------------------------             
-- Ejercicio 4.3 (1 puntos). Implementa una función *main* como base del comienzo
-- de un juego donde el usuario tendrá una visión parcial de un laberinto y podrá
-- moverse por el para encontrar la salida.
--
--                    ·····················
--                    ·/ / /// / / / /// /·
--                    ·/ /   / / / /   /  ·
--                    ·/ /// / /P/ /// / /·
--                    ·    / / / / /   / /·
--                    ·///// / /// / /////·
--                    ·····················
--
-- El programa, cuyo punto de entrada es esta función main, espera recibir dos
-- argumentos. El primero para especificar el nombre del archivo donde está 
-- almacenado el laberinto, y el segundo el archivo configuración.
--
-- Los pasos a seguir para su implementación serán los siguientes:
--        - Comprueba que se han pasado al programa dos argumentos.
--        - Si el número de argumentos es distinto de 2, imprimir por pantalla
--          un mensaje de aviso y salir.
--        - Si se dado los dos argumentos (archivos), hay que comprobar que ambos 
--          existen.
--        - Si alguno de ellos, o ambos, no existe, se debe imprimir un mensaje
--          o varios indicando que archivo(s) falta(n). Salir del programa.
--        - En el caso de existir ambos, se debe leer el archivo de laberinto
--          y la configuración (con lee_configuracion). El laberinto se debe
--          convertir en una lista de cadenas.
--        - Borrar la pantalla (función borrar) y mostrar el laberinto con 
--          (mostrar_laberinto).
--          
-- Nota: En el archivo Util/UtilidadesExameh.hs podeis ver la implementación
--       de las funciones borrar, mostrar_labertiono 
-- -----------------------------------------------------------------------------             

main :: IO ()
main = do
  args <- getArgs
  if length args == 2 then do
    let (labfile, configfile) = (head args, (head.tail) args)
    
    labfileExists <- doesFileExist labfile
    configfileExists <- doesFileExist configfile

    let errors = (map putStrLn ["Archivo de " ++ f ++ " no encontrado" | 
                            (f, e) <- zip ["Laberinto ", "Configuración "] [labfileExists, configfileExists],
                            not e])
    
    if (not.null) errors then
       sequence_ errors
    else do
       labtext <- readFile labfile
       config <- lee_configuracion configfile
       let lablines = (lines labtext)
       
       borrar
       mostrar_laberinto lablines (1, 1) config
       jugar lablines (1, 1) config historial_vacio
  else
    putStrLn "Debe indicar el nombre del archivo del laberinto y de configuración"
    

-- -----------------------------------------------------------------------------             
-- Ejercicio 4.4 (1.5 puntos). Implementar una función *jugar* para controlar todo 
-- el flujo del juego (pulsación de teclas, movimiento del jugador, control de
-- movimiento válido, control de encontrar salida, guardar historial y guardarlo):
--
-- Para esta función se recomienda seguir el siguiente pseudo-código:
--
--   Implementar una función *jugar* para controlar todo el flujo del programa:
--      - Esta función necesitará recibir el laberinto (lista de cadenas), la 
--        posición actual, la tupla de configuraciones  y una cola para guardar
--        el historial de movimientos.
--          - Guarda el historial con guarda_cola
--          - Le un carácter del teclado.
--          - Si este carácter es una 'q' o una 'Q' termina la ejecución.
--          - Convierte el carácter a movimiento (key2move) y comprueba si es un
--            movimiento válido (pos2mov).ç
--          - Si NO es un movimiento válido muestra un mensaje por pantalla tras
--            mostrar el laberinto (mostrar_laberinto).
--          - Si es un movimiento válido:
--             - Actualiza la cola historial con el numero movimiento.
--             - Obtén  la siguiente posición asociado al movimiento (sigPos).
--             - Muestra el laberinto (mostrar_laberinto)
--             - Si se ha encontrado el final, pos2mov te da esta información,
--               muestra un mensaje y termina.
--             - En otro caso continúa jugando.
--
-- -----------------------------------------------------------------------------             
  
historial_vacio :: Cola String
historial_vacio = vacia
  
jugar :: [String] -> (Int, Int) -> (Int, Int, String, Int, Int) -> Cola String -> IO()
jugar lab (i,j) config historial = do  
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  
  guarda_cola historial "historial.lab"

  c <- getChar
  let a = toUpper c
  if a /= 'Q' then do
    let move = key2move a
    let validos =  pos2mov lab (i,j)  
          
    borrar
    
    if elem move validos then do
      let nhistorial = inserta move historial
      
      let (ii,jj) = sigPos move (i,j)
      mostrar_laberinto lab (ii, jj) config
      
      if elem "END" (pos2mov lab (ii,jj)) then
         putStrLn "Enhorabuna, ha encontrado la salida"
      else
         jugar lab (ii,jj) config nhistorial
    else do
      mostrar_laberinto lab (i, j) config
      putStrLn "Movimiento no válido"
      jugar lab (i,j) config historial
        
  else do
    putStrLn "Ha salido del programa"

