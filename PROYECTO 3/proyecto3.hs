-- La calificación máxima de este examen práctico es de 7 puntos aunque
-- la suma total de los puntos de los ejercicios sea superior a 7. En caso
-- de que la suma de puntuaciones obtenidas sea superior a 7, esta se
-- truncará a 7 puntos.
--
-- - Obteniendo un calificación superior o igual a 4 puntos en este examen
-- práctico se asegura el aprobado de la asignatura aunque la suma de
-- las puntuaciones obtenidas de este examen y el cuestionario no sea
-- superior a 5 puntos.
--
-- - Los archivos Ofuscadas.hs y ExamenUtils.hs no deben ser modificados ni
-- manipulados en ningún sentido.
--
-- - Las entregas que produzcan algún error durante la carga NO SERÁN
-- EVALUADAS y se les asignará una calificación de 0 puntos.
--
---------------------------------------------------------------------

module Solucion where

import Test.QuickCheck
import Data.Char (isLetter, isDigit)
import Data.Array
import Control.Exception (catch, SomeException, evaluate)
import System.Directory
import Ofuscadas (suma', calcularNota', parsearRespuestas', ArbolRespuestas', inserta', respuestas_ordenadas', vacio')

import ExamenUtils (partitionEithers, parsearSoluciones, buscarSoluciones, IdTest, Soluciones, SolucionesCuestionario, PlantillaDeSoluciones)

---------------------------------------------------------------------
-- Ejercicio 1.1 (1.25 puntos) Implementar una función "suma a b" que reciba dos
-- números enteros no negativos representados como cadenas de caracteres y
-- devuelva su suma, también representada como cadena de caracteres.
--
-- No se permite el uso de funciones de conversión estándar como read, show,
-- digitToInt, intToDigit, ni ord o chr para transformar la cadena completa o
-- sus dígitos a tipos numéricos (Int, Integer).
--
-- La suma de los dígitos individuales debe realizarse utilizando exclusivamente
-- la funcionalidad que proporciona la clase de tipo Enum sobre los caracteres
-- ASCII, simulando una aritmética de Peano.
--
-- La solución debe ser recursiva y manejar números de cualquier longitud,
-- gestionando explícitamente el alineamiento de las cifras y el acarreo.
--
-- Ayuda: si las cadenas tienen longitudes diferentes, se recomienda que la
-- cadena más corta se rellene con espacios o ceros a la izquierda para alinear
-- las unidades, decenas, centenas, etc.
-- =
-- Para entender cómo funciona esta función bajo las restricciones estrictas (sin
-- convertir a enteros y usando aritmética de Peano), vamos a trazar la ejecución
-- del siguiente ejemplo: suma "28" "3". En primer lugar tenemos que alinear
-- correctamente los dígitos por lo que convertimos a suma "28" "03".
--
--      28
--    + 03
--    ------
--
-- Ahora tomamos los dígitos más a la derecha "8" y "3" y comenzamos a incrementar uno
-- y decrementar el otro hasta que alguno llegue a "0" o a "9". Se comienza con un
-- acarreo de "0"
--
--      Unidades (8 + 3):
--      8 -> 9 -> 0 -> 1
--      3 -> 2 -> 1 -> 0
--      Acarreo: 0 -> 0 -> 1 -> 1
--
--      Resultado: 1, acarreo: 1
--
-- Continuamos con la decenas pero teniendo en cuenta el acarreo:
--
--      Decenas (2 + 0 + acarreo 1):
--      2 -> 2 -> 3
--      0 -> 1 -> 0
--      Acarreo: 1 -> 0 -> 0
--
--      Resultado: 3, acarreo: 0
--
-- Por lo que el resultado serán las decenas "3" concatenado con las unidades "1",
-- es decir "31".
--
-- Ejemplos:
--
-- > suma "123456" "456"
-- "123912"
-- > suma "0" "123"
-- "123"
-- > suma "123456" ""
-- "*** Exception: Alguno de los números no es válido
-- > suma "" "211231"
-- "*** Exception: Alguno de los números no es válido
-- > suma "ab" "211231"
-- "*** Exception: Alguno de los números no es válido
-- ---------------------------------------------------------------------

type Numero = [Char]
type Acarreo = [Char]

numeros = ['0'..'9']

suma :: Numero -> Numero -> Numero
suma "" _ = error "Alguno de los números no es válido"
suma _ "" = error "Alguno de los números no es válido"
suma a b
  | all numero_valido [a,b] = (uncurry ac_final) $ suma_aux a b "0"
  | otherwise = error "Alguno de los números no es válido"
  where ac_final s "0" = s
        ac_final s a = a ++ s
        numero_valido = all ((flip elem) numeros)

suma_aux :: Numero -> Numero -> Acarreo -> (Numero, Acarreo)
suma_aux " " ys a = suma_aux ys a "0"
suma_aux xs " " a = suma_aux xs a "0"

suma_aux xs "0" "0" = (xs, "0")
-- suma_aux "0" ys "0" = (ys, "0") -- No es necesaria
suma_aux xs "0" a = suma_aux xs a "0"
suma_aux "0" ys a = suma_aux ys a "0"

suma_aux "9" [y] a = (s, [succ a'])
  where (s, a'_) = suma_aux "0" [pred y] a
suma_aux [x] [y] a = suma_aux [succ x] [pred y] a

suma_aux xs@(x:as) ys@(y:bs) ac
  | lxs == lys = (rl ++ rr, cl)
  | lxs > lys = suma_aux xs (' ':ys) ac
  | otherwise = suma_aux (' ':xs) ys ac
  where lxs = length xs
        lys = length ys
        (rr, cr) = suma_aux as bs ac
        (rl, cl) = suma_aux [x] [y] cr

---------------------------------------------------------------------
-- Ejercicio 1.2 (0,25 puntos) Implementa una propiedad para comprobar que la
-- implementación anterior efectivamente lleva a cabo la suma de forma correcta.
-- La propiedad debe recibir dos número enteros (Int).
--
-- Nota: puede usar la implementación auxiliar "suma" para implementar la
-- propiedad. En este ejercicio si está permitido usar "read" y "show".
---------------------------------------------------------------------

suma_correcta :: Int -> Int -> Bool
suma_correcta a b = read (suma a' b') == abs a + abs b
  where [a', b'] = map (show.abs) [a, b]

suma_correcta' :: Int -> Int -> Property
suma_correcta' a b = a >= 0 && b >= 0 ==> read (suma a' b') == abs a + abs b
  where [a', b'] = map (show.abs) [a, b]

-- quickCheck suma_correcta

---------------------------------------------------------------------
-- Ejercicio 2.1 (1,00 puntos) Implementa la función "calcularNota ras rcs pa pf"
-- que evalúe una lista de respuestas 'ras' dadas por un alumno teniendo una
-- plantilla de soluciones 'rcs'. El sistema debe contemplar preguntas
-- sin contestar por parte del alumno y aplicar una penalización por fallos 'pf',
-- asegurando que la nota final nunca sea negativa. Cada acierto tendrá una
-- puntuación 'pa'.
--
-- La función debe devolver el número de acierto, el número de fallos y la
-- calificación total.
--
-- > ras = [Just 'a', Nothing, Just 'b', Just 'b']
-- > rcs = ['a', 'b', 'c', 'b']
-- > calcularNota ras rcs 0.1 0.05
-- (2,1,0.15000000000000002)
--
-- rcs = [1, 3, 2, 1]
-- > ras = [Just 2, Just 3, Just 1, Just 2]
-- > calcularNota ras rcs 0.1 0.05
-- (1,3,0.0)
---------------------------------------------------------------------

calcularNota :: (Eq a) => [Maybe a] -> [a] -> Double -> Double -> (Int, Int, Double)
calcularNota respuestasUsuario respuestas ptsAcierto ptsResta =
    (aciertos, fallos, max 0.0 (aciertos' * ptsAcierto - fallos' * ptsResta))
  where
    (aciertos, fallos) = foldl paso (0, 0) (zip respuestasUsuario respuestas)
    [aciertos', fallos'] = fromIntegral <$> [aciertos, fallos]
    paso (aciertos, fallos) (Nothing, _) = (aciertos, fallos)
    paso (aciertos, fallos) (Just r, correcta)
      | r == correcta = (aciertos + 1, fallos)
      | otherwise    = (aciertos, fallos + 1)

---------------------------------------------------------------------
-- Ejercicio 2.2 (1,00 puntos) Estás desarrollando un módulo para gestionar las
-- calificaciones de un examen. El sistema necesita exportar e importar estos
-- datos a un formato ligero de texto (CSV) para ser procesado por otras
-- herramientas.
--
-- Se dispone del siguiente tipo de dato algebraico:

type Puntuacion = Double

data Calificacion = Calificacion {
    uvus::String,
    aciertos:: Int,
    fallos:: Int,
    puntuacion :: Puntuacion
} deriving (Eq)

-- Haskell permite derivar automáticamente las instancias de Show y Read, pero
-- el formato por defecto es verboso (ej: Calificacion {aciertos = 10...}). Se
-- solicita implementar manualmente las instancias de las clases de tipos Show
-- y Read para el tipo Calificacion cumpliendo los siguientes requisitos de
-- serialización CSV:
--
-- - Instancia Show: Debe convertir el objeto en una cadena de texto plana
-- con los valores de los campos separados únicamente por comas (,), sin
-- espacios adicionales, ni nombres de campos, ni el nombre del
-- constructor.
--
-- - Instancia Read: Debe ser capaz de reconstruir un objeto Calificacion
-- a partir de una cadena con el formato descrito anteriormente.
--
-- Para considerar el ejercicio correcto, debe cumplirse la propiedad de
-- identidad de la serialización:
--
-- cal = Calificacion "davsolmar" 20 5 9.5
-- > read (show cal) == cal
--
-- Ejemplos:
--
-- ghci> cal = Calificacion "davsolmar" 20 5 9.5
--
-- Prueba de Show
-- ghci> show cal
-- "davsolmar,20,5,9.5"
--
-- ghci> read "davsolmar,20,5,9.5" :: Calificacion
-- Calificacion {uvus = "davsolmar", aciertos = 20, fallos = 5, puntuacion = 9.5}
--
-- Ayuda: para solucionar este ejercicio puede usar la función 'break' del
-- preludio cuya firma es break :: (a -> Bool) -> [a] -> ([a],[a]) y
-- divide una lista en dos mediante un predicado o condición:
--
-- > break (==3) [1,2,3,4,5]
-- ([1,2],[3,4,5])
--
-- Nota: Para instanciar Read se quiere la implementación de "readsPrec _ s"
-- donde s es la cadena de caracteres a parsear. Implementar la función
-- "readCal s" que devuelve la instancia de Calificacion y usarla para
-- implementar readsPrec como readsPrec = [(readCal s, "")].
---------------------------------------------------------------------

instance Show Calificacion where
  show (Calificacion u a f p) = show u ++ "," ++ show a ++ "," ++ show f ++ "," ++ show p

-- split quita el separador de la parte derecha
split :: (a -> Bool) -> [a] -> ([a], [a])
split _ [] = ([], [])
split p str = (ls, tail' rs)
  where (ls, rs) = break p str
        tail' [] = []
        tail' xs = tail xs

-- aplica split hasta que no encuentra más comas ","
parseCSV :: String -> [String]
parseCSV [] = []
parseCSV str
  | null rs = [ls]
  | otherwise = ls:parseCSV rs
  where (ls, rs) = split (== ',') str

readCal :: String -> Calificacion
readCal s
  | null rs = error "read: no parse"
  | length vs < 3 = error "read: no parse"
  | otherwise = Calificacion (read u) (read a) (read f) (read p)
  where
    (u, rs) = split (==',') s
    vs = parseCSV rs
    (a:f:p:_) = vs

instance Read Calificacion where
  readsPrec _ s = [(readCal s, "")]

---------------------------------------------------------------------
-- Ejercicio 2.3 (1,00 puntos) Implementa la función parsearRespuestas rs que
-- recibe una cadena de caracteres procedente de la lectura de un archivo con el
-- siguiente formato:
--
-- Código: X
-- Pregunta 1: R1
-- Pregunta 2: R2
-- ...
-- ...
-- donde X es el código que identifica al cuestionario y RY corresponde a la
-- respuesta que ha dado el alumno para la pregunta Y. Tenga en cuenta, que
-- cualquier pregunta puede quedar sin responder y que se considera que el
-- formato del archivo es siempre correcto con las preguntas ordenadas de
-- menor a mayor.
--
-- > parsearRespuestas "Código: 3\nPregunta 1: b\nPregunta 2:\nPregunta 3: c"
-- (3,[(1, Just 'b'), (2, Nothing), (3, Just 'c')])
--
-- > parsearRespuestas "Código: 3\nPregunta 1: b\nPregunta 2: a\nPregunta 3: c"
-- (3,[(1, Just 'b'), (2, Just 'a'), (3, Just 'c')])
---------------------------------------------------------------------

parsearRespuestas :: (Read a) => String -> (IdTest, [(Int, Maybe a)])
parsearRespuestas contenido = (idTest, respuestas)
  where
    lineas@(primeraLinea:restoLineas) = lines contenido

    -- Extrae el ID de la primera línea "Código: _"
    idTest = read $ filter isDigit primeraLinea

    -- Procesar el resto de líneas "Pregunta X: ..."
    respuestas = map extraerRespuesta restoLineas

extraerRespuesta :: (Read a) => String -> (Int, Maybe a)
extraerRespuesta linea = (read istr, parseRespuesta $ filter (/= ' ') r)
  where (p , r) = split (==':') linea -- extrae ": ..."
        (_, istr) = split(== ' ') p

parseRespuesta :: (Read a) => String -> Maybe a
parseRespuesta [] = Nothing -- Si no hay una respuesta
parseRespuesta [c]
  | isLetter c = Just (read ("\""++[c]++"\"")) -- Tenemos un carácter
  | otherwise = Just (read [c])
parseRespuesta str = Just (read str) -- Convierte al tipo

---------------------------------------------------------------------
-- Ejercicio 2.4. (1,00 puntos) Al leer las respuestas de un examen, es posible
-- que las preguntas no se procesen en orden secuencial (por ejemplo, si el
-- archivo está desordenado o si se procesan en paralelo). Para calcular la nota
-- correctamente, necesitamos reconstruir la secuencia correcta de respuestas
-- (Pregunta 1, Pregunta 2, etc.).
--
-- Dada la siguiente implementación de árbol binario:

data ArbolRespuestas a = Vacio | Nodo (Int, a) (ArbolRespuestas a) (ArbolRespuestas a)
  deriving (Show, Read)

vacio = Vacio

-- Implementa la función "inserta (id, respuesta) a" que inserta de manera
-- ordenada en el árbol la tupla (id, respuesta) según el id y la función
-- "respuestas_ordenadas ar" que dado un árbol binario ordenado "ar" extraiga todas
-- las respuestas asociadas de manera ordenada.
--
-- > a = inserta (1, "a") (inserta (4, "c") (inserta (3, "b") (inserta (8, "d") vacio)))
-- > respuestas_ordenadas a
-- ["a","b","c","d"]
-- > a = inserta (1, "a") (inserta (4, "b") (inserta (3, "a") (inserta (8, "c") vacio)))
-- > respuestas_ordenadas a
-- ["a","a","b","c"]
-- respuestas_ordenadas vacio
-- []
-- > a = inserta (1, 1) (inserta (4, 3) (inserta (3, 2) (inserta (8, 4) vacio)))
-- respuestas_ordenadas a
-- [1,2,3,4]
---------------------------------------------------------------------

inserta :: (Int, a) -> ArbolRespuestas a -> ArbolRespuestas a
inserta par Vacio = Nodo par Vacio Vacio
inserta par@(id, str) (Nodo nodo@(nid, nstr) izq der)
  | id < nid = Nodo nodo (inserta par izq) der
  | id > nid = Nodo nodo izq (inserta par der)
  | otherwise = Nodo par izq der

respuestas_ordenadas :: ArbolRespuestas a -> [a]
respuestas_ordenadas Vacio = []
respuestas_ordenadas (Nodo (_,ch) izq der) = respuestas_ordenadas izq ++ [ch] ++ respuestas_ordenadas der

---------------------------------------------------------------------
-- Ejercicio 2.5. (1,25 puntos) Desarrolla un programa ejecutable desde línea de
-- comandos para automatizar la corrección de una carpeta llena de las respuestas
-- de los alumnos a un cuestionario tipo test. En el directorio habrá un archivo
-- por cada alumno con el formato descrito en el archivo anterior. Puede usar la
-- función 'listDirectory dir' que devuelve la lista de archivos dentro de un
-- directorio 'dir'.
--
-- Además será necesario leer y parsear el archivo con las plantillas de
-- soluciones. Una vez leído el archivo de soluciones "soluciones_cuestionarios.txt"
-- puede usar la función parsearSoluciones importándola.
--
-- Por cada conjunto de respuestas de usuario, junto con las plantillas, será
-- necesario buscar las soluciones correspondiente al código indicado el
-- archivo de respuestas.
--
-- Una vez encontradas las respuestas se deberá obtener el número de aciertos,
-- fallos y la calificación para el alumno y generar una instancia del tipo
-- de dato Calificacion. El uvus se deberá extraer el nombre del archivo de
-- respuestas. Puede usar la función "calcularNota" de las funciones ofuscadas
-- si no resolvió el ejercicio 2.1. Los puntos asignados a los aciertos y fallos
-- serán los siguientes:
--
-- puntosAcierto :: Double
-- puntosAcierto = 0.1
--
-- puntosFallo :: Double
-- puntosFallo = 0.05
--
-- Una vez generadas todas las calificaciones se deberán guardar en un archivo
-- de texto llamado 'soluciones_cuestionarios.txt' en formato CSV (uvus, aciertos,
-- fallos, calificación). A la hora de parsear las respuestas de los alumnos, puede
-- ocurrir errores. Se deberá gestionar esta situación y esa lista de errores
-- se deberán guardar en 'errores.txt' teniendo cada línea el formato
-- "<uvus>:: <mensaje de error>".
--
-- $ runhaskell examen.hs
-- Todo procesado
--
-- $ cat calificaciones.txt
-- "basico",3,1,0.25000000000000006
-- "dasolma",11,9,0.65000000000000001
-- "dasolma2",11,9,0.65000000000000001
--
-- $ cat errores.txt
-- RGP123:: Error al parsear usuario: Prelude.read: no parse
---------------------------------------------------------------------

type Respuestas = String
type RespuestasAlumno a = [Maybe a]

procesa_respuestas :: String -> Respuestas -> RespuestasAlumno Char -> Calificacion
procesa_respuestas uvus' respuestasCuestionario respuestasAlumno =
    Calificacion { uvus = uvus',
                   aciertos = aciertos',
                   fallos = fallos',
                   puntuacion = nota' }
  where (aciertos', fallos', nota') = calcularNota respuestasAlumno
                                                   respuestasCuestionario
                                                   puntosAcierto
                                                   puntosFallo

parse_respuestas :: Respuestas -> FilePath -> IO (Either String String)
parse_respuestas soluciones filePath = do
    let (_, fileName) = break (== '/') filePath
    let (_, uvus') = break (== '_') fileName
    let ((_:uvus), _) = break (== '.') uvus'
    catch
      ( do
         respuestas <- readFile filePath

         -- Soluciones
         let plantillas = parsearSoluciones soluciones -- [(IdTest, String)]

         -- Respuestas del alumno (IdTest, [(NumPregunta, [Maybe Char])])
         let (idtest, respuestasAlumno') = parsearRespuestas respuestas :: (IdTest, [(Int, Maybe Char)])

         -- Insertamos en arbol y extraemos respuestas ordenadas
         let respuestasAlumno = respuestas_ordenadas ( foldr inserta Vacio respuestasAlumno')

         case (buscarSoluciones idtest plantillas) of
           Left error -> return $ Left (uvus ++ "::" ++ error)
           Right respuestas -> do
             let resultado = show (procesa_respuestas uvus respuestas respuestasAlumno)

             _ <- evaluate (length resultado) -- Forzamos la evaluación para recorrer el error si se produce

             return $ Right resultado
      ) (\e -> return $ Left $ uvus ++ ":: Error al parsear usuario: " ++ show (e :: SomeException))

save :: [Either String String] -> IO ()
save [] = return ()
save ((Left e):xs) = appendFile "errores.txt" (e ++ "\n") >> save xs
save ((Right r):xs) = appendFile "calificaciones.txt" (r ++ "\n") >> save xs

main :: IO ()
main = do
  contenidoSoluciones <- readFile "soluciones_cuestionarios.txt"
  let dir = "respuestas_cuestionarios/"
  files <- listDirectory dir
  let files' = (map (dir++) files)
  todo <- sequence $ map (parse_respuestas contenidoSoluciones) files'
  let (errores, datos) = partitionEithers todo

  -- Vaciamos
  writeFile "calificaciones.txt" ""
  writeFile "errores.txt" ""

  -- Agregamos
  mapM_ (appendFile "calificaciones.txt") $ map (++"\n") datos
  mapM_ (appendFile "errores.txt") $ map (++"\n") errores

  putStrLn "Todo procesado"

---------------------------------------------------------------------
-- Ejercicio 3. (1.25 puntos) Defina una función "contarPatron" que, dadas dos
-- matrices "a" y "b", cuente cuántas veces aparece el patrón "b" completamente
-- dentro de "a".
--
-- Para ello, se debe deslizar la matriz "b" por todas las posiciones posibles
-- de "a", verificando en cada posición si los elementos coinciden exactamente.
-- La matriz "b" debe estar completamente contenida dentro de "a" en cada
-- comparación (sin que ninguna parte de "b" sobresalga fuera de los límites de
-- "a").
--
-- Ejemplo visual:
-- Si a = [1,2,3]  y  b = [2,3]
--        [4,5,6]         [5,6]
--        [7,8,9]
--
-- Las posiciones válidas para colocar b son:
--
-- - Posición (0,0): compara [1,2] con [2,3] -> NO coincide
--                           [4,5]     [5,6]
--
-- - Posición (0,1): compara [2,3] con [2,3] -> SÍ coincide ✓
--                           [5,6]     [5,6]
--
-- - Posición (1,0): compara [4,5] con [2,3] -> NO coincide
--                           [7,8]     [5,6]
--
-- - Posición (1,1): compara [5,6] con [2,3] -> NO coincide
--                           [8,9]     [5,6]
--
-- Resultado: contarPatron a b = 1 (solo coincide en la posición (0,1))
--
-- Por ejemplo, dadas las siguientes matrices:
--
-- a = array ((1,1),(3,3)) [((1,1),'a'),((1,2),'a'),((1,3),'a'),
--                          ((2,1),'a'),((2,2),'a'),((2,3),'a'),
--                          ((3,1),'a'),((3,2),'a'),((3,3),'a')]
--
-- b = array ((1,1),(1,1)) [((1,1),'a')]
-- c = array ((1,1),(2,2)) [((1,1),'a'),((1,2),'a'),
--                          ((2,1),'a'),((2,2),'a')]
--
-- a' = array (('a','a'),('c','c')) [(('a','a'),1),(('a','b'),1),(('a','c'),1),
--                                   (('b','a'),1),(('b','b'),1),(('b','c'),1),
--                                   (('c','a'),1),(('c','b'),1),(('c','c'),1)]
--
-- c' = array (('a','a'),('b','b')) [(('a','a'),1),(('a','b'),1),
--                                   (('b','a'),1),(('b','b'),1)]
--
-- > contarPatron a b == 9
-- > contarPatron a a == 1
-- > contarPatron b a == 0
-- > contarPatron a c == 4
-- > contarPatron a' c' == 4
---------------------------------------------------------------------

type Matriz i a = Array (i,i) a

submatriz :: (Ix i, Enum i) => (i,i) -> (i,i) -> Matriz i a -> Matriz i a
submatriz (filaInicio, colInicio) (filaFin, colFin) matriz =
  array ((filaInicio,colInicio), (filaFin, colFin))
    [((i,j), matriz!(i,j)) | i <- [filaInicio..filaFin],
                             j <- [colInicio..colFin]]

contarPatron :: (Ix i, Eq a, Enum i) => Matriz i a -> Matriz i a -> Int
contarPatron a b = length [() | (fila, i) <- zip filas [0..],
                                (col, j) <- zip columnas [0..],
                                elems ( submatriz (fila, col) (filas1!!(i+alto-1),
                                       columnas1!!(j+ancho-1)) a) == elementos_b]
  where ((af1,ac1),(af2,ac2)) = bounds a
        ((bf1,bc1),(bf2,bc2)) = bounds b
        (filas1, columnas1) = (range (af1,af2), range (ac1, ac2))
        (filas2, columnas2) = (range (bf1,bf2), range (bc1, bc2))
        (ancho, alto) = (length columnas2, length filas2)
        drop' n = reverse . drop n . reverse
        (filas, columnas) = (drop' (alto-1) filas1, drop' (ancho-1) columnas1)
        elementos_b = elems b
