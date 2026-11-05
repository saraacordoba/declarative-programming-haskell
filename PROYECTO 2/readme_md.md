# Algoritmos, Estructuras de Datos y Cifrado en Haskell

Colección de implementaciones algorítmicas en Haskell que abarca manejo avanzado de listas, funciones de orden superior, estructuras de árboles de recomendación, operaciones con matrices y decodificación de ficheros.

## Tecnologías Utilizadas

* **Haskell** (GHC)
* **Módulos principales:** `Data.Array`, `Data.List`, `Data.Char`, `System.IO`, `System.Environment`, `Test.QuickCheck`

## Contenido del Proyecto

### 1. Procesamiento de Listas y Verificación
* **`nubnR`, `nubnRA`, `nubnF`**: Tres aproximaciones (recursión directa, acumuladores y plegados con `foldr`) para eliminar elementos repetidos más de $N$ veces en una lista.
* **`prop_nub1`**: Validación mediante QuickCheck para garantizar equivalencia con la función estándar `nub`.

### 2. Programación Funcional de Orden Superior
* **`incrustaFiltra`**: Pipeline funcional que incrusta elementos, aplana niveles de anidación (`concat`) y aplica predicados de filtrado mediante composición pura.

### 3. Listas Infinitas y Evaluación Lazada
* **`cubicosSuma11`**: Generador de secuencias infinitas de ternas de números cúbicos consecutivos cuya suma cumple el criterio de ser divisible por 11.

### 4. Árboles de Recomendación y Estado
* Definición del tipo algebraico `Recomendador a` parametrizado con colas para la rotación de elementos.
* **`recomendar`**: Algoritmo de evaluación de datos contextuales contra umbrales de decisión para retornar la recomendación óptima y actualizar el estado de la cola.

### 5. Álgebra Matricial y Submatrices
* **`subMatriz` y `(||||)`**: Definición de constructores de submatrices y concatenación horizontal entre instancias de `Array (Int, Int)`.
* **`traspuestaBloques`**: Algoritmo de transposición por bloques cuadrantes de una matriz de dimensión arbitraria.

### 6. Decodificación de Archivos y E/S
* **`decodifica_linea` y `main`**: Script de lectura y procesamiento de mensajes cifrados en ficheros `.enc`, interpretando valores ASCII para reconstruir y persistir el texto resultante.