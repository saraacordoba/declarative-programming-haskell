# Sistema de Evaluación Cuestionarios y Aritmética de Peano en Haskell

Proyecto desarrollado en Haskell que aborda tres bloques principales de ingeniería de software funcional: aritmética arbitraria simulada, un motor automatizado de corrección de cuestionarios tipo test con persistencia de archivos, y algoritmos de coincidencia de patrones en matrices 2D.

## Tecnologías Utilizadas

* **Haskell** (GHC)
* **Módulos principales:** `Data.Array`, `Data.Char`, `Control.Exception`, `System.Directory`, `Test.QuickCheck`

## Estructura y Funcionalidades

### 1. Aritmética Simulada de Peano sobre Cadenas
* **`suma` / `suma_aux`**: Implementación recursiva de la suma de enteros positivos sin conversión directa a tipos numéricos (`Int`, `Integer`), basada en la clase de tipos `Enum` (`succ`, `pred`) sobre caracteres ASCII. Gestiona explícitamente el alineamiento de cifras y el arrastre de acarreo.
* **`suma_correcta` / `suma_correcta'`**: Propiedades para la validación del algoritmo aritmético mediante **QuickCheck**.

### 2. Motor Automatizado de Calificación de Cuestionarios
* **`calcularNota`**: Evaluación de respuestas del usuario comparadas contra plantillas de solución, gestionando aciertos, respuestas omitidas (`Nothing`) y penalización por fallos.
* **Serialización Personalizada (`Show` / `Read`)**: Implementación manual de clases de tipos para el dato algebraico `Calificacion`, permitiendo importar y exportar registros en formato CSV estándar.
* **`parsearRespuestas`**: Parseo estructurado de ficheros de respuesta individuales, asignando identificadores de prueba y respuestas procesadas.
* **`ArbolRespuestas a`**: Árbol binario de búsqueda (`Nodo`, `Vacio`) para la reordenación eficiente de respuestas recibidas fuera de secuencia.
* **`main` / `parse_respuestas`**: Pipeline principal de entrada/salida (I/O) que escanea directorios, procesa soluciones globales, gestiona excepciones en runtime con `Control.Exception.catch` y genera reportes de `calificaciones.txt` y `errores.txt`.

### 3. Coincidencia de Patrones en Matrices 2D
* **`submatriz`**: Extracción de subsecciones bidimensionales sobre tipos `Array (i, i) a`.
* **`contarPatron`**: Algoritmo de ventana deslizante (*sliding window*) para contar las ocurrencias exactas de una matriz patrón $B$ contenida dentro de una matriz origen $A$.