# Resolución de Laberintos y Programación Interactiva en Haskell

Proyecto desarrollado en Haskell centrado en la resolución algorítmica de laberintos, representación mediante estructuras de árbol, manipulación de matrices y un motor interactivo por consola.

## Tecnologías Utilizadas

* **Haskell** (GHC)
* **Módulos principales:** `Data.Array`, `Data.List`, `Data.Char`, `System.IO`, `System.Environment`, `Test.QuickCheck`

## Estructura y Funcionalidades

El proyecto se divide en cuatro bloques principales:

### 1. Algoritmos de Búsqueda y Navegación en Laberintos
* **`elemento` y `vecinos`**: Obtención de elementos y posiciones contiguas (Norte, Sur, Este, Oeste) sobre estructuras bidimensionales.
* **`vecinos2mov` y `pos2mov`**: Conversión de vecinos en direcciones de movimiento válidas (`AR`, `AB`, `IZ`, `DE`), detectando la condición de fin (`END`).
* **`laberinto2moves`**: Transformación de una representación en texto plano a una matriz tridimensional de movimientos disponibles.
* **`muevete` y `sigPos`**: Cálculo de coordenadas resultantes tras efectuar un desplazamiento.
* **`buscarSalida`**: Algoritmo recursivo con backtracking para la detección y extracción del camino de salida del laberinto.

### 2. Estructuras de Datos Algebraicas y Árboles
* Definición de tipos de datos algebraicos `Con` (direcciones) y `Laberinto` (árbol de rutas posibles).
* **`caminosASalida`**: Filtrado y reconstrucción de subárboles con rutas válidas hacia la meta.
* **`todasDirec`**: Verificación de cobertura completa de direcciones a lo largo del árbol de caminos.

### 3. Representación Matricial (Estilo Buscaminas)
* **`buscaminas`**: Mapeo del laberinto a un tipo `Matriz` (`Data.Array`), asignando a cada casilla la cantidad de vías transitables adyacentes.

### 4. Entorno Interactivo E/S e Historial
* **`guarda_cola`**: Persistencia del historial de movimientos en fichero plano utilizando una cola.
* **`lee_configuracion`**: Lectura de parámetros de consola e interfaz gráfica textual.
* **`main` y `jugar`**: Bucle principal del juego interactivo con captura de eventos de teclado en tiempo real, validación de reglas de navegación y renderizado por consola.