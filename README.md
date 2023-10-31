# Haskinator

Haskinator es una aplicación de predicción y consulta basada en un árbol de decisiones implementada en Haskell. Permite a los usuarios crear, cargar, persistir, consultar y obtener estadísticas sobre un oráculo.

## Requisitos

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/)
- Bibliotecas base y containers de Haskell

Asegúrate de tener GHC y las bibliotecas necesarias instaladas en tu sistema antes de compilar y ejecutar el proyecto.

## Compilación

Para compilar el proyecto, utiliza el siguiente comando:

```
make
```

Esto generará un ejecutable llamado `haskinator`.

## Uso

Para ejecutar Haskinator, simplemente utiliza:

```
./haskinator
```

Esto iniciará la aplicación y te permitirá realizar las siguientes acciones:

- **Crear**: Crea un nuevo oráculo a partir de una predicción.
- **Predecir**: Realiza predicciones y permite corregirlas.
- **Persistir**: Guarda el oráculo actual en un archivo.
- **Cargar**: Carga un oráculo desde un archivo.
- **Consultar**: Consulta el oráculo y obtén respuestas basadas en predicciones.
- **Estadísticas**: Obtiene estadísticas sobre la profundidad del árbol de decisiones.
- **Salir**: Sale de la aplicación.

## Ejemplos

- Para crear un nuevo oráculo a partir de una predicción, selecciona "Crear" y proporciona el texto de la predicción.
- Para predecir, selecciona "Predecir" y sigue las instrucciones.
- Para persistir un oráculo en un archivo, selecciona "Persistir" y proporciona el nombre del archivo.
- Para cargar un oráculo desde un archivo, selecciona "Cargar" y proporciona el nombre del archivo.
- Para consultar, selecciona "Consultar" y sigue las instrucciones.
- Para obtener estadísticas, selecciona "Estadísticas".
- Para salir, selecciona "Salir".

## Integrantes

Gabriel Chaurio ()
Miguel Perez ()
Jose Perez (16-10882)