module Oraculo
  ( crearOraculo,
    ramificar,
    prediccion,
    pregunta,
    opciones,
    respuesta,
    -- obtenerCadena,
    obtenerEstadisticas,
    Opciones,
    Oraculo (..),
  )
where

import Data.Map (Map, elems, toList, fromList, (!))

-- Definición de tipos
type Opciones = Map String Oraculo

data Oraculo = Prediccion String | Pregunta String Opciones
  deriving (Eq, Show, Read)

-- Función para crear un Oráculo a partir de una predicción.
-- Argumentos de entrada:
--   - pre: La predicción.
-- Valor de retorno:
--   - El Oráculo creado a partir de la predicción.
crearOraculo :: String -> Oraculo
crearOraculo = Prediccion

-- Función para ramificar un Oráculo en una pregunta con opciones.
-- Argumentos de entrada:
--   - opciones: Una lista de opciones.
--   - oraculos: Una lista de Oráculos correspondientes a las opciones.
--   - pregunta: La pregunta que distingue las opciones.
-- Valor de retorno:
--   - El Oráculo ramificado con las opciones y la pregunta.
ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar opciones oraculos pregunta = Pregunta pregunta $ fromList $ zip opciones oraculos

-- Función para obtener la predicción de un Oráculo.
-- Argumentos de entrada:
--   - oraculo: El Oráculo del que se desea obtener la predicción.
-- Valor de retorno:
--   - La predicción contenida en el Oráculo.
prediccion :: Oraculo -> String
prediccion (Pregunta _ _) = error "No puede obtener la predicción de una pregunta"
prediccion (Prediccion pre) = pre

-- Función para obtener la pregunta de un Oráculo.
-- Argumentos de entrada:
--   - oraculo: El Oráculo del que se desea obtener la pregunta.
-- Valor de retorno:
--   - La pregunta contenida en el Oráculo.
pregunta :: Oraculo -> String
pregunta (Pregunta txt _) = txt
pregunta _ = error "El oráculo no es una pregunta"

-- Función para obtener las opciones de un Oráculo.
-- Argumentos de entrada:
--   - oraculo: El Oráculo del que se desean obtener las opciones.
-- Valor de retorno:
--   - Un mapa de opciones asociadas a Oráculos.
opciones :: Oraculo -> Opciones
opciones (Pregunta _ opciones) = opciones
opciones (Prediccion _) = error "No puede obtener las opciones de una predicción"

-- Función para obtener la respuesta a una opción de un Oráculo de tipo pregunta.
-- Argumentos de entrada:
--   - oraculo: El Oráculo de tipo pregunta.
--   - opcion: La opción seleccionada.
-- Valor de retorno:
--   - El Oráculo correspondiente a la opción seleccionada.
respuesta :: Oraculo -> String -> Oraculo
respuesta oraculo opcion =
  case oraculo of
    Pregunta _ opciones -> opciones Data.Map.! opcion
    Prediccion _ -> error "No puede responder a una predicción"
    

-- Obtiene una cadena de preguntas y opciones para alcanzar una predicción en el Oráculo.
--   Si la predicción no pertenece al Oráculo, retorna Nothing.
--   De lo contrario, retorna Just lista, donde lista es una lista de tuplas (de dos cadenas de caracteres)
--   que corresponden a todas las preguntas que deben hacerse, a partir de la raíz del Oráculo, para alcanzar
--   la predicción suministrada y el valor de la opción escogida.
--
-- Argumentos de entrada:
--   - oraculo: El Oráculo en el que se buscará la cadena de preguntas y opciones.
--   - prediccion: La cadena de caracteres que representa la predicción deseada.
-- Valor de retorno:
--   - Nothing si la predicción no pertenece al Oráculo. Just [(String, String)] si la predicción se encuentra,
--     donde la lista contiene las preguntas y opciones para llegar a la predicción.
obtenerCadena :: Oraculo -> String ->  Maybe [(String, String)]
obtenerCadena oraculo pred = listToMaybe $ obtenerCadenaAux oraculo pred []

obtenerCadenaAux :: Oraculo -> String  -> [(String, String)] -> [(String, String)]
obtenerCadenaAux oraculo pred camino  =
  case oraculo of
    Prediccion prediccion' | pred == prediccion'->  camino 
    Pregunta pregunta opc -> concatMap (\(opcion, oraculo) ->  obtenerCadenaAux oraculo pred( camino ++ [(pregunta, opcion)])) (toList opc)
    _ -> []

listToMaybe :: [(String, String)] -> Maybe [(String, String)]
listToMaybe [] = Nothing
listToMaybe lista = Just lista

-- Función para obtener estadísticas de profundidad de un Oráculo.

-- Calcula la profundidad mínima, máxima y promedio en el árbol de decisiones.
-- Argumentos de entrada:
--   - oraculo: El Oráculo que representa el árbol de decisiones.
-- Valor de retorno:
--   - Una tupla con tres valores: (profundidad mínima, profundidad máxima, promedio de profundidades).
obtenerEstadisticas :: Oraculo -> (Int, Int, Double)
obtenerEstadisticas oraculo = (minAltura, maxAltura, avgAltura)
  where alturas = map snd (profundidad oraculo 0)
        minAltura = minimum alturas
        maxAltura = maximum alturas
        avgAltura = promedio alturas

-- Función para calcular el promedio de una lista de enteros.
-- Argumentos de entrada:
--   - lista: La lista de enteros de la cual se calculará el promedio.
-- Valor de retorno:
--   - El promedio de la lista como un número de punto flotante (Double).
promedio :: [Int] -> Double
promedio lista =
  let suma = fromIntegral (sum lista)
      cantidad = fromIntegral (length lista)
   in suma / cantidad

-- Función para calcular las alturas de las Predicciones en un árbol.
-- Argumentos de entrada:
--   - oraculo: El oráculo que representa el árbol de decisiones.
--   - nivel: El nivel de profundidad actual.
-- Valor de retorno:
--   - Una lista de tuplas donde cada tupla contiene una Predicción y su altura en el árbol.
profundidad :: Oraculo -> Int -> [(Oraculo, Int)]
profundidad (Prediccion pred) nivel = [(Prediccion pred, nivel)]
profundidad (Pregunta preg opc) nivel = concatMap (\oraculo -> profundidad oraculo (nivel + 1)) (elems opc)
