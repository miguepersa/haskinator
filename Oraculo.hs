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

import Data.Map (Map, elems, fromList, (!))

type Opciones = Map String Oraculo

data Oraculo = Prediccion String | Pregunta String Opciones
  deriving (Eq, Show, Read)

crearOraculo :: String -> Oraculo
crearOraculo = Prediccion

ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar opciones oraculos pregunta = Pregunta pregunta $ fromList $ zip opciones oraculos

prediccion :: Oraculo -> String
prediccion (Pregunta _ _) = error "No puede obtener la predicción de una pregunta"
prediccion (Prediccion pre) = pre

pregunta :: Oraculo -> String
pregunta (Pregunta txt _) = txt
pregunta _ = error "El oráculo no es una pregunta"

opciones :: Oraculo -> Opciones
opciones (Pregunta _ opciones) = opciones
opciones (Prediccion _) = error "No puede obtener la predicción de una pregunta"

respuesta :: Oraculo -> String -> Oraculo
respuesta oraculo opcion =
  case oraculo of
    Pregunta _ opciones -> opciones Data.Map.! opcion
    Prediccion _ -> error "No puede responder a una predicción"

-- obtenerCadena :: Oraculo -> String -> Maybe [(String, String)]
{-

-}


-- | Obtiene estadísticas de profundidad de un oráculo.
-- Calcula la profundidad mínima, máxima y promedio en el árbol de decisiones.
-- Parámetros de entrada:
--   - oraculo: El oráculo que representa el árbol de decisiones.
-- Retorna una tupla con tres valores: (profundidad mínima, profundidad máxima, promedio de profundidades).

obtenerEstadisticas :: Oraculo -> (Int, Int, Double)
obtenerEstadisticas oraculo = (minAltura, maxAltura, avgAltura)
  where alturas = map snd (profundidad oraculo 0)
        minAltura = minimum alturas
        maxAltura = maximum alturas
        avgAltura = promedio alturas

-- | Calcula el promedio de una lista de enteros.
-- Parámetro de entrada:
--   - lista: La lista de enteros de la cual se calculará el promedio.
-- Retorna el promedio de la lista como un número de punto flotante (Double).

promedio :: [Int] -> Double
promedio lista =
  let suma = fromIntegral (sum lista)
      cantidad = fromIntegral (length lista)
   in suma / cantidad

-- | Calcula las alturas de las Predicciones en un árbol.
-- Parámetros de entrada:
--   - oraculo: El oráculo que representa el árbol de decisiones.
--   - nivel: El nivel de profundidad actual.
-- Retorna una lista de tuplas donde cada tupla contiene una Predicción y su altura en el árbol.

profundidad :: Oraculo -> Int -> [(Oraculo, Int)]
profundidad (Prediccion pred) nivel = [(Prediccion pred, nivel)]
profundidad (Pregunta preg opc) nivel = concatMap (\oraculo -> profundidad oraculo (nivel + 1)) (elems opc)
