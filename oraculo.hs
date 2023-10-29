module Oraculo(
    crearOraculo,
    ramificar,
    prediccion,
    pregunta,
    opciones,
    respuesta,
    obtenerCadena,
    obtenerEstadisticas,
    Opciones,
    Oraculo(..)
) where

import Data.Map (Map, fromList) 


type Opciones = Map String Oraculo
data Oraculo = Prediccion String 
            | Pregunta String Opciones deriving(Show, Read)

crearOraculo :: String -> Oraculo
crearOraculo = Prediccion 

ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar opciones oraculos pregunta =
  Pregunta pregunta $ fromList $ zip opciones oraculos

prediccion :: Oraculo -> String
prediccion (Pregunta _ _) = error "No puede obtener la predicción de una pregunta"
prediccion (Prediccion pre) = pre

pregunta :: Oraculo -> String
opciones (Pregunta _ opciones) = opciones
opciones (Prediccion _) = Map.empty

opciones :: Oraculo -> Opciones
opciones (Pregunta _ opciones) = opciones
opciones (Prediccion _) = Map.empty

respuesta :: Oraculo -> String -> Oraculo
respuesta oraculo opcion =
  case oraculo of
    Pregunta _ opciones -> opciones Map.! opcion
    Prediccion _ -> error "No puede responder a una predicción"

obtenerCadena :: Oraculo -> String -> Maybe [(String, String)]
{-

-}

obtenerEstadisticas :: Oraculo -> (Int, Int, Double)
{-

-}
