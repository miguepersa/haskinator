module Oraculo(
    crearOraculo,
    ramificar,
    prediccion,
    pregunta,
    opciones,
    respuesta,
    parseOraculo,
    Opciones,
    Oraculo(..)
) where

type Opciones = Map String Oraculo
data Oraculo = Prediccion String 
            | Pregunta String Opciones deriving(Show)

crearOraculo :: String -> Oraculo
crearOraculo a = Prediccion a

ramificar :: [String] -> [Oraculo] -> String -> Oraculo
ramificar opciones oraculos pregunta =
{-
some
-}

prediccion :: Oraculo -> String
prediccion (Prediccion a _) =  a

pregunta :: Oraculo -> String
pregunta (Pregunta a _) = a

opciones :: Oraculo -> Opciones
opciones (Pregunta _ b) = b

respuesta :: Oraculo -> String -> Oraculo
respuesta oraculo opción = do
{-
some
-}

parseOraculo:: Oraculo -> Maybe String
{-
some
-}