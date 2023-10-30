module Oraculo(
    crearOraculo,
    ramificar,
    prediccion,
    pregunta,
    opciones,
    respuesta,
   -- obtenerCadena,
   -- obtenerEstadisticas,
    Opciones,
    Oraculo(..)
) where

    import qualified Data.Map as Map
    import Data.Map (fromList )

    type Opciones = Map.Map String Oraculo
    data Oraculo = Prediccion String | Pregunta String Opciones deriving(Eq, Show, Read)

    crearOraculo :: String -> Oraculo
    crearOraculo = Prediccion 

    ramificar :: [String] -> [Oraculo] -> String -> Oraculo
    ramificar opciones oraculos pregunta = Pregunta pregunta $ fromList $ zip opciones oraculos

    prediccion :: Oraculo -> String
    prediccion (Pregunta _ _) = error "No puede obtener la predicción de una pregunta"
    prediccion (Prediccion p) = p

    pregunta :: Oraculo -> String
    pregunta (Pregunta txt _) = txt
    pregunta _ = error "El oráculo no es una pregunta"

    opciones :: Oraculo -> Opciones
    opciones (Pregunta _ opciones) = opciones
    opciones (Prediccion _) = error "No puede obtener la predicción de una pregunta"

    respuesta :: Oraculo -> String -> Oraculo
    respuesta oraculo opcion =
        case oraculo of
        Pregunta _ opciones -> opciones Map.! opcion
        Prediccion _ -> error "No puede responder a una predicción"

    --obtenerCadena :: Oraculo -> String -> Maybe [(String, String)]
    {-

    -}

    --obtenerEstadisticas :: Oraculo -> (Int, Int, Double)
    {-

    -}
