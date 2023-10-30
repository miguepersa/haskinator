import Data.List ( intercalate )
import Data.Char ( toLower )
import Oraculo
import System.Exit (exitSuccess)
import System.IO
import qualified Data.Map as Map
import Text.Printf (printf)

predecir :: Oraculo -> IO Oraculo
predecir (Pregunta p opt) = procesarPregunta (Pregunta p opt)
predecir (Prediccion p) = do
    if p == "" then do
        putStrLn "\nError: Oraculo vacio. Cree uno antes de predecir\n"
        return (Prediccion p)

    else do
        putStrLn $ "Prediccion: " ++ p
        putStrLn "Si/No"
        ans <- getLine
        case map toLower ans of
            "si" -> do
                putStrLn "Prediccion correcta.\n"
                return (Prediccion p)

            "no" -> do
                putStrLn "Prediccion incorrecta.\n"

                putStrLn "¿Cuál es la respuesta correcta?"
                ansCorr <- getLine

                putStrLn $ "¿Qué pregunta distingue a \"" ++ ansCorr ++ "\" de las otras opciones?"
                preg <- getLine

                putStrLn $ "¿Cuál es la respuesta a \"" ++ preg ++ "\" para \"" ++ ansCorr ++"\"?"
                opcion1 <- getLine

                putStrLn $ "¿Cuál es la respuesta a \"" ++ preg ++ "\" para \"" ++ p ++"\"?"
                opcion2 <- getLine

                return (ramificar [opcion1, opcion2] [crearOraculo ansCorr, (Prediccion p)] preg)
            _    -> do -- Respuesta no válida
                putStrLn "Error en respuesta. Intente nuevamente."
                predecir (Prediccion p)

procesarPregunta:: Oraculo -> IO Oraculo
procesarPregunta oraculo = do
    putStrLn $ "Pregunta: " ++ pregunta oraculo
    putStrLn $ intercalate " - " (Map.keys (opciones oraculo)) ++ " - ninguna"
    ans <- getLine

    case map toLower ans of
        "ninguna" -> do
            putStrLn "xd"
            return oraculo
        _ ->
            if ans `Map.member` (opciones oraculo) then do
                arb <- predecir (respuesta oraculo ans)
                return (addOp oraculo arb ans)

            else do
                putStrLn "\nRespuesta invalida\nIntente nuevamente\n"
                predecir oraculo

addOp :: Oraculo -> Oraculo -> String -> Oraculo
addOp (Pregunta t op) pred opp = Pregunta t (Map.insert opp pred op)


consultar :: Oraculo -> IO ()
consultar oraculo = do
    putStrLn "Prediccion 1: "
    t1 <- getLine
    putStrLn "Prediccion 2: "
    t2 <- getLine
    let p1 = crearOraculo t1
    let p2 = crearOraculo t2

    if esAncestro p1 p2 oraculo && t1 /= t1 then
        imprimirCrucial (getOpciones (ancestroComun p1 p2 oraculo) p1 p2) t1 t2
    else
        predNoExiste (predExiste p1 oraculo) (predExiste p2 oraculo) t1 t2

imprimirCrucial :: (Oraculo, String, String) -> String -> String -> IO ()
imprimirCrucial (preg, o1, o2) p1 p2 = do
    putStrLn $ "Pregunta Crucial: " ++ pregunta preg
    putStrLn $ "Opción para " ++ p1 ++ ": " ++ o1
    putStrLn $ "Opción para " ++ p2 ++ ": " ++ o2

getOpciones :: Oraculo -> Oraculo -> Oraculo -> (Oraculo, String, String)
getOpciones preg p1 p2 = (preg, foldl hallar_p1 "" hijos, foldl hallar_p2 "" hijos) where
    hallar_p1 acc (op, orac) = if predExiste p1 orac then op else acc
    hallar_p2 acc (op, orac) = if predExiste p2 orac then op else acc
    hijos = Map.toList (opciones preg)

ancestroComun :: Oraculo -> Oraculo -> Oraculo -> Oraculo
ancestroComun p1 p2 oraculo = ancestroComunAux p1 p2 oraculo

ancestroComunAux :: Oraculo -> Oraculo -> Oraculo -> Oraculo
ancestroComunAux p1 p2 raiz = foldl (\acc (_, orac) -> if esAncestro p1 p2 orac then ancestroComunAux p1 p2 orac else acc) raiz hijos where hijos = Map.toList (opciones raiz)

esAncestro :: Oraculo -> Oraculo -> Oraculo -> Bool
esAncestro p1 p2 oraculo = predExiste p1 oraculo && predExiste p2 oraculo

predNoExiste :: Bool -> Bool -> String -> String -> IO ()
predNoExiste True False p1 p2 = putStrLn $ "\"" ++ p2 ++ "\" NO es válida."
predNoExiste False True p1 p2 = putStrLn $ "\"" ++ p1 ++ "\" NO es válida."
predNoExiste False False p1 p2 = putStrLn $ "\"" ++ p1 ++ "\" y \"" ++ p2  ++ "\" NO son válidas."

predExiste :: Oraculo -> Oraculo -> Bool
predExiste pred (Prediccion txt) = txt == prediccion pred
predExiste pred (Pregunta _ ops) = foldl (\acc (_, orac) -> predExiste pred orac || acc) False lista where lista = Map.toList ops

persistir :: Oraculo -> IO ()
persistir oraculo = do
    putStrLn "Ingrese el nombre del archivo a escribir: "
    archivo <- getLine
    writeFile archivo (show oraculo)
    cicloMain oraculo

cargar :: IO Oraculo
cargar  = do
    putStrLn "Ingrese el nombre del archivo a leer: "
    archivo <- getLine
    read_oraculo <- readFile archivo
    putStrLn "\n--- Oraculo Cargado ---"
    return (read read_oraculo :: Oraculo)

-- consultar :: Oraculo -> IO ()

crearPrediccion :: IO ()
crearPrediccion = do
    putStrLn "Ingrese el texto de la predicción:"
    pred <- getLine
    cicloMain (crearOraculo pred)

estadisticas :: Oraculo -> IO ()
estadisticas (Prediccion "") = putStrLn "\nError: Oraculo vacio. Cree uno antes de predecir\n"
estadisticas oraculo = do
    let (minAltura, maxAltura, avgAltura) = obtenerEstadisticas oraculo
    putStrLn $ "Mínima profundidad: " ++ show minAltura
    putStrLn $ "Máxima profundidad: " ++ show maxAltura
    putStrLn $ "Promedio de profundidad: " ++ formatFloat avgAltura

formatFloat :: Double -> String
formatFloat = printf "%.4f"

cicloMain :: Oraculo -> IO()
cicloMain oraculo = do

    putStrLn "\nEscriba una de las opciones disponibles: \n"
    putStrLn "- Crear (-Crea un oraculo a partir de una prediccion-)"
    putStrLn "- Predecir (-Predice lmao-) "
    putStrLn "- Persistir (-Cargue un oraculo a un archivo-)"
    putStrLn "- Cargar (-Extraiga un oraculo de una archivo-)"
    putStrLn "- Consultar (-Consulta algo xddxxd-)"
    putStrLn "- Estadisticas (- Obtener estadisticas xddxxd-)"
    putStrLn "- Salir\n"

    opcion <- getLine
    case opcion of

        "Crear" -> do
            crearPrediccion

        "Predecir" -> do
            nuevoOraculo <- predecir oraculo
            cicloMain nuevoOraculo

        "Persistir" -> do
            persistir oraculo


        "Cargar" -> do
            oraculoCargado <- cargar
            cicloMain oraculoCargado

        "Consultar" -> do
            cicloMain oraculo


        "Estadisticas" -> do
            estadisticas oraculo
            cicloMain oraculo

        "Salir" -> do
            exitSuccess

        _ -> do
            putStrLn "Comando invalido. "
            cicloMain oraculo

-- Función principal
main :: IO ()
main = do
    -- Solicita al usuario que ingrese un comando
    putStrLn "Bienvenido a Haskinator!"
    cicloMain $ crearOraculo ""

