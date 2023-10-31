import Data.List (intercalate)
import Data.Char (toLower)
import Oraculo
import System.Exit (exitSuccess)
import System.IO
import qualified Data.Map as Map
import Text.Printf (printf)

-- Predice usando un Oráculo.
-- Argumentos de entrada:
--   - oraculo: El oráculo a utilizar para hacer una predicción.
-- Valor de retorno:
--   - Retorna un nuevo Oráculo después de hacer una predicción.
predecir :: Oraculo -> IO Oraculo
predecir (Pregunta p opt) = procesarPregunta (Pregunta p opt)
predecir (Prediccion p) = do
    if p == "" then do
        putStrLn "\nError: Oráculo vacío. Cree uno antes de predecir\n"
        return (Prediccion p)
    else do
        putStrLn $ "Predicción: " ++ p
        putStrLn "Si/No"
        ans <- getLine
        case map toLower ans of
            "si" -> do
                putStrLn "Predicción correcta.\n"
                return (Prediccion p)
            "no" -> do
                putStrLn "Predicción incorrecta.\n"
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

-- Procesa una pregunta en el Oráculo.
-- Argumentos de entrada:
--   - oraculo: El Oráculo que contiene la pregunta.
-- Valor de retorno:
--   - Retorna un nuevo Oráculo después de procesar la pregunta.
procesarPregunta :: Oraculo -> IO Oraculo
procesarPregunta oraculo = do
    putStrLn $ "Pregunta: " ++ pregunta oraculo
    putStrLn $ intercalate " - " (Map.keys (opciones oraculo)) ++ " - ninguna"
    ans <- getLine
    case map toLower ans of
        "ninguna" -> do
            putStrLn "¿Cuál opción esperabas?"
            newOp <- getLine
            putStrLn "¿Cuál es la respuesta correcta?"
            newAns <- getLine
            return (addOp oraculo (crearOraculo newAns) newOp)

        _ ->
            if ans `Map.member` opciones oraculo then do
                arb <- predecir (respuesta oraculo ans)
                return (addOp oraculo arb ans)
            else do
                putStrLn "\nRespuesta inválida\nIntente nuevamente\n"
                predecir oraculo

-- Agrega una opción a un Oráculo.
-- Argumentos de entrada:
--   - oraculo: El Oráculo al que se le agregará la opción.
--   - pred: El Oráculo que se agregará como opción.
--   - opp: La opción que se agregará.
-- Valor de retorno:
--   - Retorna un nuevo Oráculo con la opción agregada.
addOp :: Oraculo -> Oraculo -> String -> Oraculo
addOp (Pregunta t op) pred opp = Pregunta t (Map.insert opp pred op)

-- Consulta la relación entre dos predicciones.
-- Argumentos de entrada:
--   - oraculo: El Oráculo con el que se realizará la consulta.
-- Valor de retorno:
--   - No tiene un valor de retorno, imprime la relación entre las predicciones.
consultar :: Oraculo -> IO ()
consultar oraculo = do
    putStrLn "Predicción 1: "
    t1 <- getLine
    putStrLn "Predicción 2: "
    t2 <- getLine
    let p1 = crearOraculo t1
    let p2 = crearOraculo t2
    if esAncestro p1 p2 oraculo && t1 /= t2 then
        imprimirCrucial (getOpciones (ancestroComun p1 p2 oraculo) p1 p2) t1 t2
    else
        predNoExiste (predExiste p1 oraculo) (predExiste p2 oraculo) t1 t2

-- Imprime la pregunta crucial y las opciones para dos predicciones.
-- Argumentos de entrada:
--   - (preg, o1, o2): Tupla que contiene el Oráculo de la pregunta crucial y las opciones.
--   - p1: Predicción 1.
--   - p2: Predicción 2.
-- Valor de retorno:
--   - No tiene un valor de retorno, imprime la pregunta crucial y las opciones.
imprimirCrucial :: (Oraculo, String, String) -> String -> String -> IO ()
imprimirCrucial (preg, o1, o2) p1 p2 = do
    putStrLn $ "Pregunta: \'" ++ pregunta preg ++ "\'"
    putStrLn $ "La opcion \'" ++ p1 ++ "\' lleva a \'" ++ o1 ++ "\'"
    putStrLn $ "La opcion \'" ++ p2 ++ "\' lleva a \'" ++ o2 ++ "\'"

-- Obtiene las opciones para las predicciones p1 y p2 en el Oráculo dado.
-- Argumentos de entrada:
--   - preg: El Oráculo en el que se buscarán las opciones.
--   - p1: Predicción 1.
--   - p2: Predicción 2.
-- Valor de retorno:
--   - Tupla que contiene el Oráculo, la opción para p1 y la opción para p2.
getOpciones :: Oraculo -> Oraculo -> Oraculo -> (Oraculo, String, String)
getOpciones preg p1 p2 = (preg, foldl hallar_p1 "" hijos, foldl hallar_p2 "" hijos) where
    hallar_p1 acc (op, orac) = if predExiste p1 orac then op else acc
    hallar_p2 acc (op, orac) = if predExiste p2 orac then op else acc
    hijos = Map.toList (opciones preg)

-- Encuentra el ancestro común más bajo entre dos predicciones en el Oráculo.
-- Argumentos de entrada:
--   - p1: Predicción 1.
--   - p2: Predicción 2.
--   - oraculo: El Oráculo en el que se buscará el ancestro común.
-- Valor de retorno:
--   - El Oráculo que representa el ancestro común más bajo.
ancestroComun :: Oraculo -> Oraculo -> Oraculo -> Oraculo
ancestroComun p1 p2 oraculo = ancestroComunAux p1 p2 oraculo

-- Función auxiliar para encontrar el ancestro común más bajo.
ancestroComunAux :: Oraculo -> Oraculo -> Oraculo -> Oraculo
ancestroComunAux p1 p2 raiz = foldl (\acc (_, orac) -> if esAncestro p1 p2 orac then ancestroComunAux p1 p2 orac else acc) raiz hijos where hijos = Map.toList (opciones raiz)

-- Verifica si p1 y p2 son ancestros en el Oráculo.
-- Argumentos de entrada:
--   - p1: Predicción 1.
--   - p2: Predicción 2.
--   - oraculo: El Oráculo en el que se realizará la verificación.
-- Valor de retorno:
--   - Booleano que indica si p1 y p2 son ancestros en el Oráculo.
esAncestro :: Oraculo -> Oraculo -> Oraculo -> Bool
esAncestro p1 p2 oraculo = predExiste p1 oraculo && predExiste p2 oraculo

-- Verifica si una predicción existe en el Oráculo.
-- Argumentos de entrada:
--   - pred: La predicción a verificar.
--   - oraculo: El Oráculo en el que se realizará la verificación.
-- Valor de retorno:
--   - Booleano que indica si la predicción existe en el Oráculo.

predNoExiste :: Bool -> Bool -> String -> String -> IO ()
predNoExiste True False p1 p2 = putStrLn $ "\"" ++ p2 ++ "\" NO es válida."
predNoExiste False True p1 p2 = putStrLn $ "\"" ++ p1 ++ "\" NO es válida."
predNoExiste False False p1 p2 = putStrLn $ "\"" ++ p1 ++ "\" y \"" ++ p2  ++ "\" NO son válidas."

predExiste :: Oraculo -> Oraculo -> Bool
predExiste pred (Prediccion txt) = txt == prediccion pred
predExiste pred (Pregunta _ ops) = foldl (\acc (_, orac) -> predExiste pred orac || acc) False lista where lista = Map.toList ops

-- Persiste un Oráculo en un archivo.
-- Argumentos de entrada:
--   - oraculo: El Oráculo a persistir.
-- Valor de retorno:
--   - No tiene valor de retorno, escribe el Oráculo en un archivo.
persistir :: Oraculo -> IO ()
persistir oraculo = do
    putStrLn "Ingrese el nombre del archivo a escribir: "
    archivo <- getLine
    writeFile archivo (show oraculo)
    cicloMain oraculo

-- Carga un Oráculo desde un archivo.
-- Valor de retorno:
--   - Retorna el Oráculo leído desde el archivo.
cargar :: IO Oraculo
cargar  = do
    putStrLn "Ingrese el nombre del archivo a leer: "
    archivo <- getLine
    read_oraculo <- readFile archivo
    putStrLn "\n--- Oráculo Cargado ---"
    return (read read_oraculo :: Oraculo)

-- Crea una predicción a partir del texto ingresado por el usuario.
-- No tiene argumentos de entrada.
-- Valor de retorno:
--   - No tiene valor de retorno, se utiliza para agregar una predicción al Oráculo.
crearPrediccion :: IO ()
crearPrediccion = do
    putStrLn "Ingrese el texto de la predicción:"
    pred <- getLine
    cicloMain (crearOraculo pred)

-- Obtiene estadísticas sobre la profundidad del Oráculo.
-- Argumentos de entrada:
--   - oraculo: El Oráculo del que se obtendrán las estadísticas.
-- Valor de retorno:
--   - No tiene valor de retorno, imprime las estadísticas.
estadisticas :: Oraculo -> IO ()
estadisticas (Prediccion "") = putStrLn "\nError: Oráculo vacío. Cree uno antes de predecir\n"
estadisticas oraculo = do
    let (minAltura, maxAltura, avgAltura) = obtenerEstadisticas oraculo
    putStrLn $ "\nmin: " ++ show minAltura ++ " max: " ++ show maxAltura ++ " avg: " ++ formatFloat avgAltura

-- Formatea un número de punto flotante con un número específico de decimales.
-- Argumentos de entrada:
--   - n: El número de punto flotante a formatear.
-- Valor de retorno:
--   - El número de punto flotante formateado como una cadena.
formatFloat :: Double -> String
formatFloat = printf "%.4f"

-- Función principal que maneja el flujo del programa.
-- Argumentos de entrada:
--   - oraculo: El Oráculo con el que se inicia el programa.
-- Valor de retorno:
--   - No tiene valor de retorno, controla el ciclo principal del programa.
cicloMain :: Oraculo -> IO()
cicloMain oraculo = do
    putStrLn "\nEscriba una de las opciones disponibles: \n"
    putStrLn "- Crear (-Crea un Oráculo a partir de una predicción-)"
    putStrLn "- Predecir (-Predice-)"
    putStrLn "- Persistir (-Guarda un Oráculo en un archivo-)"
    putStrLn "- Cargar (-Carga un Oráculo desde un archivo-)"
    putStrLn "- Consultar (-Consulta la relación entre predicciones-)"
    putStrLn "- Estadisticas (-Obtener estadísticas del Oráculo-)"
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
            consultar oraculo
            cicloMain oraculo
        "Estadisticas" -> do
            estadisticas oraculo
            cicloMain oraculo
        "Salir" -> do
            exitSuccess
        _ -> do
            putStrLn "Comando inválido. "
            cicloMain oraculo

-- Función principal del programa.
main :: IO ()
main = do
    -- Solicita al usuario que ingrese un comando
    putStrLn "Bienvenido a Haskinator!"
    cicloMain $ crearOraculo ""
