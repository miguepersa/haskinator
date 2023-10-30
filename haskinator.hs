import Data.List ( intercalate )
import Data.Char ( toLower )
import Oraculo
import System.Exit (exitSuccess)
import System.IO  
import Data.Map (Map, fromList) 

predecir :: Oraculo -> IO Oraculo
predecir (Pregunta p op) = do
    putStrLn p
    putStrLn $ intercalate " - " (Map.keys op) ++ " - ninguna"
    ans <- getLine

    case map toLower ans of
        "ninguna" -> do 
            putStrLn "xd"

        _ ->
            if ans `Map.member` op (Pregunta p op) then do
                arb <- predecir (respuesta (Pregunta p op) ans)
                return (addOp (Pregunta p op) arb ans)

            else do
                putStrLn "\nRespuesta invalida\nIntente nuevamente\n"
                predecir (Pregunta p op)

predecir (Prediccion p) = do
    if p == "" then do
        putStrLn "\nError: Oraculo vacio. Cree uno antes de predecir\n"
        return (Prediccion p)
    
    else do
        putStrLn "Prediccion: " ++ p
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

                putStrLn $ "¿Cuál es la respuesta a \"" ++ preg ++ "\" para \"" ++ respCorrc ++"\"?"
                opcion1 <- getLine

                putStrLn $ "¿Cuál es la respuesta a \"" ++ pregDist ++ "\" para \"" ++ p ++"\"?"
                opcion2 <- getLine

                return (ramificar [opcCorrc, opcOrac] [crearOraculo respCorrc, oraculo] pregDist)
            _    -> do -- Respuesta no válida
                putStrLn "Error en respuesta. Intente nuevamente."
                predecirPred oraculo

addOp :: Oraculo -> Oraculo -> String -> Oraculo
addOp (Pregunta t op) pred opp = Pregunta t (Map.insert opp pred op)

persistir :: Oraculo -> IO ()

cargar :: IO Oraculo

consultar :: Oraculo -> IO ()

preguntarComando :: Maybe Oraculo -> IO()
preguntarComando comando oraculo = case map toLower comando of
    
    "crear" -> do
        putStrLn "Ingrese una prediccion: "
        prediccion <- getLine
        putStrLn "El nuevo oráculo ha sido creado.\n"
        crearOraculo prediccion
        preguntar_comando "Preguntar"

    "predecir" -> do
        
    "persistir" -> do
        putStrLn "Ingrese el nombre del archivo a escribir: "
        archivo <- getLine
        writeFile archivo (show oraculo)

    "cargar" -> do
        putStrLn "Ingrese el nombre del archivo a leer: "
        archivo <- getLine
        oraculo <- readFile archivo
        crearOraculo oraculo

    "consultar" -> do

    "estadisticas" -> do

    "salir" -> do
        exitSuccess

    "preguntar"-> do 
        putStrLn "Introduzca un comando: "
        comando <- getLine
        preguntarComando comando
    
    _ -> do
        putStrLn "Introduzca un comando: "
        new_comando <- getLine
        preguntarComando comando
    



-- Función principal
main' :: IO ()
main' = do
    -- Solicita al usuario que ingrese un comando
    putStrLn "Bienvenido a Haskinator!"    
    putStrLn "Introduzca un comando: "
    comando <- getLine
    preguntarComando comando

