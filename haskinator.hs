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
consultar oraculo = do
    putStrLn "Prediccion 1: "
    t1 <- getLine
    putStrLn "Prediccion 2: "
    t2 <- getLine
    let p1 = crearOraculo t1
    let p2 = crearOraculo t2

    if (esAncestro p1 p2 oraculo && t1 != t1) then
        imprimirCrucial (getOpciones (ancestroComun p1 p2 oraculo) p1 p2) t1 t2

    else
        predNoExiste (predExiste p1 oraculo) (predExiste pp2 oraculo) t1 t2

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

