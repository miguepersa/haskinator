import Data.List ( intercalate )
import Data.Char ( toLower )
import Oraculo
import System.Exit (exitSuccess)
import System.IO  
import Data.Map (Map, fromList) 

predecir :: Oraculo -> IO Oraculo


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

