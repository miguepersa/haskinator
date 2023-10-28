import (implicit) Prelude
    ( ($), Show(show), IO, words, getLine, putStrLn )
import Data.List ( intercalate )
import qualified Oraculo as O
import System.Exit (exitSuccess)

predecir :: Oraculo -> IO Oraculo

persistir :: Oraculo -> IO ()

cargar :: IO Oraculo

consultar :: Oraculo -> IO ()

preguntar_comando :: Maybe Oraculo -> IO()
preguntar_comando comando oraculo = case comando of
    
    'Crear' -> do
        putStrLn "Ingrese una prediccion: "
        prediccion <- getLine
        putStrLn "El nuevo oráculo ha sido creado.\n"
        crearOraculo prediccion
        preguntar_comando "Preguntar"

    'Predecir' -> do
        {-
        some
        -}

    'Persistir' -> do
        putStrLn "Ingrese el nombre del archivo a escribir: "
        archivo <- getLine
        writeFile archivo (show oraculo)

    'Cargar' -> do
        putStrLn "Ingrese el nombre del archivo a leer: "
        archivo <- getLine
        oraculo <- readFile archivo
        parseOraculo oraculo

    'Consultar' -> do

    'Estadisticas' -> do

    'Salir' -> do
        exitSuccess

    'Preguntar'-> do putStrLn "Introduzca un comando: "
            comando <- getLine
            preguntar_comando comando
    
    _ ->
        putStrLn "Comando Invalido" 
        putStrLn "Introduzca un comando: "
        comando <- getLine
        preguntar_comando comando
    



-- Función principal
main' :: IO ()
main' = do
    -- Solicita al usuario que ingrese un comando
    putStrLn "Bienvenido a Haskinator!"    
    putStrLn "Introduzca un comando: "
    comando <- getLine
    preguntar_comando comando

