module Main where

import System.Environment
import System.IO.Error
import Control.Exception
import Graphics.Proc

import Body
import Parser
import DataType
import ParserMain

main :: IO()
main = do
    args <- getArgs
    let param =  ParserMain.getting_right_parameters args
     in case options param of
        1 -> if fst (panelSize param) < 200 || snd (panelSize param) < 200
              then putStrLn "Error: PanelSize muy pequeño"
              else if file param /= ""
                    then putStrLn "Error: No corresponde ingresar un archivo"
                    else do
                        xs <- Parser.lets_dance
                        case xs of
                            [[Just End]] -> putStrLn "Error: No hubo movimientos ingresados"
                            _  -> let ys  = Parser.recognize_mistakes_list xs
                                   in drawing ys xs (frameRates param) (panelSize param)
        2 -> if fst (panelSize param) < 200 || snd (panelSize param) < 200
              then putStrLn "Error: PanelSize muy pequeño"
              else do
                  ws <- catch (readFile (file param)) readHandler
                  if ws == "no"
                   then putStrLn "Error: No se encuentra el archivo ingresado"
                   else let ws' = lines ws
                         in case ws' of
                                [] -> putStrLn "Error: No hubo movimientos ingresados"
                                ["fin"] -> putStrLn "Error: No hubo movimientos ingresados"
                                _  -> let xs  = Parser.parserSt ws'
                                          ys  = Parser.recognize_mistakes_list xs
                                       in drawing ys xs (frameRates param) (panelSize param)
        _ -> putStrLn "Error: No se ingresó una forma de entrada para los comandos"
                         
--drawing verifica que los pasos ingresados sean correctos y comienza a dibujar
drawing :: Either Error Error -> [[Maybe States]] -> Float -> (Float, Float) -> IO ()
drawing (Left Not_Valid) _ _ _    = putStrLn "Error: Se ingresó un movimiento no válido"
drawing (Left Repeated) _ _ _     = putStrLn "Error: Se ingresaron dos acciones para la misma parte del cuerpo"
drawing (Right Ok) xs frame psize = let zs = Parser.eval_list xs
                                     in runProc $ def
                                            { procSetup = Body.setup zs frame psize
                                            , procDraw = Body.draw
                                            , procUpdate = Body.update }

--readHandler verifica que exista el archivo ingresado
readHandler :: IOError -> IO String  
readHandler file = if isDoesNotExistError file
                    then return "no"
                    else return "ok"
