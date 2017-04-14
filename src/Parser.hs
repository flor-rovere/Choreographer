module Parser where

import Text.ParserCombinators.Parsec
import Data.Char
import Data.List.Split

import States
import DataType

--lets_dance toma los pasos por teclado
lets_dance :: IO [[Maybe States]]
lets_dance = do 
    putStrLn "Ingrese los pasos de baile:"
    xs <- steps []
    return (parserSt xs)

steps :: [String] -> IO [String]
steps xs = do 
    line <- getLine
    if line == "fin"
     then return (xs ++ [line])
     else steps (xs ++ [line])

parserSt :: [String] -> [[Maybe States]]
parserSt []     = []
parserSt (x:xs) = let y  = splits x
                      ys = parserSt xs
                   in (y:ys)

--splits divide un comando en el cual hubo varios pasos en una lista de (tal vez) pasos
splits :: String -> [Maybe States]
splits xs = let ys = splitOn "," xs
                zs = map (f ' ') ys
             in recognizeList zs
                where f _ [] = []
                      f symbol (x:xs) = if x == symbol
                                         then f symbol xs
                                         else (x:xs)

--recognizeList parsea los comandos ingresados como String a el tipo de datos definido
recognizeList :: [String] -> [Maybe States]
recognizeList []     = []
recognizeList (x:xs) = case (parse recognizeSt "" x) of
    Right elem -> (Just elem):(recognizeList xs)
    Left err   -> [Nothing]

recognizeSt :: Parser States
recognizeSt = try (do string "hacer"
                      space
                      x <- (many1 digit)
                      space
                      string "veces"
                      space
                      Step y z <- recognizeBP
                      return (StartMany (read x :: Int) y z))
                <|> (try recognizeBP)

recognizeBP :: Parser States
recognizeBP = try (do string "mano"
                      space
                      recognizeHand)
                <|> (try (do string "pie"
                             space
                             recognizeFoot)
                       <|> (try (do string "paso"
                                    space
                                    x <- recognizeMov
                                    return (Step StartPace x))
                              <|> (try (do string "cabeza"
                                           space
                                           x <- recognizeMov
                                           return (Step Head x))
                                     <|> (try (do string "fin"
                                                  return End)))))

recognizeHand :: Parser States
recognizeHand = do string "derecha"
                   space
                   x <- recognizeMov
                   return (Step RightArm x)
                 <|> do string "izquierda"
                        space
                        x <- recognizeMov
                        return (Step LeftArm x)

recognizeFoot :: Parser States
recognizeFoot = do string "derecho"
                   space
                   x <- recognizeMov
                   return (Step RightLeg x)
                 <|> do string "izquierdo"
                        space
                        x <- recognizeMov
                        return (Step LeftLeg x)

recognizeMov :: Parser Movement
recognizeMov = try (do string "arriba"
                       return Up)
                 <|> (try (do string "medio"
                              return BCenter)
                        <|> (try (do string "abajo"
                                     return Down)
                               <|> (try (do string "costado"
                                            return Side)
                                      <|> (try (do string "medio"
                                                   return BCenter)
                                             <|> (try (do der
                                                          return BRight)
                                                    <|> (try (do izq
                                                                 return BLeft)))))))

der = try (string "derecha")
        <|> (try (string "derecho"))

izq = try (string "izquierda")
        <|> (try (string "izquierdo"))

--recognize_mistakes_list se fija si hay errores en los pasos ingresados
recognize_mistakes_list :: [[Maybe States]] -> Either Error Error
recognize_mistakes_list []     = Right Ok
recognize_mistakes_list (x:xs) = let y = recognize_mistakes [] x
                                  in case y of
                                         Left err -> Left err
                                         _        -> recognize_mistakes_list xs

recognize_mistakes :: [BodyPart] -> [Maybe States] -> Either Error Error
recognize_mistakes movs []     = Right Ok
recognize_mistakes movs (x:xs) = case x of
    Nothing                -> Left Not_Valid
    Just End               -> Right Ok
    Just (Step v i)        -> if elem v movs
                               then Left Repeated
                               else recognize_mistakes (v:movs) xs
    Just (StartMany _ v i) -> if elem v movs
                               then Left Repeated
                               else recognize_mistakes (v:movs) xs
    Just (EndMany _ v i)   -> if elem v movs
                               then Left Repeated
                               else recognize_mistakes (v:movs) xs

--eval_list devuelve la lista con los pasos listos para graficar
eval_list :: [[Maybe States]] -> ListSt
eval_list = map (\x -> eval x)

eval :: [Maybe States] -> Step
eval = map (\(Just x) -> x)
