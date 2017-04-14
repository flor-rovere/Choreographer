module ParserMain where

import Text.ParserCombinators.Parsec
import Data.Char

data R = PanelSize Float Float
       | Option Int 
       | FrameRate Float
       | File String

data Err = All_Right | Problem

data R_Param = Param
    { options    :: Int
    , frameRates :: Float
    , file       :: String
    , panelSize  :: (Float,Float)
    }

--recognize_parameters parsea los argumentos ingresados al ejecutar el main
recognize_parameters :: [String] -> [Maybe R]
recognize_parameters []     = []
recognize_parameters (x:xs) = case (parse recognizePar "" x) of
    Right elem -> (Just elem) : (recognize_parameters xs)
    Left error -> [Nothing]

recognizePar = try (do x <- float
                       (string "x" <|> string "X")
                       y <- float
                       return (PanelSize x y))
                 <|> (try (do string "opt"
                              x <- digit
                              return (Option (digitToInt x)))
                        <|> (try (do string "fr"
                                     x <- float
                                     return (FrameRate x))
                               <|> (try (do string "file"
                                            x <- many anyChar
                                            return (File x)))))

--recognize_mistakes verifica si hay errores en los argumentos ingresados
recognize_mistakes :: [Maybe R] -> Either Err Err
recognize_mistakes []     = Right All_Right
recognize_mistakes (x:xs) = case x of
    Nothing -> Left Problem
    _       -> recognize_mistakes xs

--eval genera actualiza un record con los parámetros dados
eval :: [Maybe R] -> R_Param -> R_Param
eval [] param            = param
eval ((Just x):xs) param = case x of
    Option i        -> let param' = param {options=i}
                        in eval xs param'
    FrameRate i     -> let param' = param {frameRates=i}
                        in eval xs param'
    File i          -> let param' = param {file=i}
                        in eval xs param'
    PanelSize i1 i2 -> let param' = param {panelSize=(i1,i2)}
                        in eval xs param'

getting_right_parameters :: [String] -> R_Param 
getting_right_parameters xs = let ys    = recognize_parameters xs
                                  zs    = recognize_mistakes ys
                                  param = Param{options=0,frameRates=1,file="",panelSize=(400,400)}
                               in case zs of
                                      Left Problem -> param
                                      _            -> eval ys param

--float se tomó prestado de otra persona para parsear un float
float = fmap rd $ (++) <$> (many1 digit) <*> decimal
    where rd      = read :: String -> Float
          decimal = option "" $ (:) <$> char '.' <*> (many1 digit)
