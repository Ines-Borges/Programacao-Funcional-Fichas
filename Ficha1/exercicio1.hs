module Exercicio1 where

perimetro :: Float -> Double
perimetro radius = 2 * pi * radius

main = do 
    putStrLn "Enter a radius: "
    radius <- getLine
    putStrLn "The perimeter of the circunference is: " ++ show (perimetro radius)
