module Exercicio1 where

perimeter :: String -> Double
perimeter radius = 2 * pi * read radius

main = do 
    putStrLn "Enter a radius: "
    radius <- getLine
    putStrLn ("The perimeter of the circumference is: " ++ show (perimeter radius))
