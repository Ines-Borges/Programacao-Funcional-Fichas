module Exercicio1 where

import Text.Read
import Control.Monad (liftM)

-- Exercício a)
perimeter :: String -> Double
perimeter radius = 2 * pi * read radius

-- Exercício b)
distanceTwoPoints :: [Double] -> [Double] -> Double
distanceTwoPoints point1s point2s = 
    let diffx = head point1s - head point2s :: Double
        diffy = last point1s - last point2s :: Double
        distance = sqrt (diffx*diffx + diffy*diffy) :: Double
    in distance

-- Exercício c)
addNumberToList :: Int -> [Int] -> [Int]
addNumberToList num nums = nums ++ [num]

primUlt :: [Int] -> Either (IO()) [Int]
primUlt numbers = 
    do
        Left (putStr "Give me an integer(type s to stop): ")
        number <- getLine
        if number == "s" then return numbers
        else
            case readMaybe number of
                Just int -> primUlt (numbers ++ [int])
                Nothing -> do 
                    Left (putStrLn "Enter a valid number or s to stop")
                    primUlt numbers



main = do 
    putStrLn "Calculate the perimeter of a circumference"
    putStrLn "Enter a radius: "
    radius <- getLine
    putStrLn ("The perimeter of the circumference is: " ++ show (perimeter radius))

    putStrLn "------------------------------------------"

    putStrLn "Calculate the distance between two points in space"
    putStrLn "Enter x1: "
    x1 <- getLine
    putStrLn "Enter y1: "
    y1 <- getLine
    putStrLn "Enter x2: "
    x2 <- getLine
    putStrLn "Enter y2: "
    y2 <- getLine
    let ps1 = [read x1 :: Double, read y1 :: Double]
    let ps2 = [read x2 :: Double, read y2 :: Double]
    putStrLn ("The distance between these two points is: " ++ show (distanceTwoPoints ps1 ps2))

    putStrLn "------------------------------------------"

    putStrLn "Calculate the distance between two points in space"
    let numbers = primUlt []
    let first = head numbers
    let laste = last numbers
    putStrLn ("(" ++ show first ++ "," ++ show laste ++ ")")


