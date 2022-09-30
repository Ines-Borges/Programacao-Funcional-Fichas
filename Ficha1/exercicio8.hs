module Exercicio8 where

import Data.Char

isLower :: Char -> Bool
isLower c = let charNum = ord c
            in if charNum >= 97 && charNum <= 122 then True
               else False 

isDigit :: Char -> Bool
isDigit c = if ord c >= 48 && ord c <= 57 then True
            else False

isAlpha :: Char -> Bool
isAlpha c = let charNum = ord c
            in
                if charNum >= 65 && charNum <= 90 || charNum >= 97 && charNum <= 122 then True
                else False

toUpper :: Char -> Char
toUpper c 
        | ord c >= 97 && ord c <= 122 = chr (ord c - 32)
        | ord c >= 65 && ord c <= 90 = chr (ord c)
        | otherwise = chr 0

intToDigit :: Int -> Char
intToDigit i
            | i == 1 = '1'
            | i == 2 = '2'
            | i == 3 = '3'
            | i == 4 = '4'
            | i == 5 = '5'
            | i == 6 = '6'
            | i == 7 = '7'
            | i == 8 = '8'
            | i == 9 = '9'
            | i == 0 = '0'
            | otherwise = chr 0

digitToInt :: Char -> Int
digitToInt i 
            | i == '1' = 1
            | i == '2' = 2
            | i == '3' = 3
            | i == '4' = 4
            | i == '5' = 5
            | i == '6' = 6
            | i == '7' = 7
            | i == '8' = 8
            | i == '9' = 9
            | i == '0' = 0