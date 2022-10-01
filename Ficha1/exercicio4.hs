module Exercicio4 where

data Hora = H Int Int deriving (Show, Eq)

validHour :: Hora -> Bool
validHour (H hour min) 
    | hour >= 0 && hour < 24 && min >= 0 && min < 60 = True
    | otherwise = False