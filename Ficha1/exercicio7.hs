module Exercicio7 where

type Ponto = (Int, Int)
data Figura = Circulo Ponto Double 
            | Rectangulo Ponto Ponto 
            | Triangulo Ponto Ponto Ponto 
              deriving (Show, Eq)

-- Hlper functions
dist :: Ponto -> Ponto -> Double
dist p1 p2 =
    let x = fromIntegral (abs (fst p1 - fst p2)) :: Double
        y = fromIntegral (abs (snd p1 - snd p2)) :: Double
    in sqrt (x*x + y*y) 

poligono :: Figura -> Bool
poligono (Circulo p r) = False
poligono (Rectangulo p1 p2) = True
poligono (Triangulo p1 p2 p3) = True

vertices :: Figura -> [Ponto]
vertices (Circulo p r) = []
vertices (Rectangulo p1 p2) = [p1, p2, (fst p1, snd p2), (fst p2, snd p1)]
vertices (Triangulo p1 p2 p3) = [p1, p2, p3]

area :: Figura -> Double
area (Circulo p r) = 2 * pi * r*r
area (Rectangulo p1 p2) = 
    let x = fromIntegral (abs (fst p1 - fst p2)) :: Double
        y = fromIntegral (abs (snd p1 - snd p2)) :: Double
    in x * y
area (Triangulo p1 p2 p3) = 
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
        s = (a+b+c) / 2 -- semi-perímetro
    in sqrt (s * abs (s-a) * abs (s-b) * abs (s-c)) -- fórmula de Heron


perimetro :: Figura -> Double
perimetro (Circulo p r) = 2 * pi * r
perimetro (Rectangulo p1 p2) = 
    let x = fromIntegral (abs (fst p1 - fst p2)) :: Double
        y = fromIntegral (abs (snd p1 - snd p2)) :: Double
    in 2 * x + 2* y
perimetro (Triangulo p1 p2 p3) = 
    let a = dist p1 p2
        b = dist p2 p3
        c = dist p3 p1
    in a + b + c