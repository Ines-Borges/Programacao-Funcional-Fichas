module Exercicio6 where

data Ponto = Cartesiano Double Double | Polar Double Double deriving(Show, Eq)

-- Sen a = cateto oposto / hipotenusa
-- Cos a = cateto adjacente / hipotenusa
-- Sen dá-me o y
-- Cos dá-me o x

posx :: Ponto -> Double
posx (Cartesiano x y) = x
posx (Polar r a) = cos a * r 

posy :: Ponto -> Double
posy (Cartesiano x y) = y
posy (Polar r a) = sin a * r

raio :: Ponto -> Double
raio (Cartesiano x y) = sqrt ((x*x) + (y*y))
raio (Polar r a) = r 

angulo :: Ponto -> Double
angulo (Cartesiano x y) = 
    let hipotenusa = raio (Cartesiano x y)
    in x / hipotenusa
angulo (Polar r a) = a

dist :: Ponto -> Ponto -> Double
dist (Cartesiano x1 y1) (Cartesiano x2 y2) = 
    let catetoX = x1 - x2
        catetoY = y1 - y2
    in raio (Cartesiano catetoX catetoY)
dist (Polar r1 a1) (Polar r2 a2) = 
    let x1 = posx (Polar r1 a1)
        x2 = posx (Polar r2 a2)
        y1 = posy (Polar r1 a1)
        y2 = posy (Polar r2 a2)
    in dist (Cartesiano x1 y1) (Cartesiano x2 y2)