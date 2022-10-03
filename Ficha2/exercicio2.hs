module Exercicio2 where

dobros :: [Float] -> [Float]
dobros [] = []
dobros (n:ns) = (n*2):dobros(ns)