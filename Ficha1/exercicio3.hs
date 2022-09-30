module Exercicio3 where 

type Hora = (Int, Int)

validHour :: Hora -> Bool
validHour h
    | fst h >= 0 && fst h < 24 && snd h >= 0 && snd h < 60 = True
    | otherwise = False

afterHour :: Hora -> Hora -> Bool
afterHour h1 h2 = let hour1 = fst h1
                      hour2 = fst h2
                      min1 = snd h1
                      min2 = snd h2
                  in  (hour1 /= hour2 || min1 /= min2)

convertHoursToMinutes :: Hora -> Int
convertHoursToMinutes h = let hour = fst h
                              min = snd h
                          in  hour * 60 + min

convertMinutesToHours :: Int -> Int
convertMinutesToHours m = div m 60

diffHours :: Hora -> Hora -> Int
diffHours h1 h2 = let hour1 = fst h1
                      hour2 = fst h2
                      min1 = snd h1
                      min2 = snd h2
                      diffH = abs (hour1 -hour2)
                      diffM = abs (min1 - min2)
                  in diffH * 60 - diffM

addMinutesToHours :: Int -> Hora -> Hora
addMinutesToHours m h = let hour = div m 60
                            min = mod m 60
                            h1 = fst h
                            m1 = snd h
                            minutes = m1 + min
                            minutesHour = div minutes 60
                            minutesMin = mod minutes 60
                        in (h1 + hour + minutesHour, minutesMin)