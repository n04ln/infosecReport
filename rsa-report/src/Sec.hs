module Sec (createPrimeList, createPrimeList2, isPrime, euclidEx) where

createPrimeList :: Int -> Int -> [Int]
createPrimeList n x
  | n >= x     = []
  | otherwise  = if (judgePrimeNum x ( x - 1) ) then createPrimeList n (x - 1) ++ [x] else createPrimeList n (x - 1)
    where judgePrimeNum x 1 = True
          judgePrimeNum x y = if (x `mod` y) == 0 then False else judgePrimeNum x ( y - 1 )

isPrime :: Int -> Bool
isPrime n = judgePrimeNum n (n-1)
  where judgePrimeNum x 1 = True
        judgePrimeNum x y = if (x `mod` y) == 0 then False else judgePrimeNum x ( y - 1 )

createPrimeList2 :: [Int]
createPrimeList2 = [ x| x <- [2..], and [x `mod` y /= 0 | y <- [2..(floor $ sqrt $ realToFrac x)]]]

euclidEx :: Integer -> Integer -> Integer
euclidEx x y  = f x y 1 0 0 1
  -- where f r 0  x  _  y  _ = ((x, y), r)
  where f r 0  x  _  y  _ = x
        f x y x0 x1 y0 y1 = f y r x1 (x0-q*x1) y1 (y0-q*y1)
          where (q, r) = divMod x y


