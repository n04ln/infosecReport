module Sec (p, q, n, w, createPrimeList, createPrimeList2, isPrime, getE, getD) where
p = 821 :: Integer
q = 467 :: Integer
n = p * q 
w = (p - 1) * (q - 1)

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

createPrimeList2 :: [Integer]
createPrimeList2 = [ x| x <- [2..], and [x `mod` y /= 0 | y <- [2..(floor $ sqrt $ realToFrac x)]]]

getE :: Integer -> [Integer]
getE w = [e | e <- [2,3..w], e>0, gcd w e == 1]

getD :: Integer -> Integer -> Integer
getD x y  = f x y 1 0 0 1
  where f r 0  x  _  y  _ = if x<0 then x+w else x
        f x y x0 x1 y0 y1 = f y r x1 (x0-q*x1) y1 (y0-q*y1)
          where (q, r) = divMod x y


