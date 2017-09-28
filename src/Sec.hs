module Sec (primeListThan8d, createPrimeList, createPrimeList2, isPrime, getE, getD) where

createPrimeList :: Integer -> Integer -> [Integer]
createPrimeList n x
  | n >= x     = []
  | otherwise  = if (judgePrimeNum x ( x - 1) ) then createPrimeList n (x - 1) ++ [x] else createPrimeList n (x - 1)
    where judgePrimeNum x 1 = True
          judgePrimeNum x y = if (x `mod` y) == 0 then False else judgePrimeNum x ( y - 1 )

isPrime :: Integer -> Bool
isPrime n = judgePrimeNum n (n-1)
  where judgePrimeNum x 1 = True
        judgePrimeNum x y = if (x `mod` y) == 0 then False else judgePrimeNum x ( y - 1 )

iPrimeList2 :: [Integer] -> [Integer]
iPrimeList2 xs = [ x| x <- xs , and [x `mod` y /= 0 | y <- [2..(floor $ sqrt $ realToFrac x)]]]

createPrimeList2 :: [Integer]
createPrimeList2 = iPrimeList2 [2..]

primeListThan8d :: [Integer]
primeListThan8d = iPrimeList2 [(10 ^ 9)..]

getE :: Integer -> [Integer]
getE w = [e | e <- [2,3..w], e>0, gcd w e == 1]

getD :: Integer -> Integer -> Integer 
getD e w  = f e w 1 0 0 1
  where f r 0  e  _  w  _ = e
        f e w e0 e1 w0 w1 = f w r e1 (e0-q*e1) w1 (w0-q*w1)
          where (q, r) = divMod e w

