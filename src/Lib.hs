module Lib
    ( someFunc
    ) where

import Sec

toString :: Integer -> String
toString n = show n


someFunc :: IO ()
someFunc = do
    putStrLn "Input p, q number"
    pq <- fmap (read :: String -> Integer) . words <$> getLine
    let p = head pq
        q = (head . tail) pq
    if and [isPrime p, isPrime q]
       then do
           let n  = p * q
               p' = p - 1
               q' = q - 1
               w  = p' * q'
           putStrLn . show $ getE w
           es <- fmap (read :: String -> Integer) . words <$> getLine
           let e  = head es
               d  = getD e w
           if gcd e w /= 1
              then do
                  putStrLn "ERR : This is not e-number"
                  return ()
              else
                  putStrLn $ "Public_key is (" ++ (toString n) ++ ", " ++ (toString e) ++ "), Secret_key is " ++ (toString d)
           return ()
       else do
           putStrLn "ERR : p or q is not prime number"
           return ()

