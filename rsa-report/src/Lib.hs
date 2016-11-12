module Lib
    ( someFunc
    ) where

import Sec

toString :: Integer -> String
toString n = show n

someFunc :: IO ()
someFunc = do
  putStrLn "input number [\"p\", \"q\"] ->  "
  pq <- fmap (read :: String -> Integer) . words <$> getLine
  let n  = (pq !! 0)     * (pq !! 1)
      w  = (pq !! 0 - 1) * (pq !! 1 - 1)
  putStr "Please select e-number : "
  putStrLn . show $ getE w
  es <- fmap (read :: String -> Integer) . words <$> getLine
  let e  = es !! 0
      d  = getD e w
  if gcd e w /= 1 then do
                  putStrLn "ERR : This is not e-number"
                  return ()
                  else putStrLn $ "public_key is (" ++ (toString n) ++ ", " ++ (toString e) ++ "), Secret_key is " ++ (toString d)
