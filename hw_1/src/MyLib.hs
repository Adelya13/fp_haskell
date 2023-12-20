module MyLib (someFunc, zipLong) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

zipLong :: [a] -> [b] -> [(a, b)]
zipLong [] _ = []
zipLong _ [] = []
zipLong as bs
  | length as < length bs = zipWith (\a b -> (a, b)) (cycle as) bs
  | otherwise = zipWith (\a b -> (a, b)) as (cycle bs)