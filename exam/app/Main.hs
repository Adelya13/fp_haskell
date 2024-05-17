module Main where

import MyLib
import Data.Maybe (fromMaybe)

main :: IO ()
main = do

  pubs <- readData "pubs.csv"

  let nearestPubs = nearest pubs

  mapM_ (\(pub, nearby) -> do
            putStrLn $ "Для паба " ++ show (id_ pub) ++ " " ++ show (name pub) ++ " (координаты: " ++ show (fromMaybe 0 (lon pub)) ++ ", " ++ show (fromMaybe 0 (lat pub)) ++ ") ближайшие:"
            mapM_ (\nearPub -> putStrLn $ "  id: " ++ show (id_ nearPub) ++ ", координаты: " ++ show (fromMaybe 0 (lon nearPub)) ++ ", " ++ show (fromMaybe 0 (lat nearPub))) nearby
        ) (take 3 nearestPubs)