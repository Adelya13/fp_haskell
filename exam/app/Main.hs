module Main where

import qualified MyLib (readData, nearestToId, nearest, Pub(..))
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  pubs <- MyLib.readData "pubs.csv"
  let nearestPubs = take 2 $ MyLib.nearest pubs
  putStrLn "Ближайшие пабы:"
  forM_ nearestPubs $ \(pub, nearby) -> do
    putStrLn $ "Для паба \"" ++ MyLib.name pub ++ "\":"
    putStrLn "  Ближайшие пабы:"
    forM_ nearby $ \nearbyPub -> do
      putStrLn $ "    - \"" ++ MyLib.name nearbyPub ++ "\""

  let id = 501 -- ID паба
  case MyLib.nearestToId id pubs of
    Nothing -> putStrLn "Не найдено публикации с указанным id"
    Just (pubName, lon, lat, nearbyPubs) -> do
      putStrLn $ "Ближайшие пабы к " ++ pubName ++ " (координаты: " ++ formatCoord lon ++ ", " ++ formatCoord lat ++ "):"
      forM_ nearbyPubs $ \(name, dist, lon', lat') ->
        putStrLn $ "  " ++ name ++ ": расстояние " ++ formatDist dist ++ ", координаты: (" ++ formatCoord lon' ++ ", " ++ formatCoord lat' ++ ")"
  where
    formatCoord :: Maybe Double -> String
    formatCoord = fromMaybe "Неизвестно" . fmap show

    formatDist :: Maybe Double -> String
    formatDist = fromMaybe "Неизвестно" . fmap show
