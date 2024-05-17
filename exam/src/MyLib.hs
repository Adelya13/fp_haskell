{-# LANGUAGE OverloadedStrings #-}

module MyLib (Pub(..), readData, nearest) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List (sortOn)
import Data.Maybe (mapMaybe)
import Data.List (sortOn)
import Text.Read (readMaybe)
import Data.Vector (toList)

data Pub = Pub
  { id_  :: Int
  , name :: String
  , lon  :: Maybe Double
  , lat  :: Maybe Double
  } deriving (Show, Eq)

instance FromNamedRecord Pub where
  parseNamedRecord r = do
    id' <- r .: "id"
    name' <- r .: "name"
    lon' <- parseMaybeDouble $ r .: "lon"
    lat' <- parseMaybeDouble $ r .: "lat"
    return $ Pub id' name' lon' lat'

parseMaybeDouble :: Parser String -> Parser (Maybe Double)
parseMaybeDouble p = do
  str <- p
  return $ readMaybe str

-- Читаем данные c csv файла
readData :: FilePath -> IO [Pub]
readData filePath = do
  csvData <- BL.readFile filePath
  case decodeByName csvData of
    Left err -> do
      putStrLn err
      return []
    Right (_, pubs) -> return (mapMaybe parsePub (toList pubs))
  where
    parsePub pub = case runParser (parseNamedRecord pub) of
                     Left _ -> Nothing
                     Right p -> if lon p /= Nothing && lat p /= Nothing
                                then Just p
                                else Nothing

-- Функция подсчета Евклидова расстояния между 2-мя пабами
distance :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double -> Maybe Double
distance (Just lon1) (Just lat1) (Just lon2) (Just lat2) = Just $ sqrt ((lon1 - lon2) ^ 2 + (lat1 - lat2) ^ 2)
distance _ _ _ _ = Nothing

-- Функция поиска ближайших пабов
nearest :: [Pub] -> [(Pub, [Pub])]
nearest pubs = map (\pub -> (pub, findClosestPubs pub pubs)) pubs

-- Функция нахождения ближайших пабов к заданному пабу
findClosestPubs :: Pub -> [Pub] -> [Pub]
findClosestPubs p pubs =
  let threshold = 0.01
      nearbyPubs = filter (\x -> x /= p && distance (lon p) (lat p) (lon x) (lat x) < Just threshold) pubs
  in if null nearbyPubs
     then take 3 $ sortByDistance p (filter (/= p) pubs)
     else nearbyPubs

sortByDistance :: Pub -> [Pub] -> [Pub]
sortByDistance p = sortOn (\x -> distance (lon p) (lat p) (lon x) (lat x))
