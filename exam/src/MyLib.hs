{-# LANGUAGE OverloadedStrings #-}

module MyLib (Pub(..), readData, nearest, nearestToId) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.List (find, sortOn)
import Data.Maybe (mapMaybe)
import Data.Vector (toList)
import Text.Read (readMaybe)

data Pub = Pub
  { id_  :: Int
  , name :: String
  , lon  :: Maybe Double
  , lat  :: Maybe Double
  }

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

instance Eq Pub where
  (Pub id1 _ _ _) == (Pub id2 _ _ _) = id1 == id2

instance Ord Pub where
  compare (Pub id1 _ _ _) (Pub id2 _ _ _) = compare id1 id2

-- Читаем данные c csv файла
readData :: FilePath -> IO [Pub]
readData filePath = do
  csvData <- BL.readFile filePath
  case decodeByName csvData of
    Left err -> do
      putStrLn err
      return []
    Right (_, pubs) -> return (toList pubs)

-- Функция подсчета Евклидова расстояния между 2-мя пабами
distance :: Pub -> Pub -> Maybe Double
distance (Pub _ _ (Just lon1) (Just lat1)) (Pub _ _ (Just lon2) (Just lat2)) =
  Just $ sqrt ((lon1 - lon2) ^ 2 + (lat1 - lat2) ^ 2)
distance _ _ = Nothing  -- Return Nothing if any of the coordinates is missing

-- Ищем ближайшие пабы к пабу с заданным id
nearestToId :: Int -> [Pub] -> Maybe (String, Maybe Double, Maybe Double, [(String, Maybe Double, Maybe Double, Maybe Double)])
nearestToId id pubs = do
  let pub = find (\p -> id_ p == id) pubs
  case pub of
    Nothing -> Nothing
    Just p -> do
      let nearby = mapMaybe (\otherPub -> 
                                if id_ otherPub /= id_ p 
                                  then (\d -> (name otherPub, Just d, lon otherPub, lat otherPub)) <$> distance p otherPub
                                  else Nothing) pubs
          filteredNearby = filter (\(_, dist, _, _) -> maybe False (< 0.01) dist) nearby
      return (name p, lon p, lat p, sortOn (\(_, dist, _, _) -> dist) filteredNearby)



-- Функция поиска ближайших пабов 
nearest :: [Pub] -> [(Pub, [Pub])]
nearest pubs = map (\p -> (p, getNearestPubs (id_ p))) pubs
  where
    getNearestPubs id = case nearestToId id pubs of
                          Just (_, _, _, nearby) -> mapMaybe (\(name, _, lon, lat) -> Just $ Pub id name lon lat) nearby
                          Nothing -> []


