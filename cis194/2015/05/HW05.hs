{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.Function ( on )
import Data.List ( maximumBy, sortBy )
import Data.Bits ( xor )
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret orig modif = do
    originalFile <- BS.readFile orig
    modifiedFile <- BS.readFile modif
    return . BS.pack . filter (/= 0) $ BS.zipWith xor originalFile modifiedFile

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key dec = do
    let encodedFile = dec ++ ".enc"
    encodedContents <- BS.readFile encodedFile
    let repeatedKey = BS.cycle key
        decodedFile = BS.pack $ BS.zipWith xor encodedContents repeatedKey
    BS.writeFile dec decodedFile

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file =
    fmap decode $ BS.readFile file

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victims transactions = do
    victimsTs' <- parseFile victims
    case victimsTs' of
        Nothing -> return Nothing
        Just victimsTs -> do
            ts' <- parseFile transactions
            case ts' of
                Nothing -> return Nothing
                Just ts -> do
                    return $ Just $ filter (\t -> tid t `elem` victimsTs) ts

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow =
    foldr (\t m ->
        Map.insertWith (subtract) (from t) (amount t) $
        Map.insertWith (+) (to t) (amount t) m) Map.empty

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal =
    fst . maximumBy (compare `on` snd) . Map.toList

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m tids =
    let payers = Map.toList $ Map.filter (> 0) m
        payees = Map.toList $ Map.filter (<= 0) m
        sortedPayers = reverse $ sortBy (compare `on` snd) payers
        sortedPayees = sortBy (compare `on` snd) payees
    in
        undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON file contents =
    BS.writeFile file $ encode contents

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

