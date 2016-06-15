module Main where

import Data.List ( intercalate )
import Data.List.Split ( splitOn )
import qualified Data.Map as M
import Options.Applicative
import System.IO
import qualified System.IO.Strict as S

type Name = String

type Dict = M.Map Name [Name]

fromFile :: FilePath -> IO Dict
fromFile fname = do
    contents <- lines <$> S.readFile fname
    return $ M.fromList $ map fromLine contents

-- name: translation1, translation2...
fromLine :: String -> (Name, [Name])
fromLine line =
    (name, splitOn ", " defs)
        where (name, rest) = break (== ':') line
              defs         = drop 2 rest

path :: FilePath
path =
    "/home/nyorem/lang/en/VOC"

translations :: Dict -> Name -> Maybe [Name]
translations =
    flip M.lookup

addD :: Dict -> (Name, [Name]) -> Dict
addD dict (name, defs) =
    M.insertWith (++) name defs dict

removeD :: Dict -> Name -> Dict
removeD dict name =
    M.delete name dict

export :: Mode -> Dict -> String
export m =
    unlines . map (\(name, defs) -> name ++ ": " ++ intercalate ", " defs) . M.toList . f
        where f = if m == Normal then id else invert

invert :: (Ord k, Ord v) => M.Map k [v] -> M.Map v [k]
invert m = M.fromListWith (++) pairs
    where pairs = [ (v, [k]) | (k, vs) <- M.toList m, v <- vs ]

data Options =
    Options { mode      :: Mode
            , list      :: Bool
            , translate :: Maybe Name
            , add       :: Bool
            , remove    :: Maybe Name
            }

data Mode = Normal
          | Inverse
          deriving (Eq)

defReader :: String -> ReadM (Name, [Name])
defReader opt =
    case words opt of
      (name:defs) -> return (name, defs)
      _           -> readerError $ ""

options :: Parser Options
options =
    Options <$> flag Normal Inverse ( long "invert"
                                    <> short 'i'
                                    <> help "Invert dictionary" )
            <*> switch ( long "list"
                       <> short 'l'
                       <> help "List")
            <*> optional ( strOption ( long "translate"
                                      <> short 't'
                                      <> metavar "WORD"
                                      <> help "Word to translate" )
                         )
            <*> switch ( long "add"
                       <> short 'a'
                       <> help "Add a word" )
            <*> optional ( strOption ( long "remove"
                                      <> short 'r'
                                      <> metavar "WORD"
                                      <> help "Remove a word in the dictionary" )
                         )

opts :: ParserInfo Options
opts =
    info (helper <*> options)
         ( fullDesc <> header "dict - A simple command line dictionary" )

data Command = List Mode
             | Translate Mode Name
             | Add Mode Name [Name]
             | Remove Mode Name

handleOptions :: Options -> IO (Maybe Command)
handleOptions (Options m True _ _ _)         = return $ Just $ List m
handleOptions (Options m _ (Just name) _ _)  = return $ Just $ Translate m name
handleOptions (Options m _ _ True _)         = do
    putStr "Word to add? "
    hFlush stdout
    name <- getLine
    putStr "Translations? "
    hFlush stdout
    defs <- getLine
    return $ Just $ Add m name (splitOn ", " defs)
handleOptions (Options m _ _ _ (Just name))  = return $ Just $ Remove m name
handleOptions _                              = return $ Nothing

chooseD :: Dict -> Dict -> Mode -> Dict
chooseD dict _     Normal  = dict
chooseD _    idict Inverse = idict

executeCommand :: Dict -> Dict -> Command -> IO ()
executeCommand dict idict (List m) =
    putStr $ export Normal $ chooseD dict idict m
executeCommand dict idict (Translate m name) = do
    case translations (chooseD dict idict m) name of
      Nothing   -> putStrLn $ "No translations for " ++ name
      Just defs -> putStrLn $ name ++ ": " ++ intercalate ", " defs
executeCommand dict idict (Add m name defs) = do
    writeFile path $ export m $ addD (chooseD dict idict m) (name, defs)
executeCommand dict idict (Remove m name) =
    writeFile path $ export m $ removeD (chooseD dict idict m) name

main :: IO ()
main = do
    dict <- fromFile path
    iocmd <- handleOptions <$> execParser opts
    mcmd <- iocmd
    case mcmd of
      Nothing  -> putStrLn "No command"
      Just cmd -> executeCommand dict (invert dict) cmd

