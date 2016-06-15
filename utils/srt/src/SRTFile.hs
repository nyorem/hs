{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module SRTFile ( Time
               , parseTime
               , isTimeNegative
               , extractSRTFile
               , delaySRTFile
               , forwardSRTFile
               )
where

import Text.Parsec
import Text.Parsec.String ( Parser )

type Hours = Int
type Minutes = Int
type Seconds = Int
type Milliseconds = Int

-- | Time: HH:MM:SS,MMM
data Time = Time Hours Minutes Seconds Milliseconds

-- | Shows a number with a specific number of digits.
showWithDigits :: Int -> Int -> String
showWithDigits n digits
    | l == digits = s
    | otherwise = replicate (digits - l) '0' ++ s
        where s = show n
              l = length s

-- | Shows correctly a time: HH:MM:SS,MMM
instance Show Time where
    show (Time h m s ms) = concat [ showWithDigits h 2
                                  , ":"
                                  , showWithDigits m 2
                                  , ":"
                                  , showWithDigits s 2
                                  , ","
                                  , showWithDigits ms 3
                                  ]

-- | Tests if a time is negative.
isTimeNegative :: String -> Bool
isTimeNegative t = head t == '-'

-- | Parses a String to a Time.
parseTime :: String -> Time
parseTime str =
    case parse time "" str of
        Left err -> error $ show err
        Right t -> t

-- | Converts a Time to a number of milliseconds.
convertToMs :: Time -> Int
convertToMs (Time h m s ms) = ms + s * 1000 + m * 60 * 1000 + h * 3600 * 1000

-- | Converts a number of milliseconds to a Time.
convertFromMs :: Int -> Time
convertFromMs t = Time h m s ms
    where (h, dh) = helper t (3600 * 1000)
          (m, dm) = helper dh (60 * 1000)
          (s, ds) = helper dm 1000
          (ms, _) = helper ds 1
          helper a n = (b, d)
            where b = a `div` n
                  d = a `mod` n

normalizeTime :: Time -> Time
normalizeTime =
    convertFromMs . convertToMs

-- | Zips two times.
zipTimes :: (Int -> Int -> Int) -> Time -> Time -> Time
zipTimes f t1 t2 = convertFromMs (mt1 `f` mt2)
    where mt1 = convertToMs t1
          mt2 = convertToMs t2

-- | Adds two times.
addTimes :: Time -> Time -> Time
addTimes = zipTimes (+)

-- | Subtracts two times.
subTimes :: Time -> Time -> Time
subTimes = zipTimes subtract

-- format of an srt file:
-- index\n
-- begin --> end\n
-- text\n
--
-- ex:
-- 1
-- 00:00:01,020 --> 00:00:05,698
-- Sync and corrections by n17t01
-- www.addic7ed.com
--

-- | A 'line' of an srt file
data SRTLine = SRTLine Int Time Time String

-- | Shows correctly an srt line.
instance Show SRTLine where
    show (SRTLine ident beginTime endTime subtitle) = concat [ show ident
                                                             , "\n"
                                                             , show beginTime
                                                             , " --> "
                                                             , show endTime
                                                             , "\n"
                                                             , subtitle
                                                             , "\n"
                                                             ]

-- | An 'SRTfile' is a list of 'SRTLine's.
newtype SRTFile = SRTFile {unSRTFile :: [SRTLine]}

-- | Applies a function to all the lines in the srt file.
applyToSRTLine :: (SRTLine -> SRTLine) -> SRTFile -> SRTFile
applyToSRTLine f = SRTFile . map f . unSRTFile

-- | Shows correctly an srt file.
instance Show SRTFile where
    show = concatMap (\line -> show line ++ "\n") . unSRTFile

-- Parsing an srt file
colon :: Parser Char
colon = char ':'

comma :: Parser Char
comma = char ','

arrow :: Parser String
arrow = string "-->"

int :: Parser Int
int = many1 digit >>= return . read

-- | Parses a time.
time :: Parser Time
time = do
    hours <- int
    colon
    minutes <- int
    colon
    seconds <- int
    comma
    milliseconds <- int
    return (Time hours minutes seconds milliseconds)

-- | Parses an srt line.
parseSRTLine :: Parser SRTLine
parseSRTLine = do
    index <- int
    newline
    begin <- time
    spaces
    arrow
    spaces
    end <- time
    newline
    sub <- manyTill anyChar (try (string "\n\n"))
    return (SRTLine index begin end sub)

-- | Parses an srt file.
parseSRTFile :: Parser SRTFile
parseSRTFile = many parseSRTLine >>= return . SRTFile

extractSRTFile :: String -> IO SRTFile
extractSRTFile contents =
    case ret of
        Left err -> error $ show err
        Right srt -> return srt
    where ret = parse parseSRTFile "" contents

applySRTFile :: (Time -> Time) -> SRTFile -> SRTFile
applySRTFile f = applyToSRTLine f'
    where f' (SRTLine ident begin end sub) = SRTLine ident (f begin) (f end) sub

-- | Delays an srt file.
delaySRTFile :: Time -> SRTFile -> SRTFile
delaySRTFile delay =
    applySRTFile (`addTimes` delay)

-- | Forwards an srt file.
forwardSRTFile :: Time -> SRTFile -> SRTFile
forwardSRTFile delay =
    applySRTFile (delay `subTimes`)

