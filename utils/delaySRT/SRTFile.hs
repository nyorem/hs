module SRTFile (
                 Time(..),
                 parseSRT,
                 addTimeSRTLine,
                 subTimeSRTLine,
                 constructTimeFromStr,
                 applyToSRTLine,
               )
where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Numbers ( parseIntegral )

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

-- Time: HH:MM:SS,MMM
type Hours = Int
type Minutes = Int
type Seconds = Int
type Milliseconds = Int
data Time = Time Hours Minutes Seconds Milliseconds

-- | Shows a number with a specific number of digits.
showWithDigits :: Int -> Int -> String
showWithDigits n digits
    | l == digits = s
    | otherwise = replicate (digits - l) '0' ++ s
        where s = show n
              l = length s

-- Shows correctly a time.
instance Show Time where
    show (Time h m s ms) = concat [showWithDigits h 2,
                                   ":",
                                   showWithDigits m 2,
                                   ":",
                                   showWithDigits s 2,
                                   ",",
                                   showWithDigits ms 3]

-- | Buils a Time structure from a string.
constructTimeFromStr :: String -> Time
constructTimeFromStr str =
    case ret of
        Left err -> error $ show err
        Right t -> t
    where ret = parse parseTime "" str

-- | Convert a Time to a number of milliseconds.
convertToMs :: Time -> Int
convertToMs (Time h m s ms) = ms + s * 1000 + m * 60 * 1000 + h * 3600 * 1000

-- | Convert a number of milliseconds to a Time.
convertFromMs :: Int -> Time
convertFromMs t = Time h m s ms
    where (h, dh) = helper t (3600 * 1000)
          (m, dm) = helper dh (60 * 1000)
          (s, ds) = helper dm 1000
          (ms, _) = helper ds 1
          helper a n = (b, d)
            where b = a `div` n
                  d = a - s * n

-- | Adds two times.
addTimes :: Time -> Time -> Time
addTimes t1 t2 = convertFromMs (mt1 + mt2)
    where mt1 = convertToMs t1
          mt2 = convertToMs t2

-- | Subtracts two times.
subTimes :: Time -> Time -> Time
subTimes t1 t2 = convertFromMs (mt1 - mt2)
    where mt1 = convertToMs t1
          mt2 = convertToMs t2

-- A 'line' of an srt file
type Index = Int
type Subtitle = String
data SRTLine = SRTLine Index Time Time Subtitle

-- Shows correctly an srt line.
instance Show SRTLine where
    show (SRTLine ident beginTime endTime subtitle) = concat [show ident,
                                                              "\n",
                                                              show beginTime,
                                                              " --> ",
                                                              show endTime,
                                                              "\n",
                                                              subtitle,
                                                              "\n"]

-- | Adds a time to a srt line.
addTimeSRTLine :: Time -> SRTLine -> SRTLine
addTimeSRTLine t (SRTLine ident begin end sub) = SRTLine ident nbegin nend sub
    where nbegin = addTimes begin t
          nend   = addTimes end t

-- | Subtracts a time to a srt line.
subTimeSRTLine :: Time -> SRTLine -> SRTLine
subTimeSRTLine t (SRTLine ident begin end sub) = SRTLine ident nbegin nend sub
    where nbegin = subTimes begin t
          nend   = subTimes end t

-- A srt file
newtype SRTFile = SRTFile {toLines :: [SRTLine]}

applyToSRTLine :: (SRTLine -> SRTLine) -> SRTFile -> SRTFile
applyToSRTLine f file = SRTFile $ map f (toLines file)

-- Shows correctly an srt file.
instance Show SRTFile where
    show = concatMap (\line -> show line ++ "\n") . toLines

-- Parsing an srt file
colon :: Parser Char
colon = char ':'

comma :: Parser Char
comma = char ','

arrow :: Parser String
arrow = string "-->"

-- | Parses a time.
parseTime :: Parser Time
parseTime = do
    hours <- parseIntegral
    _ <- colon
    minutes <- parseIntegral
    _ <- colon
    seconds <- parseIntegral
    _ <- comma
    milliseconds <- parseIntegral
    return (Time hours minutes seconds milliseconds)

-- | Parses an srt line.
parseSRTLine :: Parser SRTLine
parseSRTLine = do
    index <- parseIntegral
    _ <- newline
    begin <- parseTime
    _ <- spaces
    _ <- arrow
    _ <- spaces
    end <- parseTime
    _ <- newline
    sub <- manyTill anyChar (try (string "\n\n"))
    return (SRTLine index begin end sub)

-- | Parses an srt file.
parseSRTFile :: Parser SRTFile
parseSRTFile = many parseSRTLine >>= return . SRTFile

extractSRTFile :: String -> IO SRTFile
extractSRTFile file =
    case ret of
        Left err -> error $ show err
        Right srt -> return srt
    where ret = parse parseSRTFile "" file

-- | Reads and parses an srt file.
parseSRT :: FilePath -> IO SRTFile
parseSRT file = readFile file >>= extractSRTFile >>= return

