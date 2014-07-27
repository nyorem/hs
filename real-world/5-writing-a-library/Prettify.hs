module Prettify
where

import SimpleJSON

data Doc = ToBeDefined
          deriving (Show)

-- | Appends two doc values.
(<>) :: Doc -> Doc -> Doc
a <> b = undefined

-- | Concatenates multiple doc values.
hcat :: [Doc] -> Doc
hcat ds = undefined

-- | Converts a character to doc.
char :: Char -> Doc
char c = undefined

-- | Escapes a character.
oneChar :: Char -> Doc
oneChar c = undefined

-- | Encloses a doc with two characters.
enclose :: Char -> Char -> Doc -> Doc
enclose left right x = char left <> x <> char right

string :: String -> Doc
string = enclose '"' '"' . hcat . map oneChar

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double d = undefined

