module Parser ( parseFile ) where

import Geometry

import Text.Parsec
import Text.Parsec.String (Parser)
import Graphics.Rendering.OpenGL ( GLfloat )

number :: Parser Integer
number = do
    sign <- string "-" <|> return ""
    d <- many1 digit
    return $ read (sign ++ d)

float :: Parser GLfloat
float = do
    a <- number
    _ <- char '.'
    b <- number
    return $ read $ show a ++ "." ++ show b

xy :: Parser Point
xy = do
    x <- float
    _ <- char ' '
    y <- float
    return (x, y)

xyFile :: Parser [Point]
xyFile = xy `endBy1` newline

parseFile :: FilePath -> IO [Point]
parseFile filename = do
    contents <- readFile filename
    case parse xyFile filename contents of
        Left err -> fail $ show err
        Right points -> return points

