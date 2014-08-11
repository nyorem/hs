import Data.Maybe

-- | Tries to read a string.
-- Returns Nothing if there is an error during parsing.
maybeRead :: (Read a) => String -> Maybe a
maybeRead str =
    case reads str of
        [(x, _)] -> Just x
        _ -> Nothing
