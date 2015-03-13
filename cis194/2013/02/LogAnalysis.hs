-- HOMEWORK 2

{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- ex 1
-- | Parse an element of a log message.
parseElemLog :: [String] -- ^ The log message
             -> Int      -- ^ The index of the element to parse
             -> Int      -- ^ The parsed element
parseElemLog w n = read (w !! n) :: Int

-- | Find the message in a log message.
findLogString :: Int      -- ^ The starting index
              -> [String] -- ^ The log message
              -> String   -- ^ The corresponding message
findLogString n = unwords . (drop n)

-- | Parse one log message.
parseMessage :: String     -- ^ The string to parse
             -> LogMessage -- ^ The parsed log message
parseMessage str =
    let w = words str in
        case head w of
            "I" -> LogMessage Info (parseElemLog w 1) (findLogString 2 w)
            "W" -> LogMessage Warning (parseElemLog w 1) (findLogString 2 w)
            "E" -> let n = parseElemLog w 1 in
                       LogMessage (Error n) (parseElemLog w 2) (findLogString 3 w)
            _ -> Unknown str

-- | Parse an entire file of log messages.
parse :: String       -- ^ The contents of the file
      -> [LogMessage] -- ^ The list of parsed log messages
parse = (map parseMessage) . lines

-- ex 2
-- | Get the timestamp of a LogMessage
getTimestamp :: LogMessage -- ^ A message
             -> TimeStamp  -- ^ The corresponding timestamp
getTimestamp (LogMessage _ t _) = t
getTimestamp (Unknown _) = undefined

-- | Insert a LogMessage into a sorted MessageTree according to timestamps.
-- If the LogMessage is Unknown then the MessageTree is unchanged.
insert :: LogMessage  -- ^ The message to insert
       -> MessageTree -- ^ The tree in which the message should be inserted
       -> MessageTree -- ^ The tree with the message inserted
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msg (Node leftTree msg' rightTree)
    | getTimestamp msg <= getTimestamp msg' = Node (insert msg leftTree) msg' rightTree
    | otherwise = Node leftTree msg' (insert msg rightTree)

-- ex 3
-- | Build the corresponding MessageTree to a list of LogMessage.
build :: [LogMessage] -- ^ The list of log messages
      -> MessageTree  -- ^ The associated tree
build = foldr insert Leaf

-- ex 4
-- | Transform a binary search tree MessageTree into a list of LogMessage.
inOrder :: MessageTree  -- ^ A message tree
        -> [LogMessage] -- ^ Sorted list of log messages
inOrder Leaf = []
inOrder (Node leftTree msg rightTree) = inOrder leftTree ++ [msg] ++ inOrder rightTree

-- ex 5
-- | Predicate that returns True iff the log message is an Error.
isError :: LogMessage -- ^ A log message
        -> Bool
isError (LogMessage (Error _) _ _) = True
isError _ = False

-- | Function that returns the error level of an error message.
errorLevel :: LogMessage -- ^ A log message
           -> Int        -- ^ The associated error level if error, undefined otherwise
errorLevel (LogMessage (Error level) _ _) = level
errorLevel _ = undefined

-- | Extract the message associated to an error message.
errorMessage :: LogMessage -- ^ A log message
             -> String     -- ^ The associated error message if error, undefined otherwise
errorMessage (LogMessage (Error _) _ msg) = msg
errorMessage _ = undefined

-- | Extract log messages which are errors and which error's level is greater then threshold.
extractRelevantMessages :: Int          -- ^ Threshold (error level)
                        -> [LogMessage] -- ^ Sorted list of log messages
                        -> [LogMessage] -- ^ Filtered list of log messages
extractRelevantMessages threshold = filter (\x -> (isError x) && (errorLevel x >= threshold))

-- | Extract log messages which are error messages and which error level is greater than 50.
whatWentWrong :: [LogMessage] -- ^ List of unsorted log messages
              -> [String]     -- ^ List of error messages which error level is greater than 50
whatWentWrong l =
    let orderedMessages = inOrder (build l)
        extractedMessages = extractRelevantMessages 50 orderedMessages in
        map errorMessage extractedMessages

