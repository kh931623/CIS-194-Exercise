module Main where

import Lib
import Log

main :: IO ()
main = someFunc

parseErrorMessage :: String -> (Int, Int, String)
parseErrorMessage str =
  let (level : timestamp : xs) = words str
   in (read level :: Int, read timestamp :: Int, unwords xs)

parseOtherMessage :: String -> (Int, String)
parseOtherMessage str =
  let (timestamp : xs) = words str
   in (read timestamp :: Int, unwords xs)

parseMessage :: String -> LogMessage
parseMessage ('I' : rs) =
  let (timestamp, msg) = parseOtherMessage rs
   in LogMessage Info timestamp msg
parseMessage ('W' : rs) =
  let (timestamp, msg) = parseOtherMessage rs
   in LogMessage Warning timestamp msg
parseMessage ('E' : rs) =
  let (level, timestamp, msg) = parseErrorMessage rs
   in LogMessage (Error level) timestamp msg
parseMessage str = Unknown str

parse :: String -> [LogMessage]
parse = map parseMessage . lines

compareLog :: LogMessage -> LogMessage -> Bool
compareLog (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 <= t2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logMsg Leaf = Node Leaf logMsg Leaf
insert logMsg (Node left logState right)
  | isLessThanState = Node (insert logMsg left) logState right
  | otherwise = Node left logState (insert logMsg right)
  where
    isLessThanState = compareLog logMsg logState

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logState right) = inOrder left ++ (logState : inOrder right)

isErrorLog :: LogMessage -> Bool
isErrorLog (LogMessage (Error _) _ _) = True
isErrorLog _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong logs =
  let sortedLogs = (inOrder . build) logs
   in ( map (\(LogMessage _ _ msg) -> msg)
          . filter (\(LogMessage (Error level) _ _) -> level >= 50)
          . filter isErrorLog
      )
        sortedLogs